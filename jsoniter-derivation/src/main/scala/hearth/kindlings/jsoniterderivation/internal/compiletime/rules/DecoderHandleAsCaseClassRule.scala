package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.JsoniterConfig
import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  private def deriveZeroValue[A: Type]: Expr[A] =
    if (Type[A] <:< Type.of[AnyRef]) Expr.quote(null.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Boolean]) Expr.quote(false.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Byte]) Expr.quote(0.toByte.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Short]) Expr.quote(0.toShort.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Int]) Expr.quote(0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Long]) Expr.quote(0L.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Float]) Expr.quote(0.0f.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Double]) Expr.quote(0.0.asInstanceOf[A])
    else if (Type[A] =:= Type.of[Char]) Expr.quote(' '.asInstanceOf[A])
    else Expr.quote(null.asInstanceOf[A])

  @scala.annotation.nowarn("msg=is never used")
  private def inlineBuiltInDecode[Field: Type](readerExpr: Expr[JsonReader])(implicit
      JsonReaderT: Type[JsonReader]
  ): Option[Expr[Field]] =
    if (Type[Field] =:= Type.of[Int]) Some(Expr.quote(Expr.splice(readerExpr).readInt().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Long]) Some(Expr.quote(Expr.splice(readerExpr).readLong().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Double]) Some(Expr.quote(Expr.splice(readerExpr).readDouble().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Float]) Some(Expr.quote(Expr.splice(readerExpr).readFloat().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Boolean])
      Some(Expr.quote(Expr.splice(readerExpr).readBoolean().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[String])
      Some(Expr.quote(Expr.splice(readerExpr).readString(null).asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Byte]) Some(Expr.quote(Expr.splice(readerExpr).readByte().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Short]) Some(Expr.quote(Expr.splice(readerExpr).readShort().asInstanceOf[Field]))
    else if (Type[Field] =:= Type.of[Char]) Some(Expr.quote(Expr.splice(readerExpr).readChar().asInstanceOf[Field]))
    else None

  /** Fold a list of Expr[Unit] into a single sequenced Expr[Unit]. */
  @scala.annotation.nowarn("msg=is never used")
  private def foldExprUnits(exprs: List[Expr[Unit]]): Expr[Unit] = {
    implicit val ut: Type[Unit] = CTypes.Unit
    exprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
      Expr.quote {
        Expr.splice(acc)
        Expr.splice(step)
      }
    }
  }

  /** Derive decoder function for a single case class field, handling @stringified annotation. */
  @scala.annotation.nowarn("msg=is never used")
  private def deriveFieldDecoderFn[Field: Type](
      fName: String,
      typeName: String,
      hasStringifiedAnnotation: Boolean
  )(implicit ctx: DecoderCtx[?], JsonReaderT: Type[JsonReader]): MIO[Expr[JsonReader => Field]] =
    Log.namedScope(s"fieldDecoderFn:$fName") {
      if (hasStringifiedAnnotation) {
        deriveStringifiedDecoder[Field] match {
          case Some(dec) => MIO.pure(dec)
          case None      =>
            val err = CodecDerivationError
              .StringifiedOnNonNumeric(fName, typeName, Type[Field].prettyPrint)
            Log.error(err.message) >> MIO.fail(err)
        }
      } else {
        deriveStringifiedDecoder[Field] match {
          case Some(stringifiedDec) =>
            deriveFieldDecoder[Field].map { normalDec =>
              Expr.quote { (r: JsonReader) =>
                if (Expr.splice(ctx.config).isStringified)
                  Expr.splice(stringifiedDec).apply(r)
                else
                  Expr.splice(normalDec).apply(r)
              }
            }
          case None =>
            deriveFieldDecoder[Field]
        }
      }
    }

  /** Build transient-init expressions: fill null slots for absent fields with defaults/None/"". */
  @scala.annotation.nowarn("msg=is never used")
  private def buildTransientInitExprs(
      nonTransientWithIndex: List[((String, Parameter), Int)],
      decodedValuesExpr: Expr[Array[Any]],
      config: Expr[hearth.kindlings.jsoniterderivation.JsoniterConfig]
  )(implicit AnyT: Type[Any]): List[Expr[Unit]] =
    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
      import param.tpe.Underlying as Field
      val initSteps = List.newBuilder[Expr[Unit]]

      if (param.hasDefault) {
        param.defaultValue
          .flatMap { existentialOuter =>
            val methodOf = existentialOuter.value
            methodOf.value match {
              case noInstance: Method.NoInstance[?] =>
                import noInstance.Returned
                noInstance(Map.empty).toOption.map(_.upcast[Any])
              case _ => None
            }
          }
          .foreach { defaultExpr =>
            initSteps += Expr.quote {
              if (
                Expr.splice(config).transientDefault &&
                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
              )
                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(defaultExpr)
            }
          }
      }

      val isOpt = Type[Field] match {
        case IsOption(_) => true; case _ => false
      }
      if (isOpt) {
        initSteps += Expr.quote {
          if (
            Expr.splice(config).transientNone &&
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
          )
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = (None: Any)
        }
      }

      if (Type[Field] =:= CTypes.String) {
        initSteps += Expr.quote {
          if (
            Expr.splice(config).transientEmpty &&
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
          )
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = ("": Any)
        }
      }

      initSteps.result()
    }

  /** Build require-check expressions: validate that required fields are present in the decoded array. */
  @scala.annotation.nowarn("msg=is never used")
  private def buildRequireCheckExprs(
      nonTransientWithIndex: List[((String, Parameter), Int)],
      decodedValuesExpr: Expr[Array[Any]],
      config: Expr[hearth.kindlings.jsoniterderivation.JsoniterConfig]
  ): List[Expr[Unit]] =
    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
      import param.tpe.Underlying as Field
      val checks = List.newBuilder[Expr[Unit]]

      if (param.hasDefault) {
        checks += Expr.quote {
          if (
            Expr.splice(config).requireDefaultFields &&
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
          )
            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
        }
      }

      val isCollOrMap = Type[Field] match {
        case IsMap(_)        => true
        case IsCollection(_) => true
        case _               => false
      }
      if (isCollOrMap) {
        checks += Expr.quote {
          if (
            Expr.splice(config).requireCollectionFields &&
            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
          )
            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
        }
      }

      checks.result()
    }

  /** Build map of transient field default values for the constructor. */
  private def buildTransientDefaults(
      fieldsList: List[(String, Parameter)]
  )(implicit transientFieldT: Type[transientField]): Map[String, Expr_??] =
    fieldsList
      .filter { case (_, p) => hasAnnotationType[transientField](p) }
      .flatMap { case (fName, param) =>
        param.defaultValue.flatMap { existentialOuter =>
          val methodOf = existentialOuter.value
          methodOf.value match {
            case noInstance: Method.NoInstance[?] =>
              import noInstance.Returned
              noInstance(Map.empty).toOption.map(expr => (fName, expr.as_??))
            case _ => None
          }
        }
      }
      .toMap

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            val useOptimized = dctx.evaluatedConfig.isDefined
            val decodeMIO =
              if (useOptimized) decodeCaseClassFieldsOptimized[A](caseClass)
              else decodeCaseClassFields[A](caseClass)
            decodeMIO.map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Separate transient from non-transient fields
      val nonTransientFields = fieldsList.filterNot { case (_, p) => hasAnnotationType[transientField](p) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (either zero-param or all-transient): construct directly
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = CodecDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some("Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                // Still need to read the empty object from the reader
                MIO.pure(Expr.quote {
                  JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
                  Expr.splice(expr)
                })
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Build transient field default values for the constructor
          val transientDefaults: Map[String, Expr_??] = buildTransientDefaults(fieldsList)

          // Re-index non-transient fields for the array (0, 1, 2, ...)
          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          // Step 1: For each non-transient field, derive a decoder and build dispatch/accessor.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation).map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(arrayIndex))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              // Step 2: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val requireCheckAll: Expr[Unit] =
                    foldExprUnits(buildRequireCheckExprs(nonTransientWithIndex, decodedValuesExpr, dctx.config))

                  val transientInitAll: Expr[Unit] =
                    foldExprUnits(buildTransientInitExprs(nonTransientWithIndex, decodedValuesExpr, dctx.config))

                  // Non-transient fields read from the array
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  // Merge with transient defaults
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) =>
                      MIO.pure(Expr.quote {
                        Expr.splice(requireCheckAll)
                        Expr.splice(transientInitAll)
                        Expr.splice(constructExpr)
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Step 3: Build the field dispatch - if-else chain matching mapped field names.
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    JsoniterDerivationUtils.readObject[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        fieldMappings.foldRight(Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else reader.decodeError("unexpected field: " + fieldName)
                        }: Expr[Unit]) {
                          case ((name, index, decodeFnErased, Some(customName)), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(Expr(customName))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                        }
                      }
                    }
                  }
                }
            }
      }
    }

    /** Optimized case class decoding: readKeyAsCharBuf + isCharBufEqualsTo + no runtime config checks. Used when config
      * is statically evaluable via semiEval.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFieldsOptimized[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val config = dctx.evaluatedConfig.get
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => hasAnnotationType[transientField](p) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = CodecDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some("Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  JsoniterDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
                  Expr.splice(expr)
                })
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          val transientDefaults: Map[String, Expr_??] = buildTransientDefaults(fieldsList)

          implicit val BooleanT: Type[Boolean] = CTypes.Boolean
          implicit val UnitT: Type[Unit] = CTypes.Unit

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              val mappedName = nameOverride.getOrElse(config.fieldNameMapper(fName))
              Log.namedScope(s"Deriving optimized decoder for field $fName: ${Type[Field].prettyPrint}") {
                val isStringifiedStaticallyFalse = !config.isStringified

                val inlinedDecode: Option[Expr[JsonReader] => Expr[Field]] =
                  if (isStringifiedStaticallyFalse && !hasStringifiedAnnotation)
                    inlineBuiltInDecode[Field](Expr.quote(null.asInstanceOf[JsonReader])).flatMap { _ =>
                      summonJsonValueCodecCached[Field] match {
                        case Right(_) => None
                        case Left(_)  =>
                          Some((readerExpr: Expr[JsonReader]) => inlineBuiltInDecode[Field](readerExpr).get)
                      }
                    }
                  else None

                val deriveMIO: MIO[Expr[JsonReader] => Expr[Field]] = inlinedDecode match {
                  case Some(inlined) => MIO.pure(inlined)
                  case None          =>
                    val decoderMIO: MIO[Expr[JsonReader => Field]] =
                      if (hasStringifiedAnnotation)
                        deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation = true)
                      else if (isStringifiedStaticallyFalse)
                        deriveFieldDecoder[Field]
                      else
                        deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation = false)
                    decoderMIO.map(fn => (r: Expr[JsonReader]) => Expr.quote(Expr.splice(fn).apply(Expr.splice(r))))
                }

                deriveMIO.map { decodeField =>
                  val defaultExpr: Expr[Field] = deriveZeroValue[Field]
                  val fieldVar = ValDefs.createVar[Field](defaultExpr, s"_$fName")
                  fieldVar.map { case (getter, setter) =>
                    val setFromReader: Expr[JsonReader] => Expr[Unit] = { readerExpr =>
                      setter(decodeField(readerExpr))
                    }
                    (fName, mappedName, getter.as_??, setFromReader)
                  }
                }
              }
            }
            .flatMap { fieldVarDefs =>
              val emptyVarDefs: ValDefs[List[(String, String, Expr_??, Expr[JsonReader] => Expr[Unit])]] =
                ValDefsTraverse.pure(List.empty)

              val allFieldVars = fieldVarDefs.toList.foldLeft(emptyVarDefs) { (acc, varDef) =>
                acc.map2(varDef) { case (list, entry) => list :+ entry }
              }

              val lenVar = ValDefs.createVar[Int](Expr(-1), "_l")
              val combined = allFieldVars.map2(lenVar) { case (fieldInfos, (lenGet, lenSet)) =>
                (fieldInfos, lenGet, lenSet)
              }

              MIO.pure(combined.use { case (fieldInfos, lenGetter, lenSetter) =>
                val fieldMap: Map[String, Expr_??] =
                  fieldInfos.map { case (name, _, getter, _) => (name, getter) }.toMap ++ transientDefaults
                val constructExpr: Expr[A] = caseClass.primaryConstructor(fieldMap) match {
                  case Right(expr) => expr
                  case Left(error) => Environment.reportErrorAndAbort(error)
                }

                val readerExpr = dctx.reader

                val dispatch: Expr[Unit] = fieldInfos.foldRight(
                  Expr.quote(Expr.splice(readerExpr).skip()): Expr[Unit]
                ) { case ((_, mappedName, _, setFromReader), elseExpr) =>
                  Expr.quote {
                    if (
                      Expr.splice(readerExpr).isCharBufEqualsTo(Expr.splice(lenGetter), Expr.splice(Expr(mappedName)))
                    )
                      Expr.splice(setFromReader(readerExpr))
                    else Expr.splice(elseExpr)
                  }
                }

                Expr.quote {
                  if (!Expr.splice(readerExpr).isNextToken('{'.toByte))
                    Expr.splice(readerExpr).decodeError("expected '{'")
                  if (!Expr.splice(readerExpr).isNextToken('}'.toByte)) {
                    Expr.splice(readerExpr).rollbackToken()
                    while (Expr.splice(lenGetter) < 0 || Expr.splice(readerExpr).isNextToken(','.toByte)) {
                      Expr.splice(lenSetter(Expr.quote(Expr.splice(readerExpr).readKeyAsCharBuf())))
                      Expr.splice(dispatch)
                    }
                    if (!Expr.splice(readerExpr).isCurrentToken('}'.toByte))
                      Expr.splice(readerExpr).objectEndOrCommaError()
                  }
                  Expr.splice(constructExpr)
                }
              })
            }
      }
    }

    /** Decode case class fields from an already-opened JSON object (for discriminator mode). The object's `{` and
      * discriminator key-value have already been read. Returns Expr[A] that reads remaining fields via
      * readObjectInline.
      */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => hasAnnotationType[transientField](p) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields: just read closing `}`
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = CodecDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some("Unexpected field in zero-arg case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  val reader = Expr.splice(dctx.reader)
                  if (!reader.isNextToken('}'.toByte)) {
                    if (reader.isCurrentToken(','.toByte)) {
                      reader.rollbackToken()
                      while (reader.isNextToken(','.toByte)) {
                        val _ = reader.readKeyAsString()
                        reader.skip()
                      }
                    }
                  }
                  Expr.splice(expr)
                })
              case None =>
                val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Build transient field default values
          val transientDefaults: Map[String, Expr_??] = buildTransientDefaults(fieldsList)

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation).map { decodeFn =>
                  val decodeFnErased: Expr[JsonReader => Any] = Expr.quote { (r: JsonReader) =>
                    Expr.splice(decodeFn).apply(r).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      JsoniterDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(arrayIndex))),
                        Expr.splice(decodeFn)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (fName, arrayIndex, decodeFnErased, makeAccessor, nameOverride)
                }
              }
            }
            .flatMap { fieldData =>
              val fieldDataList = fieldData.toList

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val requireCheckAll0: Expr[Unit] =
                    foldExprUnits(buildRequireCheckExprs(nonTransientWithIndex, decodedValuesExpr, dctx.config))

                  val transientInitAll: Expr[Unit] =
                    foldExprUnits(buildTransientInitExprs(nonTransientWithIndex, decodedValuesExpr, dctx.config))

                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) =>
                      MIO.pure(Expr.quote {
                        Expr.splice(requireCheckAll0)
                        Expr.splice(transientInitAll)
                        Expr.splice(constructExpr)
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    JsoniterDerivationUtils.readObjectInline[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        fieldMappings.foldRight(Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else reader.decodeError("unexpected field: " + fieldName)
                        }: Expr[Unit]) {
                          case ((name, index, decodeFnErased, Some(customName)), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(Expr(customName))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
                                if (_seen != null) {
                                  if (_seen(Expr.splice(Expr(index))))
                                    JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                  _seen(Expr.splice(Expr(index))) = true
                                }
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                        }
                      }
                    }
                  }
                }
            }
      }
    }

  }

  /** Derive a decode function for a case class field. Tries implicit summoning first, falls back to a forward-declared
    * cached `def`.
    *
    * Per project rule 5, [[LambdaBuilder]] is reserved for lambdas passed into collection / Optional iteration helpers.
    * The recursive fallback uses the def-caching pattern (see `docs/contributing/def-caching-skill.md`):
    *
    *   - Forward-declare `def decode_field_X(reader: JsonReader, config: JsoniterConfig): Field` against the shared
    *     [[ValDefsCache]] (the same one populated by the surrounding codec entry point).
    *   - Populate it via `buildCachedWith`, building a *fresh* `DecoderCtx` from the def's own parameter Exprs so the
    *     body never references outer-scope-bound names (which would not resolve once the def is hoisted to the outer
    *     scope by `vals.toValDefs.use`).
    *   - Wrap the resulting helper-call function in a direct cross-quotes lambda so callers still get the expected
    *     `Expr[JsonReader => Field]` shape.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[JsonReader => Field]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
    implicit val JsoniterConfigT: Type[JsoniterConfig] = CTypes.JsoniterConfig

    Log.namedScope(s"deriveFieldDecoder:${Type[Field].prettyPrint}") {
      summonJsonValueCodecCached[Field] match {
        case Right(codecExpr) =>
          Log.info(s"Found implicit JsonValueCodec[${Type[Field].prettyPrint}]") >> MIO.pure(
            Expr.quote { (r: JsonReader) =>
              Expr.splice(codecExpr).decodeValue(r, Expr.splice(codecExpr).nullValue)
            }
          )
        case Left(_) =>
          Log.info(s"Building decoder for ${Type[Field].prettyPrint} via cached def") >> {
            val cacheKey = s"jsoniter-field-decode:${Type[Field].prettyPrint}"
            val defBuilder =
              ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, Field](s"decode_field_${Type[Field].shortName}")
            for {
              // Memoize: case classes can have multiple fields of the same type, and the
              // case-class rule iterates them sequentially. Without this guard, the second
              // field of the same type would re-enter `buildCachedWith` for an already-built
              // key and trigger `HearthRequirementError("key+signature already built")`.
              cacheState <- ctx.cache.get
              _ <-
                if (defBuilder.isBuilt(cacheState, cacheKey)) MIO.pure(())
                else
                  for {
                    _ <- ctx.cache.forwardDeclare(cacheKey, defBuilder)
                    _ <- MIO.scoped { rs =>
                      rs(ctx.cache.buildCachedWith(cacheKey, defBuilder) { case (_, (readerExpr, configExpr)) =>
                        rs(
                          deriveDecoderRecursively[Field](using
                            DecoderCtx.from(readerExpr, configExpr, ctx.cache, ctx.derivedType)
                          )
                        )
                      })
                    }
                  } yield ()
              callerOpt <- ctx.cache.get2Ary[JsonReader, JsoniterConfig, Field](cacheKey)
            } yield {
              val callerFn = callerOpt.get
              val configExpr: Expr[JsoniterConfig] = ctx.config
              Expr.quote { (r: JsonReader) =>
                Expr.splice(callerFn(Expr.quote(r), configExpr))
              }
            }
          }
      }
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def deriveStringifiedDecoder[F: Type](implicit
      JsonReaderT: Type[JsonReader]
  ): Option[Expr[JsonReader => F]] =
    if (Type[F] =:= CTypes.Int)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsInt().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Long)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsLong().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Double)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsDouble().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Float)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsFloat().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Short)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsShort().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.Byte)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsByte().asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.BigDecimal)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsBigDecimal(null).asInstanceOf[F]
      })
    else if (Type[F] =:= CTypes.BigInt)
      Some(Expr.quote { (r: JsonReader) =>
        r.readStringAsBigInt(null).asInstanceOf[F]
      })
    else None

}
