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
    if (literalKey[Field].isDefined || nullableUnionUnderlying[Field].isDefined)
      None // nullable unions and literal types are checked by their own rule
    else if (Type[Field] =:= Type.of[Int]) Some(Expr.quote(Expr.splice(readerExpr).readInt().asInstanceOf[Field]))
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

  /** Derive decoder function for a single case class field. A field is stringified only by its own `@stringified`
    * (numbers and booleans of its value are JSON strings, also inside options, collections, map values and tuples). The
    * field's default value is applied by `withFieldDefault`/`withFieldDefaultFn` (DecoderAbsentFieldSupport).
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoderFn[Field: Type](
      fName: String,
      typeName: String,
      hasStringifiedAnnotation: Boolean
  )(implicit ctx: DecoderCtx[?], JsonReaderT: Type[JsonReader]): MIO[Expr[JsonReader => Field]] =
    Log.namedScope(s"fieldDecoderFn:$fName") {
      deriveFieldDecoder[Field](using Type[Field], ctx.copy(stringified = Some(hasStringifiedAnnotation)))
    }

  /** Fill expressions for absent fields of the generic (runtime config) decoders. */
  private def buildAbsentFieldFillExprs[A: DecoderCtx](
      nonTransientWithIndex: List[((String, Parameter), Int)],
      decodedValuesExpr: Expr[Array[Any]],
      config: Expr[hearth.kindlings.jsoniterderivation.JsoniterConfig],
      reader: Expr[JsonReader],
      argumentValues: Map[String, Expr_??]
  )(implicit fieldNameT: Type[fieldNameAnn]): List[Expr[Unit]] =
    absentFieldFillExprs(
      nonTransientWithIndex.map { case ((fName, param), idx) =>
        (fName, param, idx, fieldNameOverride(param))
      },
      decodedValuesExpr,
      config,
      reader,
      argumentValues,
      name =>
        nonTransientWithIndex.exists { case ((n, p), _) =>
          n == name && isTransientAtRuntimeOnly[A](p, dctx.evaluatedConfig)
        }
    )

  /** Build map of transient field default values for the constructor. Defaults of non-first parameter lists may depend
    * on the preceding parameters (`nonTransientValues` and the preceding transient defaults).
    */
  private def buildTransientDefaults[A: DecoderCtx](
      fieldsList: List[(String, Parameter)],
      nonTransientValues: Map[String, Expr_??]
  ): Map[String, Expr_??] =
    fieldsList.foldLeft(Map.empty[String, Expr_??]) { case (acc, (fName, param)) =>
      if (!isTransientParam[A](param, dctx.evaluatedConfig)) acc
      else
        parameterDefault(param, nonTransientValues ++ acc).fold(acc)(expr => acc + (fName -> expr))
    }

  /** Constructs a case class without non-transient fields (zero-param or all-`@transientField`) from the transient
    * defaults.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def constructWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[A]] =
    foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
      onTypes = _ => Map.empty,
      onValues = _ => buildTransientDefaults(fieldsList, Map.empty)
    ) match {
      case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
      case Left(error)          =>
        val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
        Log.error(err.message) >> MIO.fail(err)
    }

  @scala.annotation.nowarn("msg=is never used")
  private def skipUnexpectedExpr[A: DecoderCtx]: Expr[Boolean] = {
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    dctx.evaluatedConfig match {
      case Some(c) => Expr(c.skipUnexpectedFields)
      case None    => Expr.quote(Expr.splice(dctx.config).skipUnexpectedFields)
    }
  }

  /** Decodes a case class without non-transient fields (zero-param or all-`@transientField`): constructs it from the
    * transient defaults and reads the object, skipping unknown keys unless `skipUnexpectedFields = false` (as
    * jsoniter-scala does).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[A]] = {
    implicit val StringT: Type[String] = CTypes.String
    constructWithoutFields[A](caseClass, fieldsList).map { constructed =>
      val allowedKey: Expr[String] =
        alwaysEmittedDiscriminatorKey[A](dctx.config, dctx.evaluatedConfig).getOrElse(Expr.quote(null: String))
      Expr.quote {
        JsoniterDerivationUtils.readObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpectedExpr[A]),
          Expr.splice(allowedKey),
          false
        )
        Expr.splice(constructed)
      }
    }
  }

  /** [[decodeObjectWithoutFields]] for an ADT leaf in discriminator mode (see
    * `JsoniterDerivationUtils.readInlineObjectWithoutFields`).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeInlineObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)]
  ): MIO[Expr[Boolean] => Expr[A]] = {
    implicit val StringT: Type[String] = CTypes.String
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    constructWithoutFields[A](caseClass, fieldsList).map { constructed => (afterOpen: Expr[Boolean]) =>
      val checkDuplication: Expr[Boolean] = dctx.evaluatedConfig match {
        case Some(c) => Expr(c.checkFieldDuplication)
        case None    => Expr.quote(Expr.splice(dctx.config).checkFieldDuplication)
      }
      Expr.quote {
        JsoniterDerivationUtils.readInlineObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpectedExpr[A]),
          Expr.splice(dctx.config).discriminatorFieldName.orNull,
          Expr.splice(checkDuplication),
          Expr.splice(afterOpen)
        )
        Expr.splice(constructed)
      }
    }
  }

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) => decodeProduct[A](caseClass).map(Rule.matched)
          case Left(reason)     => MIO.pure(Rule.yielded(reason))
        }
      }

    /** Decodes a product as a JSON object, with the optimized codegen when the config is known at compile time. */
    private[compiletime] def decodeProduct[A: DecoderCtx](caseClass: ProductType[A]): MIO[Expr[A]] =
      if (dctx.evaluatedConfig.isDefined) decodeCaseClassFieldsOptimized[A](caseClass)
      else decodeCaseClassFields[A](caseClass)

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Separate transient from non-transient fields
      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          // Re-index non-transient fields for the array (0, 1, 2, ...)
          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          // Step 1: For each non-transient field, derive a decoder and build dispatch/accessor.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation).map { rawDecodeFn =>
                  val decodeFn = withFieldDefaultFn[Field](param)(rawDecodeFn)
                  val decodeFnErased: Expr[JsonReader => Any] =
                    if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) Expr.quote { (r: JsonReader) =>
                      JsoniterDerivationUtils.readUnlessTransient(
                        r,
                        Expr.splice(dctx.config).scalaTransientSupport,
                        (r2: JsonReader) => Expr.splice(decodeFn).apply(r2).asInstanceOf[Any]
                      )
                    }
                    else
                      Expr.quote { (r: JsonReader) =>
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
                  // Non-transient fields read from the array
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  // Merge with transient defaults
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults(fieldsList, nonTransientFieldMap)
                  // Absent fields are filled in parameter order, so defaults may depend on preceding parameters.
                  val absentFieldFillAll: Expr[Unit] =
                    foldExprUnits(
                      buildAbsentFieldFillExprs(
                        nonTransientWithIndex,
                        decodedValuesExpr,
                        dctx.config,
                        dctx.reader,
                        fieldMap
                      )
                    )
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(
                        Expr.quote {
                          Expr.splice(absentFieldFillAll)
                          Expr.splice(constructExpr.value.asInstanceOf[Expr[A]])
                        }
                      )
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
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size + 1)))
                      else null
                    JsoniterDerivationUtils.readObject[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        val unexpected: Expr[Unit] = Expr.quote {
                          if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else JsoniterDerivationUtils.unexpectedField(reader, fieldName)
                        }
                        // `alwaysEmitDiscriminator`: ADT leaf codecs accept (and skip) the discriminator they write
                        val fallback: Expr[Unit] =
                          alwaysEmittedDiscriminatorKey[A](dctx.config, None) match {
                            case Some(discriminatorKey) =>
                              Expr.quote {
                                if (fieldName == Expr.splice(discriminatorKey)) {
                                  if (_seen != null) {
                                    if (_seen(Expr.splice(Expr(fieldMappings.size))))
                                      JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                                    _seen(Expr.splice(Expr(fieldMappings.size))) = true
                                  }
                                  reader.skip()
                                } else Expr.splice(unexpected)
                              }
                            case None => unexpected
                          }
                        fieldMappings.foldRight(fallback) {
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
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val config = dctx.evaluatedConfig.get
      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          implicit val BooleanT: Type[Boolean] = CTypes.Boolean
          implicit val UnitT: Type[Unit] = CTypes.Unit

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              val mappedName = nameOverride.getOrElse(config.fieldNameMapper(fName))
              Log.namedScope(s"Deriving optimized decoder for field $fName: ${Type[Field].prettyPrint}") {
                val isStringifiedStaticallyFalse = true // fields are stringified only by `@stringified`

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
                      deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation)
                    decoderMIO.map(fn => (r: Expr[JsonReader]) => Expr.quote(Expr.splice(fn).apply(Expr.splice(r))))
                }

                deriveMIO.map { rawDecodeField =>
                  val decodeField = withFieldDefault[Field](param)(rawDecodeField)
                  // Absent-field handling (see DecoderAbsentFieldSupport), resolved at compile time: an `Option`
                  // without an applicable default starts as `None`; every other field gets a presence flag checked
                  // after the read loop (fill with default/empty, or fail as required). The flag also detects
                  // duplicated keys when `checkFieldDuplication` is on.
                  val readerExpr = dctx.reader
                  val fallbacks = absentFieldFallbacks[Field](param)
                  val startsAsNone = absentIsAlwaysNone(fallbacks, config)
                  val initExpr: Expr[Field] =
                    if (startsAsNone) staticAbsentFallback(fallbacks, config).get else deriveZeroValue[Field]
                  val fieldVar = ValDefs.createVar[Field](initExpr, s"_$fName")
                  if (startsAsNone && !config.checkFieldDuplication)
                    fieldVar.map { case (getter, setter) =>
                      val setFromReader: Expr[JsonReader] => Expr[Unit] = { r => setter(decodeField(r)) }
                      val afterLoop: Map[String, Expr_??] => Expr[Unit] = _ => Expr.quote(())
                      (fName, mappedName, getter.as_??, setFromReader, afterLoop)
                    }
                  else
                    fieldVar.map2(ValDefs.createVar[Boolean](Expr(false), s"_${fName}_present")) {
                      case ((getter, setter), (isPresent, setPresent)) =>
                        val setFromReader: Expr[JsonReader] => Expr[Unit] = { r =>
                          val markPresent: Expr[Unit] =
                            if (config.checkFieldDuplication) Expr.quote {
                              if (Expr.splice(isPresent))
                                JsoniterDerivationUtils.throwDuplicateField(
                                  Expr.splice(r),
                                  Expr.splice(Expr(mappedName))
                                )
                              Expr.splice(setPresent(Expr(true)))
                            }
                            else setPresent(Expr(true))
                          Expr.quote {
                            Expr.splice(markPresent)
                            Expr.splice(setter(decodeField(r)))
                          }
                        }
                        // Built once all field variables exist: defaults may depend on preceding parameters.
                        val afterLoop: Map[String, Expr_??] => Expr[Unit] = values =>
                          if (startsAsNone) Expr.quote(())
                          else {
                            val fallbacksWithArgs = absentFieldFallbacks[Field](param, values)
                            val fill: Expr[Unit] = staticAbsentFallback(fallbacksWithArgs, config) match {
                              case Some(fallback) => setter(fallback)
                              case None           =>
                                setter(missingRequiredFieldOf[Field](readerExpr, Expr(mappedName)))
                            }
                            Expr.quote(if (!Expr.splice(isPresent)) Expr.splice(fill))
                          }
                        (fName, mappedName, getter.as_??, setFromReader, afterLoop)
                    }
                }
              }
            }
            .flatMap { fieldVarDefs =>
              val emptyVarDefs: ValDefs[
                List[(String, String, Expr_??, Expr[JsonReader] => Expr[Unit], Map[String, Expr_??] => Expr[Unit])]
              ] =
                ValDefsTraverse.pure(List.empty)

              val allFieldVars = fieldVarDefs.toList.foldLeft(emptyVarDefs) { (acc, varDef) =>
                acc.map2(varDef) { case (list, entry) => list :+ entry }
              }

              val lenVar = ValDefs.createVar[Int](Expr(-1), "_l")
              // `alwaysEmitDiscriminator`: ADT leaf codecs accept (and skip) the discriminator they write
              val discriminatorKey: Option[String] =
                if (config.alwaysEmitDiscriminator && hasSealedParent[A]) config.discriminatorFieldName else None
              val discriminatorPresentVar: ValDefs[Option[(Expr[Boolean], Expr[Boolean] => Expr[Unit])]] =
                if (discriminatorKey.isDefined && config.checkFieldDuplication)
                  ValDefs.createVar[Boolean](Expr(false), "_discriminator_present").map(Some(_))
                else ValDefsTraverse.pure(None)
              val combined = allFieldVars.map2(lenVar.map2(discriminatorPresentVar)((l, d) => (l, d))) {
                case (fieldInfos, ((lenGet, lenSet), discriminatorPresent)) =>
                  (fieldInfos, lenGet, lenSet, discriminatorPresent)
              }

              MIO.pure(combined.use { case (fieldInfos, lenGetter, lenSetter, discriminatorPresent) =>
                val nonTransientValues: Map[String, Expr_??] =
                  fieldInfos.map { case (name, _, getter, _, _) => (name, getter) }.toMap
                val fieldMap: Map[String, Expr_??] =
                  nonTransientValues ++ buildTransientDefaults(fieldsList, nonTransientValues)
                val constructExpr: Expr[A] = foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                  onTypes = _ => Map.empty,
                  onValues = _ => fieldMap
                ) match {
                  case Right(expr) => expr.value.asInstanceOf[Expr[A]]
                  case Left(error) => Environment.reportErrorAndAbort(error)
                }

                val readerExpr = dctx.reader

                val unexpectedKey: Expr[Unit] =
                  if (config.skipUnexpectedFields) Expr.quote(Expr.splice(readerExpr).skip())
                  else Expr.quote(Expr.splice(readerExpr).unexpectedKeyError(Expr.splice(lenGetter)))
                val unexpectedField: Expr[Unit] = discriminatorKey match {
                  case Some(key) =>
                    val markPresent: Expr[Unit] = discriminatorPresent match {
                      case Some((isPresent, setPresent)) =>
                        Expr.quote {
                          if (Expr.splice(isPresent))
                            JsoniterDerivationUtils.throwDuplicateField(Expr.splice(readerExpr), Expr.splice(Expr(key)))
                          Expr.splice(setPresent(Expr(true)))
                        }
                      case None => Expr.quote(())
                    }
                    Expr.quote {
                      if (Expr.splice(readerExpr).isCharBufEqualsTo(Expr.splice(lenGetter), Expr.splice(Expr(key)))) {
                        Expr.splice(markPresent)
                        Expr.splice(readerExpr).skip()
                      } else Expr.splice(unexpectedKey)
                    }
                  case None => unexpectedKey
                }

                val dispatch: Expr[Unit] =
                  fieldInfos.foldRight(unexpectedField) { case ((_, mappedName, _, setFromReader, _), elseExpr) =>
                    Expr.quote {
                      if (
                        Expr.splice(readerExpr).isCharBufEqualsTo(Expr.splice(lenGetter), Expr.splice(Expr(mappedName)))
                      )
                        Expr.splice(setFromReader(readerExpr))
                      else Expr.splice(elseExpr)
                    }
                  }

                val afterLoopAll: Expr[Unit] = foldExprUnits(fieldInfos.map(_._5(fieldMap)))

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
                  Expr.splice(afterLoopAll)
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
        caseClass: ProductType[A]
    ): MIO[Expr[Boolean] => Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField
      implicit val stringifiedT: Type[stringified] = CTypes.Stringified

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.totalParameters.flatten.toList

      // Validate: @transientField on fields without defaults
      fieldsList
        .collectFirst {
          case (name, param) if isTransientParam[A](param, dctx.evaluatedConfig) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = CodecDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      val nonTransientFields = fieldsList.filterNot { case (_, p) => isTransientParam[A](p, dctx.evaluatedConfig) }

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          // Zero non-transient fields (zero-param or all-@transientField)
          decodeInlineObjectWithoutFields[A](caseClass, fieldsList)

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoderFn[Field](fName, Type[A].prettyPrint, hasStringifiedAnnotation).map { rawDecodeFn =>
                  val decodeFn = withFieldDefaultFn[Field](param)(rawDecodeFn)
                  val decodeFnErased: Expr[JsonReader => Any] =
                    if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) Expr.quote { (r: JsonReader) =>
                      JsoniterDerivationUtils.readUnlessTransient(
                        r,
                        Expr.splice(dctx.config).scalaTransientSupport,
                        (r2: JsonReader) => Expr.splice(decodeFn).apply(r2).asInstanceOf[Any]
                      )
                    }
                    else
                      Expr.quote { (r: JsonReader) =>
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
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults(fieldsList, nonTransientFieldMap)
                  // Absent fields are filled in parameter order, so defaults may depend on preceding parameters.
                  val absentFieldFillAll: Expr[Unit] =
                    foldExprUnits(
                      buildAbsentFieldFillExprs(
                        nonTransientWithIndex,
                        decodedValuesExpr,
                        dctx.config,
                        dctx.reader,
                        fieldMap
                      )
                    )
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(
                        Expr.quote {
                          Expr.splice(absentFieldFillAll)
                          Expr.splice(constructExpr.value.asInstanceOf[Expr[A]])
                        }
                      )
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder => (afterOpen: Expr[Boolean]) =>
                  val constructLambda = builder.build[A]

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size + 1)))
                      else null
                    JsoniterDerivationUtils.readObjectInline[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda),
                      Expr.splice(afterOpen),
                      Expr.splice(dctx.config).discriminatorFieldName.orNull
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        // The first discriminator was consumed already: a second one is a duplicate (with
                        // checkFieldDuplication) or skipped, as in jsoniter-scala.
                        val fallback: Expr[Unit] = Expr.quote {
                          if (Expr.splice(dctx.config).discriminatorFieldName.contains(fieldName)) {
                            if (_seen != null) JsoniterDerivationUtils.throwDuplicateField(reader, fieldName)
                            else reader.skip()
                          } else if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                          else JsoniterDerivationUtils.unexpectedField(reader, fieldName)
                        }
                        fieldMappings.foldRight(fallback) {
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
            val stringifiedSuffix = stringifiedKeySuffix(ctx.stringified)
            val cacheKey = s"jsoniter-field-decode:${Type[Field].prettyPrint}$stringifiedSuffix"
            val defBuilder = ValDefBuilder.ofDef2[JsonReader, JsoniterConfig, Field](
              s"decode_field_${stringifiedNamePrefix(ctx.stringified)}${Type[Field].shortName}"
            )
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
                            DecoderCtx
                              .from[Field](
                                readerExpr,
                                configExpr,
                                ctx.cache,
                                ctx.derivedType,
                                evaluatedConfig = ctx.evaluatedConfig
                              )
                              .copy(stringified = ctx.stringified)
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

}
