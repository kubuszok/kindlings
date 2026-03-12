package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.annotations.{fieldName as fieldNameAnn, stringified, transientField}
import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (reader, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(reader, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.reader, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

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
          val transientDefaults: Map[String, Expr_??] = fieldsList
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
                val decodeFnMIO: MIO[Expr[JsonReader => Field]] = if (hasStringifiedAnnotation) {
                  deriveStringifiedDecoder[Field] match {
                    case Some(dec) => MIO.pure(dec)
                    case None      =>
                      val err = CodecDerivationError
                        .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                      Log.error(err.message) >> MIO.fail(err)
                  }
                } else {
                  deriveStringifiedDecoder[Field] match {
                    case Some(stringifiedDec) =>
                      deriveFieldDecoder[Field].map { normalDec =>
                        Expr.quote { (r: JsonReader) =>
                          if (Expr.splice(dctx.config).isStringified)
                            Expr.splice(stringifiedDec).apply(r)
                          else
                            Expr.splice(normalDec).apply(r)
                        }
                      }
                    case None =>
                      deriveFieldDecoder[Field]
                  }
                }
                decodeFnMIO.map { decodeFn =>
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
                  // Build require check expressions: validate required fields before transient init
                  val requireCheckExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val checks = List.newBuilder[Expr[Unit]]

                      // requireDefaultFields: fields with defaults must be explicitly present
                      if (param.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
                        }
                      }

                      // requireCollectionFields: collection/map fields must be explicitly present
                      val isCollOrMap = Type[Field] match {
                        case IsMap(_)        => true
                        case IsCollection(_) => true
                        case _               => false
                      }
                      if (isCollOrMap) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireCollectionFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
                        }
                      }

                      checks.result()
                    }

                  val requireCheckAll: Expr[Unit] =
                    requireCheckExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  // Build transient init expressions: fill null slots for absent fields
                  val transientInitExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val initSteps = List.newBuilder[Expr[Unit]]

                      // transientDefault: use field's default value
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
                                Expr.splice(dctx.config).transientDefault &&
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                              )
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(defaultExpr)
                            }
                          }
                      }

                      // transientNone: use None for Option fields
                      val isOpt = Type[Field] match {
                        case IsOption(_) => true; case _ => false
                      }
                      if (isOpt) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientNone &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = (None: Any)
                        }
                      }

                      // transientEmpty: use "" for String fields
                      if (Type[Field] =:= CTypes.String) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientEmpty &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = ("": Any)
                        }
                      }

                      initSteps.result()
                    }

                  val transientInitAll: Expr[Unit] =
                    transientInitExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

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
          val transientDefaults: Map[String, Expr_??] = fieldsList
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

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldNameAnn](param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              val hasStringifiedAnnotation = hasAnnotationType[stringified](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                val decodeFnMIO: MIO[Expr[JsonReader => Field]] = if (hasStringifiedAnnotation) {
                  deriveStringifiedDecoder[Field] match {
                    case Some(dec) => MIO.pure(dec)
                    case None      =>
                      val err = CodecDerivationError
                        .StringifiedOnNonNumeric(fName, Type[A].prettyPrint, Type[Field].prettyPrint)
                      Log.error(err.message) >> MIO.fail(err)
                  }
                } else {
                  deriveStringifiedDecoder[Field] match {
                    case Some(stringifiedDec) =>
                      deriveFieldDecoder[Field].map { normalDec =>
                        Expr.quote { (r: JsonReader) =>
                          if (Expr.splice(dctx.config).isStringified)
                            Expr.splice(stringifiedDec).apply(r)
                          else
                            Expr.splice(normalDec).apply(r)
                        }
                      }
                    case None =>
                      deriveFieldDecoder[Field]
                  }
                }
                decodeFnMIO.map { decodeFn =>
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
                  // Build require check expressions: validate required fields before transient init
                  val requireCheckExprs0: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName0, param0), idx0) =>
                      import param0.tpe.Underlying as Field0
                      val checks = List.newBuilder[Expr[Unit]]

                      if (param0.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName0)))
                        }
                      }

                      val isCollOrMap0 = Type[Field0] match {
                        case IsMap(_)        => true
                        case IsCollection(_) => true
                        case _               => false
                      }
                      if (isCollOrMap0) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireCollectionFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            JsoniterDerivationUtils.throwMissingField(Expr.splice(Expr(fName0)))
                        }
                      }

                      checks.result()
                    }

                  val requireCheckAll0: Expr[Unit] =
                    requireCheckExprs0.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

                  // Build transient init expressions: fill null slots for absent fields
                  val transientInitExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName0, param0), idx0) =>
                      import param0.tpe.Underlying as Field0
                      val initSteps = List.newBuilder[Expr[Unit]]

                      if (param0.hasDefault) {
                        param0.defaultValue
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
                                Expr.splice(dctx.config).transientDefault &&
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                              )
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = Expr.splice(defaultExpr)
                            }
                          }
                      }

                      val isOpt0 = Type[Field0] match {
                        case IsOption(_) => true; case _ => false
                      }
                      if (isOpt0) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientNone &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = (None: Any)
                        }
                      }

                      if (Type[Field0] =:= CTypes.String) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientEmpty &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx0))) = ("": Any)
                        }
                      }

                      initSteps.result()
                    }

                  val transientInitAll: Expr[Unit] =
                    transientInitExprs.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                      Expr.quote {
                        Expr.splice(acc)
                        Expr.splice(step)
                      }
                    }

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

  /** Derive a decode function for a case class field. Tries implicit summoning first, falls back to recursive
    * derivation via the full rule chain.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  protected def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[JsonReader => Field]] = {
    implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

    CTypes
      .JsonValueCodec[Field]
      .summonExprIgnoring(DecoderUseImplicitWhenAvailableRule.ignoredImplicits*)
      .toEither match {
      case Right(codecExpr) =>
        Log.info(s"Found implicit JsonValueCodec[${Type[Field].prettyPrint}]") >> MIO.pure(
          Expr.quote { (r: JsonReader) =>
            Expr.splice(codecExpr).decodeValue(r, Expr.splice(codecExpr).nullValue)
          }
        )
      case Left(_) =>
        Log.info(s"Building decoder for ${Type[Field].prettyPrint} via recursive derivation") >>
          LambdaBuilder
            .of1[JsonReader]("fieldReader")
            .traverse { fieldReaderExpr =>
              deriveDecoderRecursively[Field](using ctx.nest[Field](fieldReaderExpr))
            }
            .map { builder =>
              builder.build[Field]
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
