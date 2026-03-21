package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.{UBJsonConfig, UBJsonReader}
import hearth.kindlings.ubjsonderivation.annotations.{fieldName as fieldNameAnn, transientField}
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait DecoderHandleAsCaseClassRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            // Note: caching is handled by deriveDecoderRecursively
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

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
                  UBJsonDerivationUtils.readEmptyObject(Expr.splice(dctx.reader))
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
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[UBJsonReader => Any] = Expr.quote { (r: UBJsonReader) =>
                    Expr.splice(decodeFn(Expr.quote(r))).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = mkFieldAccessExpr[Field](arrExpr, arrayIndex)
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
                  // Require check
                  val requireCheckExprs: List[Expr[Unit]] =
                    nonTransientWithIndex.flatMap { case ((fName, param), idx) =>
                      import param.tpe.Underlying as Field
                      val checks = List.newBuilder[Expr[Unit]]

                      if (param.hasDefault) {
                        checks += Expr.quote {
                          if (
                            Expr.splice(dctx.config).requireDefaultFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            UBJsonDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
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
                            Expr.splice(dctx.config).requireCollectionFields &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            UBJsonDerivationUtils.throwMissingField(Expr.splice(Expr(fName)))
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

                  // Transient init
                  val transientInitExprs: List[Expr[Unit]] =
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
                                Expr.splice(dctx.config).transientDefault &&
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
                            Expr.splice(dctx.config).transientNone &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = (None: Any)
                        }
                      }

                      if (Type[Field] =:= CTypes.String) {
                        initSteps += Expr.quote {
                          if (
                            Expr.splice(dctx.config).transientEmpty &&
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                          )
                            Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = ("": Any)
                        }
                      }

                      // For collections/maps, use default value when transientEmpty and field is missing
                      val isCollOrMapField = !isOpt && {
                        val isMapF = Type[Field] match { case IsMap(_) => true; case _ => false }
                        val isCollF = Type[Field] match { case IsCollection(_) => true; case _ => false }
                        isMapF || isCollF
                      }
                      if (isCollOrMapField && param.hasDefault) {
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
                                Expr.splice(dctx.config).transientEmpty &&
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) == null
                              )
                                Expr.splice(decodedValuesExpr)(Expr.splice(Expr(idx))) = Expr.splice(defaultExpr)
                            }
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

                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    (name, index, decodeFnErased, nameOverride)
                  }

                  Expr.quote {
                    val _seen: Array[Boolean] =
                      if (Expr.splice(dctx.config).checkFieldDuplication)
                        new Array[Boolean](Expr.splice(Expr(fieldMappings.size)))
                      else null
                    UBJsonDerivationUtils.readObject[A](
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
                                    UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
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
                                    UBJsonDerivationUtils.throwDuplicateField(reader, fieldName)
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

    /** Decode fields from an already-opened object (for discriminator mode). */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

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
                  Some("Unexpected field in zero-arg case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                MIO.pure(Expr.quote {
                  val reader = Expr.splice(dctx.reader)
                  while (!reader.isObjectEnd()) {
                    val _ = reader.readFieldName()
                    reader.skip()
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
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decodeFn =>
                  val decodeFnErased: Expr[UBJsonReader => Any] = Expr.quote { (r: UBJsonReader) =>
                    Expr.splice(decodeFn(Expr.quote(r))).asInstanceOf[Any]
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = mkFieldAccessExpr[Field](arrExpr, arrayIndex)
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
                  val fieldMap = nonTransientFieldMap ++ transientDefaults
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
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
                    UBJsonDerivationUtils.readObjectInline[A](
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
                                arr(Expr.splice(Expr(index))) = Expr.splice(decodeFnErased).apply(reader)
                              } else Expr.splice(elseExpr)
                            }
                          case ((name, index, decodeFnErased, None), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(dctx.config).fieldNameMapper(Expr.splice(Expr(name)))) {
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

  /** Helper method to create a typed field access expression from an Array[Any]. Extracted to avoid path-dependent type
    * issues on Scala 2 when `Field` comes from `import param.tpe.Underlying as Field` inside `Expr.quote`.
    */
  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def mkFieldAccessExpr[F: Type](arrExpr: Expr[Array[Any]], index: Int): Expr[F] = {
    implicit val AnyT: Type[Any] = CTypes.Any
    implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny
    Expr.quote {
      Expr.splice(arrExpr)(Expr.splice(Expr(index))).asInstanceOf[F]
    }
  }

  @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
  private def deriveFieldDecoder[Field: Type](implicit ctx: DecoderCtx[?]): MIO[Expr[UBJsonReader] => Expr[Field]] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val ConfigT: Type[UBJsonConfig] = CTypes.UBJsonConfig

    // 1. Check if already cached via setHelper (case class/enum rules)
    ctx.getHelper[Field].flatMap {
      case Some(helperCall) =>
        Log.info(s"Found cached decoder helper for field ${Type[Field].prettyPrint}") >>
          MIO.pure((reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config))
      case None =>
        // 2. Check if cached via previous deriveFieldDecoder call
        ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field]("field-decode-method").flatMap {
          case Some(helperCall) =>
            Log.info(s"Found field decode cache for ${Type[Field].prettyPrint}") >>
              MIO.pure((reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config))
          case None =>
            // 3. Forward-declare, derive body via traverse, then build
            val defBuilder = ValDefBuilder.ofDef2[UBJsonReader, UBJsonConfig, Field](
              s"decode_field_${Type[Field].shortName}"
            )
            for {
              _ <- Log.info(s"Building decoder for ${Type[Field].prettyPrint} via cached def")
              _ <- ctx.cache.forwardDeclare("field-decode-method", defBuilder)
              builtBuilder <- defBuilder.traverse { case (_, (reader, config)) =>
                val nestedCtx =
                  DecoderCtx.from[Field](reader, config, ctx.cache, ctx.derivedType)
                deriveDecoderRecursively[Field](using nestedCtx)
              }
              currentCache <- ctx.cache.get
              _ <- ctx.cache.set(builtBuilder.buildCached(currentCache, "field-decode-method"))
              fn <- ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field]("field-decode-method")
            } yield {
              val helperCall = fn.get
              (reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config)
            }
        }
    }
  }

}
