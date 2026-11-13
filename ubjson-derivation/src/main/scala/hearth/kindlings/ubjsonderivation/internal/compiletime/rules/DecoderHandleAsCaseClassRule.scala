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

  /** Defaults of the transient fields for the constructor. Defaults of non-first parameter lists may depend on the
    * preceding parameters (`nonTransientValues` and the preceding transient defaults).
    */
  private def buildTransientDefaults[A: DecoderCtx](
      fieldsList: List[(String, Parameter)],
      nonTransientValues: Map[String, Expr_??]
  ): Map[String, Expr_??] =
    fieldsList.foldLeft(Map.empty[String, Expr_??]) { case (acc, (fName, param)) =>
      if (!isTransientParam[A](param, dctx.evaluatedConfig)) acc
      else parameterDefault(param, nonTransientValues ++ acc).fold(acc)(expr => acc + (fName -> expr))
    }

  /** Fill expressions of the absent fields (in parameter order, so defaults may depend on preceding parameters). */
  private def buildAbsentFieldFillExprs[A: DecoderCtx](
      nonTransientWithIndex: List[((String, Parameter), Int)],
      decodedValuesExpr: Expr[Array[Any]],
      argumentValues: Map[String, Expr_??]
  ): List[Expr[Unit]] =
    absentFieldFillExprs(
      nonTransientWithIndex.map { case ((fName, param), idx) => (fName, param, idx, fieldNameOverride(param)) },
      decodedValuesExpr,
      dctx.config,
      dctx.reader,
      argumentValues,
      name =>
        nonTransientWithIndex.exists { case ((n, p), _) =>
          n == name && isTransientAtRuntimeOnly[A](p, dctx.evaluatedConfig)
        }
    )

  /** A field's decode function, which with a runtime config skips a `scala.transient` field when
    * `scalaTransientSupport` is on (its slot stays absent: it gets its default).
    */
  @scala.annotation.nowarn("msg=is never used")
  private def erasedFieldDecodeFn[A: DecoderCtx, Field: Type](
      param: Parameter,
      decodeFn: Expr[UBJsonReader] => Expr[Field]
  ): Expr[UBJsonReader => Any] = {
    implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
    implicit val AnyT: Type[Any] = CTypes.Any
    if (isTransientAtRuntimeOnly[A](param, dctx.evaluatedConfig)) Expr.quote { (r: UBJsonReader) =>
      UBJsonDerivationUtils.readUnlessTransient(
        r,
        Expr.splice(dctx.config).scalaTransientSupport,
        (r2: UBJsonReader) => Expr.splice(decodeFn(Expr.quote(r2))).asInstanceOf[Any]
      )
    }
    else Expr.quote((r: UBJsonReader) => Expr.splice(decodeFn(Expr.quote(r))).asInstanceOf[Any])
  }

  /** Decodes a case class without non-transient fields (zero-param or all-`@transientField`): constructs it from the
    * transient defaults and reads the object, skipping unknown keys unless `skipUnexpectedFields = false` (as
    * jsoniter-scala does). When `alreadyOpened`, a sealed trait decoder consumed the object start already.
    */
  @scala.annotation.nowarn("msg=is never used")
  private def decodeObjectWithoutFields[A: DecoderCtx](
      caseClass: ProductType[A],
      fieldsList: List[(String, Parameter)],
      alreadyOpened: Boolean
  ): MIO[Expr[A]] = {
    implicit val BooleanT: Type[Boolean] = CTypes.Boolean
    val constructedMIO: MIO[Expr[A]] =
      foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
        onTypes = _ => Map.empty,
        onValues = _ => buildTransientDefaults[A](fieldsList, Map.empty)
      ) match {
        case Right(constructExpr) => MIO.pure(constructExpr.value.asInstanceOf[Expr[A]])
        case Left(error)          =>
          val err = CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
          Log.error(err.message) >> MIO.fail(err)
      }
    constructedMIO.map { constructed =>
      val skipUnexpected: Expr[Boolean] = dctx.evaluatedConfig match {
        case Some(c) => Expr(c.skipUnexpectedFields)
        case None    => Expr.quote(Expr.splice(dctx.config).skipUnexpectedFields)
      }
      Expr.quote {
        UBJsonDerivationUtils.readObjectWithoutFields(
          Expr.splice(dctx.reader),
          Expr.splice(skipUnexpected),
          Expr.splice(Expr(alreadyOpened))
        )
        Expr.splice(constructed)
      }
    }
  }

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        ProductType.parse[A] match {
          case Right(caseClass) =>
            // Note: caching is handled by deriveDecoderRecursively
            decodeCaseClassFields[A](caseClass).map(Rule.matched)

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFields[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

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
          decodeObjectWithoutFields[A](caseClass, fieldsList, alreadyOpened = false)

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { rawDecodeFn =>
                  val decodeFn = withFieldDefault[Field](param)(rawDecodeFn)
                  val decodeFnErased: Expr[UBJsonReader => Any] = erasedFieldDecodeFn[A, Field](param, decodeFn)
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
                  // Absent fields: default / None / empty collection, or `missing required field`
                  // (see DecoderAbsentFieldSupport). The transient* flags only affect encoding.
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    fieldDataList.map(_._4(decodedValuesExpr)).toMap
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults[A](fieldsList, nonTransientFieldMap)
                  val absentFieldFillAll: Expr[Unit] =
                    buildAbsentFieldFillExprs[A](nonTransientWithIndex, decodedValuesExpr, fieldMap)
                      .foldLeft(Expr.quote(()): Expr[Unit]) { (acc, step) =>
                        Expr.quote {
                          Expr.splice(acc)
                          Expr.splice(step)
                        }
                      }
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

                  // Pre-compute mapped field names when config is statically known
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    val mappedName: Option[String] = nameOverride.orElse(
                      dctx.evaluatedConfig.map(_.fieldNameMapper(name))
                    )
                    (name, index, decodeFnErased, mappedName)
                  }

                  val skipCheckFieldDuplication = dctx.evaluatedConfig.exists(!_.checkFieldDuplication)
                  val skipUnexpectedStatically = dctx.evaluatedConfig.map(_.skipUnexpectedFields)

                  if (skipCheckFieldDuplication) {
                    // Optimized: no _seen array needed
                    Expr.quote {
                      UBJsonDerivationUtils.readObject[A](
                        Expr.splice(dctx.reader),
                        Expr.splice(Expr(fieldMappings.size)),
                        Expr.splice(constructLambda)
                      ) { case (fieldName, arr, reader) =>
                        Expr.splice {
                          val fallback: Expr[Unit] = skipUnexpectedStatically match {
                            case Some(true) =>
                              Expr.quote(reader.skip())
                            case Some(false) =>
                              Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                            case None =>
                              Expr.quote {
                                if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                                else reader.decodeError("unexpected field: " + fieldName)
                              }
                          }
                          fieldMappings.foldRight(fallback) {
                            case ((name, index, decodeFnErased, Some(resolvedName)), elseExpr) =>
                              Expr.quote {
                                if (fieldName == Expr.splice(Expr(resolvedName))) {
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
                  } else {
                    // Full path with _seen array
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
                          val fallback: Expr[Unit] = skipUnexpectedStatically match {
                            case Some(true) =>
                              Expr.quote(reader.skip())
                            case Some(false) =>
                              Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                            case None =>
                              Expr.quote {
                                if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                                else reader.decodeError("unexpected field: " + fieldName)
                              }
                          }
                          fieldMappings.foldRight(fallback) {
                            case ((name, index, decodeFnErased, Some(resolvedName)), elseExpr) =>
                              Expr.quote {
                                if (fieldName == Expr.splice(Expr(resolvedName))) {
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
    }

    /** Decode fields from an already-opened object (for discriminator mode). */
    @scala.annotation.nowarn("msg=is never used|unused explicit parameter|Non local returns")
    private[compiletime] def decodeCaseClassFieldsInline[A: DecoderCtx](
        caseClass: ProductType[A]
    ): MIO[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader
      implicit val fieldNameT: Type[fieldNameAnn] = CTypes.FieldName
      implicit val transientFieldT: Type[transientField] = CTypes.TransientField

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
          decodeObjectWithoutFields[A](caseClass, fieldsList, alreadyOpened = true)

        case Some(fields) =>
          implicit val AnyT: Type[Any] = CTypes.Any
          implicit val ArrayAnyT: Type[Array[Any]] = CTypes.ArrayAny

          val nonTransientWithIndex = nonTransientFields.zipWithIndex

          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val nameOverride = fieldNameOverride(param)
              val arrayIndex = nonTransientWithIndex.find(_._1._1 == fName).map(_._2).getOrElse(param.index)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { rawDecodeFn =>
                  val decodeFn = withFieldDefault[Field](param)(rawDecodeFn)
                  val decodeFnErased: Expr[UBJsonReader => Any] = erasedFieldDecodeFn[A, Field](param, decodeFn)
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
                  val fieldMap = nonTransientFieldMap ++ buildTransientDefaults[A](fieldsList, nonTransientFieldMap)
                  val absentFieldFills: List[Expr[Unit]] =
                    buildAbsentFieldFillExprs[A](nonTransientWithIndex, decodedValuesExpr, fieldMap)
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fieldMap
                  ) match {
                    case Right(constructExpr) =>
                      MIO.pure(absentFieldFills.foldRight(constructExpr.value.asInstanceOf[Expr[A]]) { (fill, rest) =>
                        Expr.quote {
                          Expr.splice(fill)
                          Expr.splice(rest)
                        }
                      })
                    case Left(error) =>
                      val err =
                        CodecDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]

                  // Pre-compute mapped field names when config is statically known
                  val fieldMappings = fieldDataList.map { case (name, index, decodeFnErased, _, nameOverride) =>
                    val mappedName: Option[String] = nameOverride.orElse(
                      dctx.evaluatedConfig.map(_.fieldNameMapper(name))
                    )
                    (name, index, decodeFnErased, mappedName)
                  }

                  val skipUnexpectedStatically = dctx.evaluatedConfig.map(_.skipUnexpectedFields)

                  Expr.quote {
                    UBJsonDerivationUtils.readObjectInline[A](
                      Expr.splice(dctx.reader),
                      Expr.splice(Expr(fieldMappings.size)),
                      Expr.splice(constructLambda)
                    ) { case (fieldName, arr, reader) =>
                      Expr.splice {
                        val fallback: Expr[Unit] = skipUnexpectedStatically match {
                          case Some(true) =>
                            Expr.quote(reader.skip())
                          case Some(false) =>
                            Expr.quote(reader.decodeError("unexpected field: " + fieldName))
                          case None =>
                            Expr.quote {
                              if (Expr.splice(dctx.config).skipUnexpectedFields) reader.skip()
                              else reader.decodeError("unexpected field: " + fieldName)
                            }
                        }
                        fieldMappings.foldRight(fallback) {
                          case ((name, index, decodeFnErased, Some(resolvedName)), elseExpr) =>
                            Expr.quote {
                              if (fieldName == Expr.splice(Expr(resolvedName))) {
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

    // `T | Null` fields get their own cache key (`String | Null` =:= `String` would share the decoder of `String`)
    val nullable = nullableUnionUnderlying[Field].isDefined
    val cacheKey = if (nullable) "nullable-field-decode-method" else "field-decode-method"
    // 1. Check if already cached via setHelper (case class/enum rules)
    (if (nullable) MIO.pure(None) else ctx.getHelper[Field]).flatMap {
      case Some(helperCall) =>
        Log.info(s"Found cached decoder helper for field ${Type[Field].prettyPrint}") >>
          MIO.pure((reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config))
      case None =>
        // 2. Check if cached via previous deriveFieldDecoder call
        ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field](cacheKey).flatMap {
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
              _ <- ctx.cache.forwardDeclare(cacheKey, defBuilder)
              builtBuilder <- defBuilder.traverse { case (_, (reader, config)) =>
                val nestedCtx =
                  DecoderCtx.from[Field](reader, config, ctx.cache, ctx.derivedType, ctx.evaluatedConfig)
                deriveDecoderRecursively[Field](using nestedCtx)
              }
              currentCache <- ctx.cache.get
              _ <- ctx.cache.set(builtBuilder.buildCached(currentCache, cacheKey))
              fn <- ctx.cache.get2Ary[UBJsonReader, UBJsonConfig, Field](cacheKey)
            } yield {
              val helperCall = fn.get
              (reader: Expr[UBJsonReader]) => helperCall(reader, ctx.config)
            }
        }
    }
  }

}
