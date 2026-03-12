package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.circederivation.annotations.{fieldName, transientField}
import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import cats.data.{Validated, ValidatedNel}
import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (cursor, config, failFast) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(cursor, config, failFast))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
                  MIO.pure(Rule.matched(Expr.quote {
                    Expr
                      .splice(helperCall(dctx.cursor, dctx.config, dctx.failFast))
                      .asInstanceOf[Either[DecodingFailure, A]]
                  }))
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
    ): MIO[Expr[Any]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val HCursorT: Type[HCursor] = DTypes.HCursor
      implicit val DecodingFailureT: Type[DecodingFailure] = DTypes.DecodingFailure
      implicit val UnitT: Type[Unit] = DTypes.Unit
      implicit val EitherDFUnitT: Type[Either[DecodingFailure, Unit]] = DTypes.EitherDFUnit
      implicit val SetStringT: Type[Set[String]] = DTypes.SetString
      implicit val ConfigT: Type[Configuration] = DTypes.Configuration
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      // Validate: @transientField on fields without defaults is a compile error
      fieldsList
        .collectFirst {
          case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
        }
        .foreach { name =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        }

      // Build a List[String] expression of the field names (accounting for @fieldName overrides)
      // for strict decoding — only non-transient fields
      val fieldNamesListExpr: Expr[List[String]] =
        fieldsList
          .filterNot { case (_, param) => hasAnnotationType[transientField](param) }
          .map { case (name, param) =>
            getAnnotationStringArg[fieldName](param).getOrElse(name)
          }
          .foldRight(Expr.quote(List.empty[String])) { (name, acc) =>
            Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
          }

      // For strict decoding with @fieldName, the names are already resolved at compile time
      // so we need a version that doesn't apply config.transformMemberNames for those
      val hasAnyFieldNameAnnotation = fieldsList.exists { case (_, param) =>
        getAnnotationStringArg[fieldName](param).isDefined
      }

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          // Zero-parameter case class: construct directly, but check strictDecoding
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] = {
                val err = DecoderDerivationError.CannotConstructType(
                  Type[A].prettyPrint,
                  isSingleton = false,
                  Some(s"Unexpected parameter in zero-argument case class")
                )
                Log.error(err.message) >> MIO.fail(err)
              }
            })
            .flatMap {
              case Some(expr) =>
                implicit val ValidatedNelDFA: Type[ValidatedNel[DecodingFailure, A]] = DTypes.ValidatedNelDF[A]
                implicit val ValidatedNelDFUnitT: Type[ValidatedNel[DecodingFailure, Unit]] =
                  DTypes.ValidatedNelDFUnit
                MIO.pure(Expr.quote {
                  (if (Expr.splice(dctx.failFast)) {
                     CirceDerivationUtils.checkIsObject(Expr.splice(dctx.cursor)).flatMap { _ =>
                       val config = Expr.splice(dctx.config)
                       if (config.strictDecoding)
                         CirceDerivationUtils
                           .checkStrictDecoding(Expr.splice(dctx.cursor), Set.empty[String])
                           .map(_ => Expr.splice(expr))
                       else
                         Right(Expr.splice(expr)): Either[DecodingFailure, A]
                     }
                   } else {
                     CirceDerivationUtils
                       .checkIsObjectAccumulating(Expr.splice(dctx.cursor))
                       .andThen { _ =>
                         if (Expr.splice(dctx.config).strictDecoding)
                           CirceDerivationUtils
                             .checkStrictDecodingAccumulating(Expr.splice(dctx.cursor), Set.empty[String])
                             .map(_ => Expr.splice(expr))
                         else
                           Validated.valid(Expr.splice(expr)): ValidatedNel[DecodingFailure, A]
                       }
                   }): Any
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherDFAnyT: Type[Either[DecodingFailure, Any]] = DTypes.EitherDFAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[DecodingFailure, Any]]] = DTypes.ListEitherDFAny
          implicit val ValidatedNelDFAnyT: Type[ValidatedNel[DecodingFailure, Any]] = DTypes.ValidatedNelDFAny
          implicit val ListValidatedNelT: Type[List[ValidatedNel[DecodingFailure, Any]]] = DTypes.ListValidatedNelDFAny
          implicit val ValidatedNelDFArrayAnyT: Type[ValidatedNel[DecodingFailure, Array[Any]]] =
            DTypes.ValidatedNelDFArrayAny

          // Step 1: For each field, derive a decoder and build BOTH fail-fast and accumulating
          // decode expressions, plus accessor. The failFast branching is done at the wrapping level.
          fields
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val isTransient = hasAnnotationType[transientField](param)
              val nameOverride = getAnnotationStringArg[fieldName](param)
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val defaultAsAnyOpt: Option[Expr[Any]] =
                    if (param.hasDefault)
                      param.defaultValue.flatMap { existentialOuter =>
                        val methodOf = existentialOuter.value
                        methodOf.value match {
                          case noInstance: Method.NoInstance[?] =>
                            import noInstance.Returned
                            noInstance(Map.empty).toOption.map(_.upcast[Any])
                          case _ => None
                        }
                      }
                    else None

                  // --- Fail-fast decode expression ---
                  val ffExpr: Expr[Either[DecodingFailure, Any]] =
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) => Expr.quote(Right(Expr.splice(d)): Either[DecodingFailure, Any])
                        case None    => Expr.quote(Right(null): Either[DecodingFailure, Any])
                      }
                    } else {
                      nameOverride match {
                        case Some(customName) =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(Expr.splice(Expr(customName)))
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(Expr.splice(Expr(customName)))
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                        case None =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                                if (config.useDefaults) {
                                  val field = Expr.splice(dctx.cursor).downField(fn)
                                  if (field.failed)
                                    Right(Expr.splice(defaultAnyExpr)): Either[DecodingFailure, Any]
                                  else
                                    field.as(Expr.splice(decoderExpr)).asInstanceOf[Either[DecodingFailure, Any]]
                                } else
                                  Expr
                                    .splice(dctx.cursor)
                                    .downField(fn)
                                    .as(Expr.splice(decoderExpr))
                                    .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(dctx.cursor)
                                  .downField(Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName))))
                                  .as(Expr.splice(decoderExpr))
                                  .asInstanceOf[Either[DecodingFailure, Any]]
                              }
                          }
                      }
                    }

                  // --- Accumulating decode expression ---
                  val accExpr: Expr[ValidatedNel[DecodingFailure, Any]] =
                    if (isTransient) {
                      defaultAsAnyOpt match {
                        case Some(d) =>
                          Expr.quote(Validated.valid(Expr.splice(d)): ValidatedNel[DecodingFailure, Any])
                        case None =>
                          Expr.quote(Validated.valid(null): ValidatedNel[DecodingFailure, Any])
                      }
                    } else {
                      nameOverride match {
                        case Some(customName) =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                if (config.useDefaults)
                                  CirceDerivationUtils.decodeFieldWithDefaultAccumulating(
                                    Expr.splice(dctx.cursor),
                                    Expr.splice(Expr(customName)),
                                    Expr.splice(decoderExpr),
                                    Expr.splice(defaultAnyExpr)
                                  )
                                else
                                  Expr
                                    .splice(decoderExpr)
                                    .tryDecodeAccumulating(
                                      Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                    )
                                    .map(x => x: Any)
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(decoderExpr)
                                  .tryDecodeAccumulating(
                                    Expr.splice(dctx.cursor).downField(Expr.splice(Expr(customName)))
                                  )
                                  .map(x => x: Any)
                              }
                          }
                        case None =>
                          defaultAsAnyOpt match {
                            case Some(defaultAnyExpr) =>
                              Expr.quote {
                                val config = Expr.splice(dctx.config)
                                val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                                if (config.useDefaults)
                                  CirceDerivationUtils.decodeFieldWithDefaultAccumulating(
                                    Expr.splice(dctx.cursor),
                                    fn,
                                    Expr.splice(decoderExpr),
                                    Expr.splice(defaultAnyExpr)
                                  )
                                else
                                  Expr
                                    .splice(decoderExpr)
                                    .tryDecodeAccumulating(Expr.splice(dctx.cursor).downField(fn))
                                    .map(x => x: Any)
                              }
                            case None =>
                              Expr.quote {
                                Expr
                                  .splice(decoderExpr)
                                  .tryDecodeAccumulating(
                                    Expr
                                      .splice(dctx.cursor)
                                      .downField(
                                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                                      )
                                  )
                                  .map(x => x: Any)
                              }
                          }
                      }
                    }

                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      CirceDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (ffExpr, accExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val ffExprs = fieldData.toList.map(_._1)
              val accExprs = fieldData.toList.map(_._2)
              val makeAccessors = fieldData.toList.map(_._3)

              // Step 2: Build List literals for both paths
              val ffListExpr: Expr[List[Either[DecodingFailure, Any]]] =
                ffExprs.foldRight(Expr.quote(List.empty[Either[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }
              val accListExpr: Expr[List[ValidatedNel[DecodingFailure, Any]]] =
                accExprs.foldRight(Expr.quote(List.empty[ValidatedNel[DecodingFailure, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              // Step 3: Build the constructor lambda using LambdaBuilder + primaryConstructor
              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  caseClass.primaryConstructor(fieldMap) match {
                    case Right(constructExpr) => MIO.pure(constructExpr)
                    case Left(error)          =>
                      val err = DecoderDerivationError.CannotConstructType(
                        Type[A].prettyPrint,
                        isSingleton = false,
                        Some(error)
                      )
                      Log.error(err.message) >> MIO.fail(err)
                  }
                }
                .map { builder =>
                  val constructLambda = builder.build[A]
                  // Step 4: Wrap with if(failFast) branching and strictDecoding check
                  val resolvedFieldNames: Option[Expr[Set[String]]] =
                    if (hasAnyFieldNameAnnotation) {
                      val nameExprs = fieldsList
                        .filterNot { case (_, p) => hasAnnotationType[transientField](p) }
                        .map { case (name, p) =>
                          getAnnotationStringArg[fieldName](p) match {
                            case Some(custom) => (custom, true)
                            case None         => (name, false)
                          }
                        }
                      val resolvedList = nameExprs.foldRight(Expr.quote(List.empty[String])) {
                        case ((name, true), acc) =>
                          Expr.quote(Expr.splice(Expr(name)) :: Expr.splice(acc))
                        case ((name, false), acc) =>
                          Expr.quote {
                            Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(name))) ::
                              Expr.splice(acc)
                          }
                      }
                      Some(Expr.quote(Expr.splice(resolvedList).toSet))
                    } else None

                  Expr.quote {
                    val config = Expr.splice(dctx.config)
                    (if (Expr.splice(dctx.failFast)) {
                       // --- Fail-fast path ---
                       val decoded = CirceDerivationUtils
                         .sequenceDecodeResults(Expr.splice(ffListExpr))
                         .map(Expr.splice(constructLambda))
                       if (config.strictDecoding) {
                         val expectedFields = Expr.splice(
                           resolvedFieldNames.getOrElse(
                             Expr.quote {
                               Expr.splice(fieldNamesListExpr).map(config.transformMemberNames).toSet
                             }
                           )
                         )
                         CirceDerivationUtils
                           .checkStrictDecoding(Expr.splice(dctx.cursor), expectedFields)
                           .flatMap(_ => decoded)
                       } else decoded
                     } else {
                       // --- Accumulating path ---
                       val decoded = CirceDerivationUtils
                         .sequenceDecodeResultsAccumulating(Expr.splice(accListExpr))
                         .map(Expr.splice(constructLambda))
                       if (config.strictDecoding) {
                         val expectedFields = Expr.splice(
                           resolvedFieldNames.getOrElse(
                             Expr.quote {
                               Expr.splice(fieldNamesListExpr).map(config.transformMemberNames).toSet
                             }
                           )
                         )
                         CirceDerivationUtils
                           .checkStrictDecodingAccumulating(Expr.splice(dctx.cursor), expectedFields)
                           .andThen(_ => decoded)
                       } else decoded
                     }): Any
                  }
                }
            }
      }
    }

  }
}
