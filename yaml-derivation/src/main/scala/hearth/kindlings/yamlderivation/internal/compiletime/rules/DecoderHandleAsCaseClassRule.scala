package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}
import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsCaseClassRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCaseClassRule extends DecoderDerivationRule("handle as case class when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            for {
              _ <- dctx.setHelper[A] { (node, config) =>
                decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(node, config))
              }
              result <- dctx.getHelper[A].flatMap {
                case Some(helperCall) =>
                  MIO.pure(Rule.matched(helperCall(dctx.node, dctx.config)))
                case None =>
                  MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result

          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used|unused explicit parameter")
    private def decodeCaseClassFields[A: DecoderCtx](
        caseClass: CaseClass[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val fieldNameT: Type[fieldName] = DTypes.FieldName
      implicit val transientFieldT: Type[transientField] = DTypes.TransientField

      val constructor = caseClass.primaryConstructor
      val fieldsList = constructor.parameters.flatten.toList

      fieldsList.collectFirst {
        case (name, param) if hasAnnotationType[transientField](param) && !param.hasDefault => name
      } match {
        case Some(name) =>
          val err = DecoderDerivationError.TransientFieldMissingDefault(name, Type[A].prettyPrint)
          return Log.error(err.message) >> MIO.fail(err)
        case None => // OK
      }

      val transientFields = fieldsList.filter { case (_, param) => hasAnnotationType[transientField](param) }
      val nonTransientFields = fieldsList.filter { case (_, param) => !hasAnnotationType[transientField](param) }

      val transientDefaults: Map[String, Expr_??] = transientFields.flatMap { case (fName, param) =>
        param.defaultValue.flatMap { existentialOuter =>
          val methodOf = existentialOuter.value
          methodOf.value match {
            case noInstance: Method.NoInstance[?] =>
              import noInstance.Returned
              noInstance(Map.empty).toOption.map { defaultExpr =>
                (fName, defaultExpr.as_??)
              }
            case _ => None
          }
        }
      }.toMap

      NonEmptyList.fromList(nonTransientFields) match {
        case None =>
          caseClass
            .construct[MIO](new CaseClass.ConstructField[MIO] {
              def apply(field: Parameter): MIO[Expr[field.tpe.Underlying]] =
                transientDefaults.get(field.name) match {
                  case Some(defaultExpr) =>
                    MIO.pure(defaultExpr.value.asInstanceOf[Expr[field.tpe.Underlying]])
                  case None =>
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
                MIO.pure(Expr.quote {
                  YamlDerivationUtils.checkIsMapping(Expr.splice(dctx.node)).map(_ => Expr.splice(expr))
                })
              case None =>
                val err = DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false)
                Log.error(err.message) >> MIO.fail(err)
            }

        case Some(fields) =>
          implicit val AnyT: Type[Any] = DTypes.Any
          implicit val EitherCEAnyT: Type[Either[ConstructError, Any]] = DTypes.EitherCEAny
          implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
          implicit val ListEitherT: Type[List[Either[ConstructError, Any]]] = DTypes.ListEitherCEAny

          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              val nameOverride = getAnnotationStringArg[fieldName](param)
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
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[ConstructError, Any]] = nameOverride match {
                    case Some(customName) =>
                      defaultAsAnyOpt match {
                        case Some(defaultAnyExpr) =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            if (config.useDefaults)
                              YamlDerivationUtils.decodeFieldWithDefault(
                                Expr.splice(dctx.node),
                                Expr.splice(Expr(customName)),
                                Expr.splice(decoderExpr),
                                Expr.splice(defaultAnyExpr)
                              )
                            else
                              YamlDerivationUtils
                                .getField(
                                  Expr.splice(dctx.node),
                                  Expr.splice(Expr(customName))
                                )
                                .flatMap(fieldNode =>
                                  Expr
                                    .splice(decoderExpr)
                                    .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                                )
                                .asInstanceOf[Either[ConstructError, Any]]
                          }
                        case None =>
                          Expr.quote {
                            YamlDerivationUtils
                              .getField(
                                Expr.splice(dctx.node),
                                Expr.splice(Expr(customName))
                              )
                              .flatMap(fieldNode =>
                                Expr
                                  .splice(decoderExpr)
                                  .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                              )
                              .asInstanceOf[Either[ConstructError, Any]]
                          }
                      }
                    case None =>
                      defaultAsAnyOpt match {
                        case Some(defaultAnyExpr) =>
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            val fn = config.transformMemberNames(Expr.splice(Expr(fName)))
                            if (config.useDefaults)
                              YamlDerivationUtils.decodeFieldWithDefault(
                                Expr.splice(dctx.node),
                                fn,
                                Expr.splice(decoderExpr),
                                Expr.splice(defaultAnyExpr)
                              )
                            else
                              YamlDerivationUtils
                                .getField(
                                  Expr.splice(dctx.node),
                                  fn
                                )
                                .flatMap(fieldNode =>
                                  Expr
                                    .splice(decoderExpr)
                                    .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                                )
                                .asInstanceOf[Either[ConstructError, Any]]
                          }
                        case None =>
                          Expr.quote {
                            YamlDerivationUtils
                              .getField(
                                Expr.splice(dctx.node),
                                Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                              )
                              .flatMap(fieldNode =>
                                Expr
                                  .splice(decoderExpr)
                                  .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                              )
                              .asInstanceOf[Either[ConstructError, Any]]
                          }
                      }
                  }
                  val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
                    val typedExpr = Expr.quote {
                      YamlDerivationUtils.unsafeCast(
                        Expr.splice(arrExpr)(Expr.splice(Expr(reindex))),
                        Expr.splice(decoderExpr)
                      )
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeAccessor)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeAccessors = fieldData.toList.map(_._2)

              val listExpr: Expr[List[Either[ConstructError, Any]]] =
                decodeExprs.foldRight(Expr.quote(List.empty[Either[ConstructError, Any]])) { (elem, acc) =>
                  Expr.quote(Expr.splice(elem) :: Expr.splice(acc))
                }

              LambdaBuilder
                .of1[Array[Any]]("decodedValues")
                .traverse { decodedValuesExpr =>
                  val nonTransientFieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
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
                  Expr.quote {
                    YamlDerivationUtils
                      .sequenceDecodeResults(Expr.splice(listExpr))
                      .map(Expr.splice(constructLambda))
                  }
                }
            }
      }
    }
  }
}
