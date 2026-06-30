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
            // Structural derivation: gate on the derivation policy (issue kubuszok/kindlings#85).
            enforceDerivationPolicy[A] >> decodeCaseClassFields[A](caseClass).map(Rule.matched)

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
      val fieldsList = constructor.totalParameters.flatten.toList

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
        param.defaultValue.flatMap { method =>
          foldInstanceFree(method, "Default value")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ).toOption
            .map(expr => (fName, expr))
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
                  param.defaultValue.flatMap { method =>
                    foldInstanceFree(method, "Default value")(
                      onTypes = _ => Map.empty,
                      onValues = _ => Map.empty
                    ).toOption
                      .map { ee => import ee.Underlying; ee.value.upcast[Any] }
                  }
                else None
              Log.namedScope(s"Deriving decoder for field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  // Pre-compute the resolved field name when possible
                  val resolvedFieldName: Either[Expr[String], String] = nameOverride match {
                    case Some(customName) => Right(customName)
                    case None             =>
                      dctx.evaluatedConfig match {
                        case Some(evConfig) => Right(evConfig.transformMemberNames(fName))
                        case None           =>
                          Left(Expr.quote(Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))))
                      }
                  }
                  val fieldNameExpr: Expr[String] = resolvedFieldName match {
                    case Right(s)   => Expr(s)
                    case Left(expr) => expr
                  }

                  // Check useDefaults statically when possible
                  val useDefaultsStaticallyFalse: Boolean =
                    dctx.evaluatedConfig.exists(!_.useDefaults)

                  val decodeExpr: Expr[Either[ConstructError, Any]] = defaultAsAnyOpt match {
                    case Some(defaultAnyExpr) if useDefaultsStaticallyFalse =>
                      // useDefaults is statically false: skip the default branch entirely
                      Expr.quote {
                        YamlDerivationUtils
                          .getField(
                            Expr.splice(dctx.node),
                            Expr.splice(fieldNameExpr)
                          )
                          .flatMap(fieldNode =>
                            Expr
                              .splice(decoderExpr)
                              .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                          )
                          .asInstanceOf[Either[ConstructError, Any]]
                      }
                    case Some(defaultAnyExpr) =>
                      dctx.evaluatedConfig match {
                        case Some(evConfig) if evConfig.useDefaults =>
                          // useDefaults is statically true: always use default path
                          Expr.quote {
                            YamlDerivationUtils.decodeFieldWithDefault(
                              Expr.splice(dctx.node),
                              Expr.splice(fieldNameExpr),
                              Expr.splice(decoderExpr),
                              Expr.splice(defaultAnyExpr)
                            )
                          }
                        case _ =>
                          // useDefaults not statically known: runtime check
                          Expr.quote {
                            val config = Expr.splice(dctx.config)
                            if (config.useDefaults)
                              YamlDerivationUtils.decodeFieldWithDefault(
                                Expr.splice(dctx.node),
                                Expr.splice(fieldNameExpr),
                                Expr.splice(decoderExpr),
                                Expr.splice(defaultAnyExpr)
                              )
                            else
                              YamlDerivationUtils
                                .getField(
                                  Expr.splice(dctx.node),
                                  Expr.splice(fieldNameExpr)
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
                      Expr.quote {
                        YamlDerivationUtils
                          .getField(
                            Expr.splice(dctx.node),
                            Expr.splice(fieldNameExpr)
                          )
                          .flatMap(fieldNode =>
                            Expr
                              .splice(decoderExpr)
                              .construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                          )
                          .asInstanceOf[Either[ConstructError, Any]]
                      }
                  }
                  // Reads the already-bound `Right` value directly (zero-allocation fail-fast construction); no
                  // intermediate `Array[Any]`.
                  val makeFieldArg: Expr[Any] => (String, Expr_??) = { valExpr =>
                    val typedExpr = Expr.quote {
                      YamlDerivationUtils.unsafeCast(Expr.splice(valExpr), Expr.splice(decoderExpr))
                    }
                    (fName, typedExpr.as_??)
                  }
                  (decodeExpr, makeFieldArg)
                }
              }
            }
            .flatMap { fieldData =>
              val decodeExprs = fieldData.toList.map(_._1)
              val makeFieldArgs = fieldData.toList.map(_._2)

              // Zero-allocation fail-fast construction: nested zero-closure `Either` folds short-circuit on the first
              // `Left`, with no intermediate `List`, `Array[Any]`, `sequence`, or closure.
              def constructFF(boundValues: List[Expr[Any]]): Expr[A] = {
                val nonTransientFieldMap: Map[String, Expr_??] =
                  makeFieldArgs.zip(boundValues).map { case (mk, v) => mk(v) }.toMap
                val fieldMap: Map[String, Expr_??] = nonTransientFieldMap ++ transientDefaults
                foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                  onTypes = _ => Map.empty,
                  onValues = _ => fieldMap
                ) match {
                  case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                  case Left(error)          =>
                    Environment.reportErrorAndAbort(
                      DecoderDerivationError
                        .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
                        .message
                    )
                }
              }
              MIO.pure(buildEitherFailFast[ConstructError, A](decodeExprs)(constructFF))
            }
      }
    }
  }
}
