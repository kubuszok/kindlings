package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsNamedTupleRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsNamedTupleRule extends DecoderDerivationRule("handle as named tuple when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- dctx.setHelper[A] { (node, config) =>
                decodeNamedTupleFields[A](namedTuple.primaryConstructor)(using dctx.nestInCache(node, config))
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
    private def decodeNamedTupleFields[A: DecoderCtx](
        constructor: Method
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherCEAnyT: Type[Either[ConstructError, Any]] = DTypes.EitherCEAny
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[ConstructError, Any]]] = DTypes.ListEitherCEAny

      val fieldsList = constructor.totalParameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          foldInstanceFree(constructor, "Constructor")(
            onTypes = _ => Map.empty,
            onValues = _ => Map.empty
          ) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                YamlDerivationUtils
                  .checkIsMapping(Expr.splice(dctx.node))
                  .map(_ => Expr.splice(constructExpr.value.asInstanceOf[Expr[A]]))
              })
            case Left(error) =>
              val err =
                DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          val indexedFields = fields.toList.zipWithIndex
          val namesMIO: MIO[String => Expr[String]] = dctx.evaluatedConfig match {
            case Some(evConfig) => MIO.pure((name: String) => Expr(evConfig.transformMemberNames(name)))
            case None           => dctx.transformedMemberNames(fields.toList.map(_._1))
          }
          namesMIO.flatMap { nameOf =>
            NonEmptyList
              .fromList(indexedFields)
              .get
              .parTraverse { case ((fName, param), _) =>
                import param.tpe.Underlying as Field
                Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                  deriveFieldDecoder[Field].map { decoderExpr =>
                    val fieldNameExpr = nameOf(fName)
                    def decodeExpr(fields: Expr[java.util.HashMap[String, Node]]): Expr[Either[ConstructError, Any]] =
                      Expr.quote {
                        YamlDerivationUtils.decodeRequiredFieldOf(
                          Expr.splice(fields),
                          Expr.splice(dctx.node),
                          Expr.splice(fieldNameExpr),
                          Expr.splice(decoderExpr)
                        )
                      }
                    val makeFieldArg: Expr[Any] => (String, Expr_??) = { valExpr =>
                      (fName, castTo[Field](valExpr).as_??)
                    }
                    (decodeExpr(_), makeFieldArg, fieldNameExpr)
                  }
                }
              }
              .flatMap { fieldData =>
                val decodeExprs = fieldData.toList.map(_._1)
                val makeFieldArgs = fieldData.toList.map(_._2)
                val fieldNameExprs = fieldData.toList.map(_._3).toVector
                implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
                implicit val FieldNodesT: Type[java.util.HashMap[String, Node]] = DTypes.FieldNodes
                implicit val EitherFieldNodesT: Type[Either[ConstructError, java.util.HashMap[String, Node]]] =
                  DTypes.EitherCEFieldNodes
                implicit val FailuresT: Type[YamlDerivationUtils.FailureAccumulator] = DTypes.FailureAccumulator

                def constructFF(boundValues: List[Expr[Any]]): Expr[A] = {
                  val fieldMap: Map[String, Expr_??] =
                    makeFieldArgs.zip(boundValues).map { case (mk, v) => mk(v) }.toMap
                  foldInstanceFree(constructor, "Constructor")(
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
                // as a case class: fail fast (the later fields are not decoded) or accumulating (see YamlConfig)
                def construct(fields: Expr[java.util.HashMap[String, Node]]): Expr[Either[ConstructError, A]] =
                  byErrorMode[Either[ConstructError, A]](
                    constructFailFast[Either[ConstructError, Any], A, Either[ConstructError, A]](
                      decodeExprs.map(_(fields))
                    )(
                      isSuccess = r => Expr.quote(Expr.splice(r).isRight),
                      valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConstructError, Any]].value),
                      construct = constructFF,
                      onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConstructError, A]),
                      onFailure = r => Expr.quote(Expr.splice(r).asInstanceOf[Either[ConstructError, A]])
                    ),
                    constructAccumulatingIndexed[
                      Either[ConstructError, Any],
                      YamlDerivationUtils.FailureAccumulator,
                      A,
                      Either[ConstructError, A]
                    ](decodeExprs.map(_(fields)))(
                      mergeErrors = (index, failures, r) =>
                        Expr.quote(
                          YamlDerivationUtils.addFieldFailure(
                            Expr.splice(failures),
                            Expr.splice(fieldNameExprs(index)),
                            Expr.splice(r)
                          )
                        ),
                      valueOf = r => Expr.quote(Expr.splice(r).asInstanceOf[Right[ConstructError, Any]].value),
                      construct = constructFF,
                      onSuccess = a => Expr.quote(Right(Expr.splice(a)): Either[ConstructError, A]),
                      onFailure =
                        failures => Expr.quote(YamlDerivationUtils.accumulatedFailures[A](Expr.splice(failures)))
                    ),
                    ownDefs = true
                  )
                MIO.pure(
                  ValDefs
                    .createVal[Either[ConstructError, java.util.HashMap[String, Node]]](
                      Expr.quote(YamlDerivationUtils.fieldNodes(Expr.splice(dctx.node))),
                      FreshName.FromPrefix("fieldNodes")
                    )
                    .use { fieldNodes =>
                      Expr.quote {
                        if (Expr.splice(fieldNodes).isLeft)
                          Expr.splice(fieldNodes).asInstanceOf[Either[ConstructError, A]]
                        else
                          Expr.splice(
                            ValDefs
                              .createVal[java.util.HashMap[String, Node]](
                                Expr.quote(
                                  Expr
                                    .splice(fieldNodes)
                                    .asInstanceOf[Right[ConstructError, java.util.HashMap[String, Node]]]
                                    .value
                                ),
                                FreshName.FromPrefix("fields")
                              )
                              .use(construct)
                          )
                      }
                    }
                )
              }
          }
      }
    }

    /** `value.asInstanceOf[F]` (a method of its own so that `F` is a type parameter in the quote). */
    private def castTo[F: Type](value: Expr[Any]): Expr[F] = Expr.quote(Expr.splice(value).asInstanceOf[F])
  }
}
