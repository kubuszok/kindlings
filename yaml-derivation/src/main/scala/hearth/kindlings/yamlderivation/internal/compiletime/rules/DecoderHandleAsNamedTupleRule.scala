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
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Either[ConstructError, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val NodeT: Type[Node] = DTypes.Node
      implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
      implicit val AnyT: Type[Any] = DTypes.Any
      implicit val EitherCEAnyT: Type[Either[ConstructError, Any]] = DTypes.EitherCEAny
      implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
      implicit val ListEitherT: Type[List[Either[ConstructError, Any]]] = DTypes.ListEitherCEAny

      val fieldsList = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fieldsList) match {
        case None =>
          constructor(Map.empty) match {
            case Right(constructExpr) =>
              MIO.pure(Expr.quote {
                YamlDerivationUtils.checkIsMapping(Expr.splice(dctx.node)).map(_ => Expr.splice(constructExpr))
              })
            case Left(error) =>
              val err =
                DecoderDerivationError.CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error))
              Log.error(err.message) >> MIO.fail(err)
          }

        case Some(fields) =>
          val indexedFields = fields.toList.zipWithIndex
          NonEmptyList
            .fromList(indexedFields)
            .get
            .parTraverse { case ((fName, param), reindex) =>
              import param.tpe.Underlying as Field
              Log.namedScope(s"Deriving decoder for named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveFieldDecoder[Field].map { decoderExpr =>
                  val decodeExpr: Expr[Either[ConstructError, Any]] = Expr.quote {
                    YamlDerivationUtils
                      .getField(
                        Expr.splice(dctx.node),
                        Expr.splice(dctx.config).transformMemberNames(Expr.splice(Expr(fName)))
                      )
                      .flatMap(fieldNode =>
                        Expr.splice(decoderExpr).construct(fieldNode)(org.virtuslab.yaml.LoadSettings.empty)
                      )
                      .asInstanceOf[Either[ConstructError, Any]]
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
                  val fieldMap: Map[String, Expr_??] =
                    makeAccessors.map(_(decodedValuesExpr)).toMap
                  constructor(fieldMap) match {
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
