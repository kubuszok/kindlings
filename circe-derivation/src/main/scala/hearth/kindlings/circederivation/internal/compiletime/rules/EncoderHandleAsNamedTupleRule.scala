package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.data.NonEmptyList
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsNamedTupleRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsNamedTupleRule extends EncoderDerivationRule("handle as named tuple when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a named tuple") >> {
        NamedTuple.parse[A].toEither match {
          case Right(namedTuple) =>
            for {
              _ <- ectx.setHelper[A] { (value, config) =>
                encodeNamedTupleFields[A](namedTuple.primaryConstructor)(using ectx.nestInCache(value, config))
              }
              result <- ectx.getHelper[A].flatMap {
                case Some(helperCall) => MIO.pure(Rule.matched(helperCall(ectx.value, ectx.config)))
                case None             => MIO.pure(Rule.yielded(s"Failed to build helper for ${Type[A].prettyPrint}"))
              }
            } yield result
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def encodeNamedTupleFields[A: EncoderCtx](
        constructor: Method.NoInstance[A]
    ): MIO[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json
      implicit val StringT: Type[String] = Types.String
      implicit val ProductType: Type[Product] = Types.Product
      implicit val IntType: Type[Int] = Types.Int

      val fields = constructor.parameters.flatten.toList

      NonEmptyList.fromList(fields) match {
        case Some(fieldValues) =>
          fieldValues
            .parTraverse { case (fName, param) =>
              import param.tpe.Underlying as Field
              val fieldExpr: Expr[Field] = Expr.quote {
                Expr
                  .splice(ectx.value)
                  .asInstanceOf[Product]
                  .productElement(Expr.splice(Expr(param.index)))
                  .asInstanceOf[Field]
              }
              Log.namedScope(s"Encoding named tuple field $fName: ${Type[Field].prettyPrint}") {
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr)).map { fieldJson =>
                  (fName, fieldJson)
                }
              }
            }
            .map { fieldPairs =>
              fieldPairs.toList.foldRight(Expr.quote(List.empty[(String, Json)])) { case ((fName, fieldJson), acc) =>
                Expr.quote {
                  (
                    Expr.splice(ectx.config).transformMemberNames(Expr.splice(Expr(fName))),
                    Expr.splice(fieldJson)
                  ) :: Expr.splice(acc)
                }
              }
            }
            .map { fieldsListExpr =>
              Expr.quote {
                CirceDerivationUtils.jsonFromFields(Expr.splice(fieldsListExpr))
              }
            }
        case None =>
          MIO.pure(Expr.quote {
            CirceDerivationUtils.jsonFromFields(Nil)
          })
      }
    }
  }
}
