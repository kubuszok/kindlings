package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.fp.instances.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.{Decoder, DecodingFailure}

trait DecoderHandleAsTupleRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples decode from JSON arrays of the exact arity, like circe's `Decoder.decodeTupleN` (used directly when every
    * element has an implicit Decoder; this rule covers tuples containing derived types).
    */
  object DecoderHandleAsTupleRule extends DecoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      if (!Type[A].isTuple || Type[A].isNamedTuple)
        MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
      else
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            implicit val AnyT: Type[Any] = DTypes.Any
            implicit val ArrayAnyT: Type[Array[Any]] = DTypes.ArrayAny
            implicit val ListDecoderAnyT: Type[List[Decoder[Any]]] = DTypes.ListDecoderAny
            implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
            val params = caseClass.primaryConstructor.totalParameters.flatten.toList
            params
              .traverse { case (name, param) =>
                import param.tpe.Underlying as Field
                implicit val DecoderField: Type[Decoder[Field]] = DTypes.Decoder[Field]
                deriveFieldDecoder[Field].map { decoder =>
                  val asAny: Expr[Decoder[Any]] = Expr.quote(Expr.splice(decoder).asInstanceOf[Decoder[Any]])
                  val access: Expr[Array[Any]] => (String, Expr_??) = arr =>
                    (
                      name,
                      Expr
                        .quote(
                          CirceDerivationUtils
                            .unsafeCast(Expr.splice(arr)(Expr.splice(Expr(param.index))), Expr.splice(decoder))
                        )
                        .as_??
                    )
                  (asAny, access)
                }
              }
              .map { fields =>
                val decodersExpr = fields.foldRight(Expr.quote(List.empty[Decoder[Any]])) { case ((d, _), acc) =>
                  Expr.quote(Expr.splice(d) :: Expr.splice(acc))
                }
                def construct(arr: Expr[Array[Any]]): Expr[A] =
                  foldInstanceFree(caseClass.primaryConstructor, "Constructor")(
                    onTypes = _ => Map.empty,
                    onValues = _ => fields.map(_._2(arr)).toMap
                  ) match {
                    case Right(constructExpr) => constructExpr.value.asInstanceOf[Expr[A]]
                    case Left(error)          =>
                      Environment.reportErrorAndAbort(
                        DecoderDerivationError
                          .CannotConstructType(Type[A].prettyPrint, isSingleton = false, Some(error.toString))
                          .message
                      )
                  }
                Rule.matched(Expr.quote {
                  CirceDerivationUtils.unsafeResult[Either[DecodingFailure, A]](
                    CirceDerivationUtils.mapResult(
                      Expr.splice(dctx.failFast),
                      CirceDerivationUtils.decodeTupleElements(
                        Expr.splice(dctx.cursor),
                        Expr.splice(dctx.failFast),
                        Expr.splice(decodersExpr)
                      ),
                      (arr: Array[Any]) => {
                        val _ = arr
                        Expr.splice(construct(Expr.quote(arr)))
                      }
                    )
                  )
                })
              }
          case Left(reason) => MIO.pure(Rule.yielded(reason))
        }
  }
}
