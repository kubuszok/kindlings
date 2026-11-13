package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.fp.instances.*
import hearth.std.*

import io.circe.Json

trait EncoderHandleAsTupleRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Tuples encode as JSON arrays, like circe's `Encoder.encodeTupleN` (used directly when every element has an
    * implicit Encoder; this rule covers tuples containing derived types).
    */
  object EncoderHandleAsTupleRule extends EncoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      if (!Type[A].isTuple || Type[A].isNamedTuple)
        MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
      else
        CaseClass.parse[A].toEither match {
          case Right(caseClass) =>
            implicit val JsonT: Type[Json] = Types.Json
            caseClass
              .caseFieldValuesAt(ectx.value)
              .toList
              .traverse { case (_, fieldValue) =>
                import fieldValue.{Underlying as Field, value as fieldExpr}
                deriveEncoderRecursively[Field](using ectx.nest(fieldExpr))
              }
              .map { elems =>
                val listExpr = elems.foldRight(Expr.quote(List.empty[Json])) { (e, acc) =>
                  Expr.quote(Expr.splice(e) :: Expr.splice(acc))
                }
                Rule.matched(Expr.quote(Json.fromValues(Expr.splice(listExpr))))
              }
          case Left(reason) => MIO.pure(Rule.yielded(reason))
        }
  }
}
