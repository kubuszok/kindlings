package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.instances.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonWriter

/** Tuples are arrays, as in jsoniter-scala (`(1, "a")` is `[1,"a"]`). */
trait EncoderHandleAsTupleRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsTupleRule extends EncoderDerivationRule("handle as tuple when possible") {

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      if (!Type[A].isTuple || Type[A].isNamedTuple)
        MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a tuple"))
      else
        CaseClass.parse[A].toEither match {
          case Left(reason) => MIO.pure(Rule.yielded(reason))
          case Right(cc)    =>
            implicit val UnitT: Type[Unit] = CTypes.Unit
            implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter
            cc.caseFieldValuesAt(ectx.value)
              .toList
              .parTraverse { case (_, element) =>
                import element.{Underlying as E, value as elementExpr}
                deriveEncoderRecursively[E](using ectx.nest(elementExpr))
              }
              .map { encoded =>
                val body = encoded.foldLeft(Expr.quote(()): Expr[Unit]) { (acc, e) =>
                  Expr.quote {
                    Expr.splice(acc)
                    Expr.splice(e)
                  }
                }
                Rule.matched(Expr.quote {
                  Expr.splice(ectx.writer).writeArrayStart()
                  Expr.splice(body)
                  Expr.splice(ectx.writer).writeArrayEnd()
                })
              }
        }
  }
}
