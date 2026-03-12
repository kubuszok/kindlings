package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

@scala.annotation.nowarn("msg=is never used")
trait EqValueTypeRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqValueTypeRule extends EqDerivationRule("Eq as value type") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          val unwrappedX = isValueType.value.unwrap(eqctx.x)
          val unwrappedY = isValueType.value.unwrap(eqctx.y)
          deriveEqRecursively[Inner](using eqctx.nest(unwrappedX, unwrappedY)).map(Rule.matched(_))
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
      }
  }
}
