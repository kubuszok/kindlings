package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

@scala.annotation.nowarn("msg=is never used")
trait OrderValueTypeRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderValueTypeRule extends OrderDerivationRule("Order as value type") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          deriveOrderRecursively[Inner](using
            octx.nest(isValueType.value.unwrap(octx.x), isValueType.value.unwrap(octx.y))
          ).map(Rule.matched(_))
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
      }
  }
}
