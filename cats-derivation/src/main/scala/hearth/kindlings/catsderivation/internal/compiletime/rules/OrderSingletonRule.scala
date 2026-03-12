package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait OrderSingletonRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderSingletonRule extends OrderDerivationRule("Order as singleton") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          MIO.pure(Rule.matched(Expr(0)))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
