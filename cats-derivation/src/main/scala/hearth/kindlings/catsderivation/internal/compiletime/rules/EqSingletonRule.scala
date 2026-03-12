package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EqSingletonRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqSingletonRule extends EqDerivationRule("Eq as singleton") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          MIO.pure(Rule.matched(Expr(true)))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
