package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait HashSingletonRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions =>

  object HashSingletonRule extends HashDerivationRule("Hash as singleton") {
    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          MIO.pure(Rule.matched(Expr(Type[A].shortName.hashCode)))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
