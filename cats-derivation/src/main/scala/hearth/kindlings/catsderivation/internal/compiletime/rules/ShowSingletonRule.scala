package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowSingletonRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  object ShowSingletonRule extends ShowDerivationRule("Show as singleton") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          val name = Type[A].shortName
          MIO.pure(Rule.matched(Expr(s"$name()")))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason.toString))
      }
  }
}
