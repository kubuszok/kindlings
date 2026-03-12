package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FastShowPrettyHandleAsSingletonRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsSingletonRule extends DerivationRule("handle as singleton when possible") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
        SingletonValue.parse[A].toEither match {
          case Right(_) =>
            val name = Expr(Type[A].shortName)
            MIO.pure(Rule.matched(Expr.quote {
              Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
            }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(reason))
        }
      }
  }
}
