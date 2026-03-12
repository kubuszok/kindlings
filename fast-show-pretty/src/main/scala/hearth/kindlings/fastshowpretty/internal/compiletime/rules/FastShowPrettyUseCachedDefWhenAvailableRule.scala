package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.{FastShowPretty, RenderConfig}

trait FastShowPrettyUseCachedDefWhenAvailableRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyUseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >>
        ctx.getInstance[A].flatMap {
          case Some(instance) => callCachedInstance[A](instance)
          case None           =>
            ctx.getHelper[A].flatMap {
              case Some(helperCall) => callCachedHelper[A](helperCall)
              case None             => yieldUnsupportedType[A]
            }
        }

    private def callCachedInstance[A: DerivationCtx](
        instance: Expr[FastShowPretty[A]]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >> MIO.pure(Rule.matched(Expr.quote {
        Expr
          .splice(instance)
          .render(Expr.splice(ctx.sb), Expr.splice(ctx.config), Expr.splice(ctx.level))(Expr.splice(ctx.value))
      }))

    private def callCachedHelper[A: DerivationCtx](
        helperCall: (Expr[StringBuilder], Expr[RenderConfig], Expr[Int], Expr[A]) => Expr[StringBuilder]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found cached helper call for ${Type[A].prettyPrint}, using it") >> MIO.pure(
        Rule.matched(helperCall(ctx.sb, ctx.config, ctx.level, ctx.value))
      )

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached definition"))

  }
}
