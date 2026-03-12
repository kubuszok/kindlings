package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.FastShowPretty

trait FastShowPrettyUseImplicitWhenAvailableRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyUseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {

    lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
      case method if method.value.name == "derived" => method.value.asUntyped
    }

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
        // Skip implicit search for the self type being derived to prevent self-referential loops
        // (e.g., `implicit val fsp: FastShowPretty[X] = FastShowPretty.derived[X]` would otherwise
        // find `fsp` itself during macro expansion, generating code that calls itself infinitely).
        if (ctx.derivedType.exists(_.Underlying =:= Type[A]))
          MIO.pure(
            Rule.yielded(s"The type ${Type[A].prettyPrint} is the type being derived, skipping implicit search")
          )
        else
          Types.FastShowPretty[A].summonExprIgnoring(ignoredImplicits*).toEither match {
            case Right(instanceExpr) => cacheImplicitAndUseIt[A](instanceExpr)
            case Left(reason)        => yieldUnsupportedType[A](reason)
          }
      }

    private def cacheImplicitAndUseIt[A: DerivationCtx](
        instanceExpr: Expr[FastShowPretty[A]]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Found implicit ${instanceExpr.prettyPrint}, caching it and using a cached value") >>
        ctx.setInstance[A](instanceExpr) >> FastShowPrettyUseCachedDefWhenAvailableRule[A]

    private def yieldUnsupportedType[A: DerivationCtx](reason: String): MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(
        Rule.yielded(
          s"The type ${Type[A].prettyPrint} does not have an implicit FastShowPretty instance: $reason"
        )
      )
  }
}
