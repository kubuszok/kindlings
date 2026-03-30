package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryUseCachedRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  object ArbitraryUseCachedRule extends ArbitraryDerivationRule("use cached Arbitrary when available") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      arbctx.getHelper[A].flatMap {
        case Some(helperCall) =>
          MIO.pure(Rule.matched(helperCall(Expr.quote(()))))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Arbitrary for ${Type[A].prettyPrint}"))
      }
  }
}
