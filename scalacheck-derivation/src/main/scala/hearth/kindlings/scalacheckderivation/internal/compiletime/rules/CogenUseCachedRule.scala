package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenUseCachedRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  object CogenUseCachedRule extends CogenDerivationRule("use cached Cogen when available") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      cogenctx.getHelper[A].flatMap {
        case Some(helperCall) =>
          MIO.pure(Rule.matched(helperCall(Expr.quote(()))))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Cogen for ${Type[A].prettyPrint}"))
      }
  }
}
