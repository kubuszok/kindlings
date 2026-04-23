package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkUseCachedRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  object ShrinkUseCachedRule extends ShrinkDerivationRule("use cached Shrink when available") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      shrinkctx.getHelper[A].flatMap {
        case Some(helperCall) =>
          MIO.pure(Rule.matched(helperCall(Expr.quote(()))))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Shrink for ${Type[A].prettyPrint}"))
      }
  }
}
