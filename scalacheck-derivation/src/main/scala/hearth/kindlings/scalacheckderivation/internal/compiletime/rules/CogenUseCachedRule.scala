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
          // Wrap in cogenLazy to break infinite recursion for recursive types.
          // Cogen.perturb is strict (unlike Gen which has Gen.lzy), so a cached def
          // that references itself would stack overflow without this lazy wrapper.
          val directCall = helperCall(Expr.quote(()))
          val lazyCogen: Expr[Cogen[A]] = Expr.quote {
            hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenLazy[A](Expr.splice(directCall))
          }
          MIO.pure(Rule.matched(lazyCogen))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Cogen for ${Type[A].prettyPrint}"))
      }
  }
}
