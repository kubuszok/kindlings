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
          // Wrap in shrinkLazy to break infinite recursion for recursive types (e.g. `List[TreeNode]` inside `Branch`):
          // field shrinkers are built eagerly, so a cached def that reaches itself through a field would otherwise
          // call itself while constructing its own instance. Same approach as `CogenUseCachedRule`.
          val directCall = helperCall(Expr.quote(()))
          MIO.pure(Rule.matched(Expr.quote {
            hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils.shrinkLazy[A](Expr.splice(directCall))
          }))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Shrink for ${Type[A].prettyPrint}"))
      }
  }
}
