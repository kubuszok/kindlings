package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkFallbackToImplicitRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  /** Terminal rule (issue kubuszok/kindlings#219): a nested type that no structural rule can handle still gets whatever
    * a plain implicit search finds - usually ScalaCheck's no-op `Shrink.shrinkAny` - so ignoring `shrinkAny` in
    * [[ShrinkUseImplicitRule]] only gives structural derivation priority over it: types that no rule can derive keep
    * compiling to the same no-op shrinker as before.
    */
  object ShrinkFallbackToImplicitRule extends ShrinkDerivationRule("fall back to any implicit Shrink") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      if (shrinkctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        ShrinkTypes.Shrink[A].summonExpr.toEither match {
          case Right(implicitShrink) =>
            Log.info(s"Falling back to implicit Shrink[${Type[A].prettyPrint}]") >>
              MIO.pure(Rule.matched(implicitShrink))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Shrink[${Type[A].prettyPrint}]: $reason"))
        }
  }
}
