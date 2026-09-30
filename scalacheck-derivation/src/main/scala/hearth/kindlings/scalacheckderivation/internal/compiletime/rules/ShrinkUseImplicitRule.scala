package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkUseImplicitRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn
  object ShrinkUseImplicitRule extends ShrinkDerivationRule("use implicit Shrink when available") {

    /** Every implicit ScalaCheck puts into `Shrink`'s implicit scope (its companion and `ShrinkLowPriority`) is ignored
      * here, so only instances the user (or another library) provides take precedence over derivation (issue
      * kubuszok/kindlings#219):
      *   - the universal no-op `shrinkAny` matches every type, hiding every structural rule for nested types,
      *   - the generic combinators (`shrinkContainer`, `shrinkOption`, `shrinkEither`, `shrinkTuple*`, ...) would pass
      *     `shrinkAny` to their elements (the exclusion only applies to the summoned type itself), e.g.
      *     `Shrink[Vector[Inner]]` would resolve to `shrinkContainer(shrinkAny[Inner])` and never shrink `Inner`,
      *   - the leaf instances (`shrinkIntegral`, `shrinkString`, durations, ...) are still used - through
      *     [[ShrinkBuiltInRule]] or [[ShrinkFallbackToImplicitRule]].
      *
      * A user-defined `implicit val noShrink: Shrink[X] = Shrink.shrinkAny[X]` is a different symbol, so it still wins.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[Shrink.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] = {
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

      if (shrinkctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        ShrinkTypes.Shrink[A].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(implicitShrink) =>
            Log.info(s"Found implicit Shrink[${Type[A].prettyPrint}]") >>
              MIO.pure(Rule.matched(implicitShrink))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Shrink[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
