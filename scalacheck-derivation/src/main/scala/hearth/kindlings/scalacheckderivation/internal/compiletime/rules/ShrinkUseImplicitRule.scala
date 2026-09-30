package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkUseImplicitRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn
  object ShrinkUseImplicitRule extends ShrinkDerivationRule("use implicit Shrink when available") {

    /** ScalaCheck's universal no-op `Shrink.shrinkAny` (from `ShrinkLowPriority`) matches every type, so summoning
      * without ignoring it would hide every structural rule for nested types (issue kubuszok/kindlings#219). It is
      * still used - by [[ShrinkFallbackToImplicitRule]] - for types no structural rule can handle. A user-defined
      * `implicit val noShrink: Shrink[X] = Shrink.shrinkAny[X]` is a different symbol, so it still wins here.
      *
      * ScalaCheck's generic combinators are ignored too: the exclusion only applies to the summoned type itself, so
      * e.g. `Shrink[Vector[Inner]]` would otherwise resolve to `shrinkContainer(shrinkAny[Inner])` and never shrink
      * `Inner`. Each of them has a structural counterpart (collection, map, Option, enum, case class rules) that
      * recurses through derivation instead. Non-generic built-ins (`shrinkIntegral`, `shrinkString`, ...) are kept.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] = {
      val genericCombinators = Set("shrinkAny", "shrinkContainer", "shrinkContainer2", "shrinkOption", "shrinkEither")
      Type.of[Shrink.type].unsortedMethods.collect {
        case method if genericCombinators(method.name) || method.name.startsWith("shrinkTuple") => method.asUntyped
      }
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
