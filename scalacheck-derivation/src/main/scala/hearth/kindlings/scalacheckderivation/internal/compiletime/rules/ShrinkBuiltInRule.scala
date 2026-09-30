package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkBuiltInRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShrinkBuiltInRule extends ShrinkDerivationRule("use ScalaCheck built-in Shrink") {

    /** The no-op catch-all `shrinkAny` (only used by [[ShrinkFallbackToImplicitRule]], after every structural rule) and
      * ScalaCheck's combinators that Kindlings replaces with structural rules recursing through derivation (collection,
      * map, Option, enum for `Either`, case class for tuples). Every other ScalaCheck instance - the leaf types
      * (`shrinkIntegral`, `shrinkFractional`, `shrinkString`, durations, `java.time.Period`, ...) - is used as is. No
      * per-type whitelist, so this supports exactly what the used ScalaCheck version supports, on every platform.
      */
    lazy val structurallyReplaced: Seq[UntypedMethod] = {
      val names = Set("shrinkAny", "shrinkContainer", "shrinkContainer2", "shrinkOption", "shrinkEither")
      Type.of[Shrink.type].unsortedMethods.collect {
        case method if method.isImplicit && (names(method.name) || method.name.startsWith("shrinkTuple")) =>
          method.asUntyped
      }
    }

    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      // For the type being derived, a non-ScalaCheck instance may be the very definition in progress (a `derives`
      // given, or `implicit val x: Shrink[A] = Shrink.derived[A]`), so ScalaCheck's instance is only used when no other
      // instance exists - then the summoned one is necessarily built from ScalaCheck's own instances. This keeps e.g.
      // `Shrink.derived[String]` (forced by `derives` on generic types) on ScalaCheck's instance.
      if (
        shrinkctx.derivedType.exists(_.Underlying =:= Type[A]) &&
        ShrinkTypes.Shrink[A].summonExprIgnoring(ShrinkUseImplicitRule.ignoredImplicits*).toEither.isRight
      )
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type with a non-ScalaCheck instance in scope"))
      else
        ShrinkTypes.Shrink[A].summonExprIgnoring(structurallyReplaced*).toEither match {
          case Right(shrinkExpr) =>
            Log.info(s"Using ScalaCheck built-in Shrink for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(shrinkExpr))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No built-in Shrink[${Type[A].prettyPrint}]: $reason"))
        }
  }
}
