package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenBuiltInRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object CogenBuiltInRule extends CogenDerivationRule("use ScalaCheck built-in Cogen") {

    /** ScalaCheck's combinators that Kindlings replaces with structural rules recursing through derivation (collection,
      * Option, enum for `Either`/`Try`, case class for tuples). Every other ScalaCheck instance - all the leaf types
      * (primitives, `String`, `UUID`, `java.time.*`, durations, throwables, Java enums, ...) and the combinators
      * without a structural counterpart (functions, `PartialFunction`) - is used as is. No per-type whitelist, so this
      * supports exactly what the used ScalaCheck version supports, on every platform.
      *
      * `cogenSet`/`cogenSortedSet`/`cogenMap`/`cogenSortedMap` are deliberately kept: they sort the elements first, so
      * equal sets/maps perturb the seed identically regardless of iteration order. The structural collection rule
      * perturbs in iteration order, so it only takes over when those cannot be used (e.g. no `Cogen` for the element).
      */
    lazy val structurallyReplaced: Seq[UntypedMethod] = {
      val names = Set(
        "cogenOption",
        "cogenEither",
        "cogenTry",
        "cogenList",
        "cogenVector",
        "cogenStream",
        "cogenLazyList",
        "cogenSeq",
        "cogenArray",
        "cogenFunction0",
        "cogenPartialFunction"
      )
      Type.of[Cogen.type].unsortedMethods.collect {
        case method if method.isImplicit && (names(method.name) || method.name.matches("(tuple|function)\\d+")) =>
          method.asUntyped
      }
    }

    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      // For the type being derived, a non-ScalaCheck instance may be the very definition in progress (a `derives`
      // given, or `implicit val x: Cogen[A] = Cogen.derived[A]`), so ScalaCheck's instance is only used when no other
      // instance exists - then the summoned one is necessarily built from ScalaCheck's own instances. This keeps e.g.
      // `Cogen.derived[String]` (forced by `derives` on generic types) on ScalaCheck's instance.
      if (
        cogenctx.derivedType.exists(_.Underlying =:= Type[A]) &&
        CogenTypes.Cogen[A].summonExprIgnoring(CogenUseImplicitRule.ignoredImplicits*).toEither.isRight
      )
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type with a non-ScalaCheck instance in scope"))
      else
        CogenTypes.Cogen[A].summonExprIgnoring(structurallyReplaced*).toEither match {
          case Right(cogenExpr) =>
            Log.info(s"Using ScalaCheck built-in Cogen for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(cogenExpr))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No built-in Cogen[${Type[A].prettyPrint}]: $reason"))
        }
  }
}
