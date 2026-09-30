package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryBuiltInRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryBuiltInRule extends ArbitraryDerivationRule("use ScalaCheck built-in Arbitrary") {

    /** ScalaCheck's combinators that Kindlings replaces with structural rules recursing through derivation (collection,
      * map, Option, enum for `Either`/`Try`, case class for tuples). Every other ScalaCheck instance - all the leaf
      * types (primitives, `String`, `UUID`, `java.time.*`, durations, throwables, Java enums, ...) and the combinators
      * without a structural counterpart (functions, `PartialFunction`, `Future`, `Gen`) - is used as is. No per-type
      * whitelist, so this supports exactly what the used ScalaCheck version supports, on every platform.
      */
    lazy val structurallyReplaced: Seq[UntypedMethod] = {
      val names = Set(
        "arbContainer",
        "arbContainer2",
        "arbOption",
        "arbEither",
        "arbTry",
        "arbPartialFunction",
        "arbFuture",
        "arbGen"
      )
      Type.of[Arbitrary.type].unsortedMethods.collect {
        case method if method.isImplicit && (names(method.name) || method.name.matches("arb(Tuple|Function)\\d+")) =>
          method.asUntyped
      }
    }

    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      implicit val ArbitraryA: Type[Arbitrary[A]] = ArbitraryTypes.Arbitrary[A]

      // For the type being derived, a non-ScalaCheck instance may be the very definition in progress (a `derives`
      // given, or `implicit val x: Arbitrary[A] = Arbitrary.derived[A]`), so ScalaCheck's instance is only used when no other
      // instance exists - then the summoned one is necessarily built from ScalaCheck's own instances. This keeps e.g.
      // `Arbitrary.derived[String]` (forced by `derives` on generic types) on ScalaCheck's instance.
      if (
        arbctx.derivedType.exists(_.Underlying =:= Type[A]) &&
        ArbitraryTypes.Arbitrary[A].summonExprIgnoring(ArbitraryUseImplicitRule.ignoredImplicits*).toEither.isRight
      )
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type with a non-ScalaCheck instance in scope"))
      else
        ArbitraryTypes.Arbitrary[A].summonExprIgnoring(structurallyReplaced*).toEither match {
          case Right(arbExpr) =>
            Log.info(s"Using ScalaCheck built-in Arbitrary for ${Type[A].prettyPrint}") >>
              MIO.pure(Rule.matched(Expr.quote {
                Expr.splice(arbExpr).arbitrary
              }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No built-in Arbitrary[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
