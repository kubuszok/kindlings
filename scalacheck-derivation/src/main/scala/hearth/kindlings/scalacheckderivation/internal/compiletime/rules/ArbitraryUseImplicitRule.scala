package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.{Arbitrary, Gen}

trait ArbitraryUseImplicitRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryUseImplicitRule extends ArbitraryDerivationRule("use implicit Arbitrary when available") {

    /** Every implicit ScalaCheck puts into `Arbitrary`'s implicit scope (its companion and the traits it mixes in) is
      * ignored here, so only instances the user (or another library) provides take precedence over derivation.
      * ScalaCheck's own instances are still used - by [[ArbitraryBuiltInRule]] - except for the combinators Kindlings
      * replaces with structural rules, which recurse through derivation for the nested types.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[Arbitrary.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] = {
      implicit val ArbitraryA: Type[Arbitrary[A]] = ArbitraryTypes.Arbitrary[A]

      // Check if we're deriving for the same type to prevent recursion
      if (arbctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        // Summon implicit and ignore the `derived` method to prevent infinite recursion
        ArbitraryTypes.Arbitrary[A].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(implicitArbitrary) =>
            Log.info(s"Found implicit Arbitrary[${Type[A].prettyPrint}]") >>
              MIO.pure(Rule.matched(Expr.quote {
                Expr.splice(implicitArbitrary).arbitrary
              }))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Arbitrary[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
