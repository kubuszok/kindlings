package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenUseImplicitRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn
  object CogenUseImplicitRule extends CogenDerivationRule("use implicit Cogen when available") {

    /** Every implicit ScalaCheck puts into `Cogen`'s implicit scope (its companion and the traits it mixes in) is
      * ignored here, so only instances the user (or another library) provides take precedence over derivation.
      * ScalaCheck's own instances are still used - by [[CogenBuiltInRule]] - except for the combinators Kindlings
      * replaces with structural rules, which recurse through derivation for the nested types.
      */
    lazy val ignoredImplicits: Seq[UntypedMethod] =
      Type.of[Cogen.type].unsortedMethods.collect {
        case method if method.isImplicit => method.asUntyped
      }

    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] = {
      implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

      if (cogenctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        CogenTypes.Cogen[A].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(implicitCogen) =>
            Log.info(s"Found implicit Cogen[${Type[A].prettyPrint}]") >>
              MIO.pure(Rule.matched(implicitCogen))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Cogen[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
