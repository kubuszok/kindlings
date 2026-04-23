package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkUseImplicitRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn
  object ShrinkUseImplicitRule extends ShrinkDerivationRule("use implicit Shrink when available") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] = {
      implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

      if (shrinkctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        ShrinkTypes.Shrink[A].summonExprIgnoring().toEither match {
          case Right(implicitShrink) =>
            Log.info(s"Found implicit Shrink[${Type[A].prettyPrint}]") >>
              MIO.pure(Rule.matched(implicitShrink))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Shrink[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
