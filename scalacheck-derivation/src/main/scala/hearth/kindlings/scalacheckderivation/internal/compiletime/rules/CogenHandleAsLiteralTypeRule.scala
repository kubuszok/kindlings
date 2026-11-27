package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsLiteralTypeRuleImpl {
  this: CogenMacrosImpl & ScalacheckLiteralSupport & MacroCommons & StdExtensions =>

  object CogenHandleAsLiteralTypeRule extends CogenDerivationRule("handle as literal type when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      literalValue[A] match {
        case Some(_) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as a literal type") >>
            MIO.pure(Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenLiteral[A]
            }))
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
      }
  }
}
