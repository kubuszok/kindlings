package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsLiteralTypeRuleImpl {
  this: ArbitraryMacrosImpl & ScalacheckLiteralSupport & MacroCommons & StdExtensions =>

  object ArbitraryHandleAsLiteralTypeRule extends ArbitraryDerivationRule("handle as literal type when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      literalValue[A] match {
        case Some(value) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as a literal type") >>
            MIO.pure(Rule.matched(Expr.quote(_root_.org.scalacheck.Gen.const[A](Expr.splice(value)))))
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
      }
  }
}
