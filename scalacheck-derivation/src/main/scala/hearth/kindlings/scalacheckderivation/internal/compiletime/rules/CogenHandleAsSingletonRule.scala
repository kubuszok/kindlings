package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsSingletonRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  object CogenHandleAsSingletonRule extends CogenDerivationRule("handle as singleton when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as singleton (identity cogen)") >>
            MIO.pure(Rule.matched(Expr.quote {
              hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils.cogenIdentity[A]
            }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }
}
