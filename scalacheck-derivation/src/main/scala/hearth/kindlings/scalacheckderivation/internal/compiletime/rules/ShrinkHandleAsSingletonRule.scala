package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsSingletonRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  object ShrinkHandleAsSingletonRule extends ShrinkDerivationRule("handle as singleton when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          Log.info(s"Handling ${Type[A].prettyPrint} as singleton (empty shrink)") >>
            MIO.pure(Rule.matched(Expr.quote {
              _root_.org.scalacheck.Shrink.shrinkAny[A]
            }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
  }
}
