package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsOptionRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  object CogenHandleAsOptionRule extends CogenDerivationRule("handle as Option when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as Option") >>
            deriveCogenRecursively[Inner](using cogenctx.nest[Inner]).flatMap { innerCogen =>
              MIO.pure(Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                  .cogenOption(Expr.splice(innerCogen))
                  .asInstanceOf[Cogen[A]]
              }))
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
      }
  }
}
