package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsOptionRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  object ShrinkHandleAsOptionRule extends ShrinkDerivationRule("handle as Option when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as Option") >>
            deriveShrinkRecursively[Inner](using shrinkctx.nest[Inner]).flatMap { innerShrink =>
              MIO.pure(Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
                  .shrinkOption(Expr.splice(innerShrink))
                  .asInstanceOf[Shrink[A]]
              }))
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
      }
  }
}
