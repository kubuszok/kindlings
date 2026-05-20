package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*

trait DiffValueTypeRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  object DiffValueTypeRule extends DiffDerivationRule("Diff as value type") {
    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val leftInner = isValueType.value.unwrap(dctx.left)
            val rightInner = isValueType.value.unwrap(dctx.right)
            deriveDiffRecursively[Inner](using dctx.nest(leftInner, rightInner)).map(Rule.matched(_))
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
