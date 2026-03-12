package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowValueTypeRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowValueTypeRule extends ShowDerivationRule("Show as value type") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking value type for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            val unwrapped = isValueType.value.unwrap(sctx.value)
            deriveShowRecursively[Inner](using sctx.nest(unwrapped)).map(Rule.matched(_))
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a value type"))
        }
      }
  }
}
