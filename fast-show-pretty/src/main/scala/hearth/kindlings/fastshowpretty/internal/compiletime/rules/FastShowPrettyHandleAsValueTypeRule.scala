package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FastShowPrettyHandleAsValueTypeRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsValueTypeRule extends DerivationRule("handle as value type when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a value type") >> {
        Type[A] match {
          case IsValueType(isValueType) =>
            import isValueType.Underlying as Inner
            deriveValueTypeUnwrapped[A, Inner](isValueType.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveValueTypeUnwrapped[A: DerivationCtx, Inner: Type](
        isValueType: IsValueTypeOf[A, Inner]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      val unwrappedExpr = isValueType.unwrap(ctx.value)

      for {
        innerResult <- deriveResultRecursively[Inner](using ctx.nest(unwrappedExpr))
      } yield Rule.matched(innerResult)
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a value type"))
  }
}
