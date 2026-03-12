package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EqBuiltInRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqBuiltInRule extends EqDerivationRule("built-in Eq for primitives") {

    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] = {
      implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
      Log.info(s"Checking built-in Eq for ${Type[A].prettyPrint}") >> MIO {
        if (
          Type[A] <:< EqTypes.Boolean || Type[A] <:< EqTypes.Byte || Type[A] <:< EqTypes.Short ||
          Type[A] <:< EqTypes.Int || Type[A] <:< EqTypes.Long || Type[A] <:< EqTypes.Float ||
          Type[A] <:< EqTypes.Double || Type[A] <:< EqTypes.Char || Type[A] <:< EqTypes.String
        )
          Rule.matched(Expr.quote(Expr.splice(eqctx.x) == Expr.splice(eqctx.y)))
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }
}
