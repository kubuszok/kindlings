package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait HashBuiltInRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions =>

  object HashBuiltInRule extends HashDerivationRule("built-in Hash for primitives") {

    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val IntType: Type[Int] = HashTypes.Int
      Log.info(s"Checking built-in Hash for ${Type[A].prettyPrint}") >> MIO {
        if (
          Type[A] <:< EqTypes.Boolean || Type[A] <:< EqTypes.Byte || Type[A] <:< EqTypes.Short ||
          Type[A] <:< EqTypes.Int || Type[A] <:< EqTypes.Long || Type[A] <:< EqTypes.Float ||
          Type[A] <:< EqTypes.Double || Type[A] <:< EqTypes.Char || Type[A] <:< EqTypes.String
        )
          Rule.matched(Expr.quote(Expr.splice(hctx.value).hashCode()))
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }
}
