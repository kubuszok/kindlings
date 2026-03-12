package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ShowBuiltInRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  object ShowBuiltInRule extends ShowDerivationRule("built-in Show for primitives") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      Log.info(s"Checking built-in Show for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< ShowTypes.String)
          Rule.matched(sctx.value.upcast[String])
        else if (
          Type[A] <:< ShowTypes.Boolean || Type[A] <:< ShowTypes.Byte || Type[A] <:< ShowTypes.Short ||
          Type[A] <:< ShowTypes.Int || Type[A] <:< ShowTypes.Long || Type[A] <:< ShowTypes.Float ||
          Type[A] <:< ShowTypes.Double || Type[A] <:< ShowTypes.Char
        )
          Rule.matched(Expr.quote(Expr.splice(sctx.value).toString))
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
      }
    }
  }
}
