package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EmptyBuiltInRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  object EmptyBuiltInRule extends EmptyDerivationRule("built-in Empty for primitives") {

    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Checking built-in Empty for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< EmptyTypes.Boolean)
          Rule.matched(Expr(false).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Byte)
          Rule.matched(Expr(0.toByte).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Short)
          Rule.matched(Expr(0.toShort).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Int)
          Rule.matched(Expr(0).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Long)
          Rule.matched(Expr(0L).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Float)
          Rule.matched(Expr(0.0f).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Double)
          Rule.matched(Expr(0.0d).asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.Char)
          Rule.matched(Expr('\u0000').asInstanceOf[Expr[A]])
        else if (Type[A] <:< EmptyTypes.String)
          Rule.matched(Expr("").asInstanceOf[Expr[A]])
        else
          Rule.yielded(s"${Type[A].prettyPrint} is not a built-in Empty type")
      }
  }
}
