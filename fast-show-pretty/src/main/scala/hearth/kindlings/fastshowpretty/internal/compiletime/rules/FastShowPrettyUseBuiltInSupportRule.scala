package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

trait FastShowPrettyUseBuiltInSupportRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyUseBuiltInSupportRule
      extends DerivationRule("use built-in support when handling primitive types") {

    implicit val Boolean: Type[Boolean] = Types.Boolean
    implicit val Byte: Type[Byte] = Types.Byte
    implicit val Short: Type[Short] = Types.Short
    implicit val Int: Type[Int] = Types.Int
    implicit val Long: Type[Long] = Types.Long
    implicit val Float: Type[Float] = Types.Float
    implicit val Double: Type[Double] = Types.Double
    implicit val Char: Type[Char] = Types.Char
    implicit val String: Type[String] = Types.String

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderByte(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Byte]))
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderShort(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Short]))
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderInt(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Int]))
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderLong(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Long]))
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderFloat(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Float]))
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderDouble(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Double]))
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderChar(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Char]))
        })
        else if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          FastShowPrettyUtils.renderString(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[String]))
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a built-in type")
      }
  }
}
