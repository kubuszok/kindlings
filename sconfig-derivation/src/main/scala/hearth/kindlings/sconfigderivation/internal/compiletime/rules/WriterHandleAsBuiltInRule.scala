package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigWriter
import org.ekrich.config.ConfigValue

trait WriterHandleAsBuiltInRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsBuiltInRule extends WriterDerivationRule("handle as built-in primitive type") {

    implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
    implicit val StringT: Type[String] = WTypes.String
    implicit val BooleanT: Type[Boolean] = WTypes.Boolean
    implicit val IntT: Type[Int] = WTypes.Int
    implicit val LongT: Type[Long] = WTypes.Long
    implicit val DoubleT: Type[Double] = WTypes.Double
    implicit val FloatT: Type[Float] = WTypes.Float
    implicit val ShortT: Type[Short] = WTypes.Short
    implicit val ByteT: Type[Byte] = WTypes.Byte
    implicit val CharT: Type[Char] = WTypes.Char
    implicit val BigDecimalT: Type[BigDecimal] = WTypes.BigDecimal
    implicit val BigIntT: Type[BigInt] = WTypes.BigInt

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          ConfigWriter.stringWriter.to(Expr.splice(wctx.value.upcast[String]))
        })
        else if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          ConfigWriter.booleanWriter.to(Expr.splice(wctx.value.upcast[Boolean]))
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          ConfigWriter.intWriter.to(Expr.splice(wctx.value.upcast[Int]))
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          ConfigWriter.longWriter.to(Expr.splice(wctx.value.upcast[Long]))
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          ConfigWriter.doubleWriter.to(Expr.splice(wctx.value.upcast[Double]))
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          ConfigWriter.floatWriter.to(Expr.splice(wctx.value.upcast[Float]))
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          ConfigWriter.shortWriter.to(Expr.splice(wctx.value.upcast[Short]))
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          ConfigWriter.byteWriter.to(Expr.splice(wctx.value.upcast[Byte]))
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          ConfigWriter.charWriter.to(Expr.splice(wctx.value.upcast[Char]))
        })
        else if (Type[A] <:< Type[BigDecimal]) Rule.matched(Expr.quote {
          ConfigWriter.bigDecimalWriter.to(Expr.splice(wctx.value.upcast[BigDecimal]))
        })
        else if (Type[A] <:< Type[BigInt]) Rule.matched(Expr.quote {
          ConfigWriter.bigIntWriter.to(Expr.splice(wctx.value.upcast[BigInt]))
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }
}
