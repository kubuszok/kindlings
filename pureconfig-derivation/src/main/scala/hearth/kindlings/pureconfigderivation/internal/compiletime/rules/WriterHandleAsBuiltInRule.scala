package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.typesafe.config.{ConfigValue, ConfigValueFactory}

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

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] =:= Type[String]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(Expr.splice(wctx.value.upcast[String]))
        })
        else if (Type[A] =:= Type[Boolean]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Boolean.valueOf(Expr.splice(wctx.value.upcast[Boolean])))
        })
        else if (Type[A] =:= Type[Int]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Integer.valueOf(Expr.splice(wctx.value.upcast[Int])))
        })
        else if (Type[A] =:= Type[Long]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Long.valueOf(Expr.splice(wctx.value.upcast[Long])))
        })
        else if (Type[A] =:= Type[Double]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Double.valueOf(Expr.splice(wctx.value.upcast[Double])))
        })
        else if (Type[A] =:= Type[Float]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Float.valueOf(Expr.splice(wctx.value.upcast[Float])))
        })
        else if (Type[A] =:= Type[Short]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Short.valueOf(Expr.splice(wctx.value.upcast[Short])))
        })
        else if (Type[A] =:= Type[Byte]) Rule.matched(Expr.quote {
          ConfigValueFactory.fromAnyRef(java.lang.Byte.valueOf(Expr.splice(wctx.value.upcast[Byte])))
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }
}
