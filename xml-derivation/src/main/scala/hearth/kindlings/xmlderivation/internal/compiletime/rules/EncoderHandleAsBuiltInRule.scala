package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait EncoderHandleAsBuiltInRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsBuiltInRule extends EncoderDerivationRule("handle as built-in primitive type") {

    implicit val ElemT: Type[scala.xml.Elem] = Types.Elem
    implicit val BooleanT: Type[Boolean] = Types.Boolean
    implicit val ByteT: Type[Byte] = Types.Byte
    implicit val ShortT: Type[Short] = Types.Short
    implicit val IntT: Type[Int] = Types.Int
    implicit val LongT: Type[Long] = Types.Long
    implicit val FloatT: Type[Float] = Types.Float
    implicit val DoubleT: Type[Double] = Types.Double
    implicit val CharT: Type[Char] = Types.Char
    implicit val StringT: Type[String] = Types.String
    implicit val BigDecimalT: Type[BigDecimal] = Types.BigDecimal
    implicit val BigIntT: Type[BigInt] = Types.BigInt

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[scala.xml.Elem]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        if (Type[A] <:< Type[String]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[String]))
        })
        else if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
          XmlDerivationUtils
            .makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Boolean]).toString)
        })
        else if (Type[A] <:< Type[Int]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Int]).toString)
        })
        else if (Type[A] <:< Type[Long]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Long]).toString)
        })
        else if (Type[A] <:< Type[Double]) Rule.matched(Expr.quote {
          XmlDerivationUtils
            .makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Double]).toString)
        })
        else if (Type[A] <:< Type[Float]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Float]).toString)
        })
        else if (Type[A] <:< Type[Short]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Short]).toString)
        })
        else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Byte]).toString)
        })
        else if (Type[A] <:< Type[Char]) Rule.matched(Expr.quote {
          XmlDerivationUtils.makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[Char]).toString)
        })
        else if (Type[A] <:< Type[BigDecimal]) Rule.matched(Expr.quote {
          XmlDerivationUtils
            .makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[BigDecimal]).toString)
        })
        else if (Type[A] <:< Type[BigInt]) Rule.matched(Expr.quote {
          XmlDerivationUtils
            .makeTextElem(Expr.splice(ectx.elementName), Expr.splice(ectx.value.upcast[BigInt]).toString)
        })
        else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }

}
