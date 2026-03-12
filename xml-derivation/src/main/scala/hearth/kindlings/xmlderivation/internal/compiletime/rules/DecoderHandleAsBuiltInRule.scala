package hearth.kindlings.xmlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.xmlderivation.XmlDecodingError
import hearth.kindlings.xmlderivation.internal.runtime.XmlDerivationUtils

trait DecoderHandleAsBuiltInRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsBuiltInRule extends DecoderDerivationRule("handle as built-in primitive type") {

    implicit val ElemT: Type[scala.xml.Elem] = DTypes.Elem
    implicit val XmlDecodingErrorT: Type[XmlDecodingError] = DTypes.XmlDecodingError
    implicit val StringT: Type[String] = DTypes.String
    implicit val BooleanT: Type[Boolean] = DTypes.Boolean
    implicit val ByteT: Type[Byte] = DTypes.Byte
    implicit val ShortT: Type[Short] = DTypes.Short
    implicit val IntT: Type[Int] = DTypes.Int
    implicit val LongT: Type[Long] = DTypes.Long
    implicit val FloatT: Type[Float] = DTypes.Float
    implicit val DoubleT: Type[Double] = DTypes.Double
    implicit val CharT: Type[Char] = DTypes.Char
    implicit val BigDecimalT: Type[BigDecimal] = DTypes.BigDecimal
    implicit val BigIntT: Type[BigInt] = DTypes.BigInt

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[XmlDecodingError, A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        implicit val EitherT: Type[Either[XmlDecodingError, A]] = DTypes.DecoderResult[A]
        val elem = dctx.elem

        if (Type[A] <:< Type[String]) {
          implicit val ResultT: Type[Either[XmlDecodingError, String]] = DTypes.DecoderResult[String]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseString(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Boolean]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Boolean]] = DTypes.DecoderResult[Boolean]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBoolean(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Int]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Int]] = DTypes.DecoderResult[Int]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseInt(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Long]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Long]] = DTypes.DecoderResult[Long]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseLong(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Double]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Double]] = DTypes.DecoderResult[Double]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseDouble(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Float]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Float]] = DTypes.DecoderResult[Float]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseFloat(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Short]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Short]] = DTypes.DecoderResult[Short]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseShort(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Byte]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Byte]] = DTypes.DecoderResult[Byte]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseByte(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[Char]) {
          implicit val ResultT: Type[Either[XmlDecodingError, Char]] = DTypes.DecoderResult[Char]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseChar(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[BigDecimal]) {
          implicit val ResultT: Type[Either[XmlDecodingError, BigDecimal]] = DTypes.DecoderResult[BigDecimal]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBigDecimal(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else if (Type[A] <:< Type[BigInt]) {
          implicit val ResultT: Type[Either[XmlDecodingError, BigInt]] = DTypes.DecoderResult[BigInt]
          Rule.matched(
            Expr
              .quote {
                XmlDerivationUtils
                  .getTextContent(Expr.splice(elem))
                  .flatMap(v => XmlDerivationUtils.parseBigInt(v, Expr.splice(elem).label))
              }
              .asInstanceOf[Expr[Either[XmlDecodingError, A]]]
          )
        } else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }

}
