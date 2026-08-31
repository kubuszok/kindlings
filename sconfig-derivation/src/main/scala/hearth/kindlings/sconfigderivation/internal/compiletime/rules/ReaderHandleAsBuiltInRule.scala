package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.{ConfigDecodingError, ConfigReader}
import org.ekrich.config.ConfigValue

trait ReaderHandleAsBuiltInRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsBuiltInRule extends ReaderDerivationRule("handle as built-in primitive type") {

    implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
    implicit val ErrorT: Type[ConfigDecodingError] = RTypes.ConfigDecodingError
    implicit val StringT: Type[String] = RTypes.String
    implicit val BooleanT: Type[Boolean] = RTypes.Boolean
    implicit val IntT: Type[Int] = RTypes.Int
    implicit val LongT: Type[Long] = RTypes.Long
    implicit val DoubleT: Type[Double] = RTypes.Double
    implicit val FloatT: Type[Float] = RTypes.Float
    implicit val ShortT: Type[Short] = RTypes.Short
    implicit val ByteT: Type[Byte] = RTypes.Byte
    implicit val CharT: Type[Char] = RTypes.Char
    implicit val BigDecimalT: Type[BigDecimal] = RTypes.BigDecimal
    implicit val BigIntT: Type[BigInt] = RTypes.BigInt

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        implicit val EitherT: Type[Either[ConfigDecodingError, A]] = RTypes.ReaderResult[A]
        val value = rctx.value

        if (Type[A] =:= Type[String]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, String]] = RTypes.ReaderResult[String]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.stringReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Boolean]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Boolean]] = RTypes.ReaderResult[Boolean]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.booleanReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Int]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Int]] = RTypes.ReaderResult[Int]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.intReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Long]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Long]] = RTypes.ReaderResult[Long]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.longReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Double]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Double]] = RTypes.ReaderResult[Double]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.doubleReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Float]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Float]] = RTypes.ReaderResult[Float]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.floatReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Short]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Short]] = RTypes.ReaderResult[Short]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.shortReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Byte]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Byte]] = RTypes.ReaderResult[Byte]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.byteReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[Char]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, Char]] = RTypes.ReaderResult[Char]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.charReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[BigDecimal]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, BigDecimal]] = RTypes.ReaderResult[BigDecimal]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.bigDecimalReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else if (Type[A] =:= Type[BigInt]) {
          implicit val ResultT: Type[Either[ConfigDecodingError, BigInt]] = RTypes.ReaderResult[BigInt]
          Rule.matched(
            Expr
              .quote {
                ConfigReader.bigIntReader.from(Expr.splice(value))
              }
              .asInstanceOf[Expr[Either[ConfigDecodingError, A]]]
          )
        } else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }
}
