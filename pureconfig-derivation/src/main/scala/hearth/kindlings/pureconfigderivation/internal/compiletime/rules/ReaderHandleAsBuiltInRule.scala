package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsBuiltInRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsBuiltInRule extends ReaderDerivationRule("handle as built-in primitive type") {

    implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
    implicit val FailuresT: Type[ConfigReaderFailures] = RTypes.ConfigReaderFailures
    implicit val StringT: Type[String] = RTypes.String
    implicit val BooleanT: Type[Boolean] = RTypes.Boolean
    implicit val IntT: Type[Int] = RTypes.Int
    implicit val LongT: Type[Long] = RTypes.Long
    implicit val DoubleT: Type[Double] = RTypes.Double
    implicit val FloatT: Type[Float] = RTypes.Float
    implicit val ShortT: Type[Short] = RTypes.Short
    implicit val ByteT: Type[Byte] = RTypes.Byte

    @scala.annotation.nowarn("msg=is never used")
    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        val cursor = rctx.cursor

        if (Type[A] <:< Type[String]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, String]] = RTypes.ReaderResult[String]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asString }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Boolean]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Boolean]] = RTypes.ReaderResult[Boolean]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asBoolean }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Int]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Int]] = RTypes.ReaderResult[Int]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asInt }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Long]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Long]] = RTypes.ReaderResult[Long]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asLong }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Double]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Double]] = RTypes.ReaderResult[Double]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asDouble }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Float]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Float]] = RTypes.ReaderResult[Float]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asFloat }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Short]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Short]] = RTypes.ReaderResult[Short]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asShort }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else if (Type[A] <:< Type[Byte]) {
          implicit val ResultT: Type[Either[ConfigReaderFailures, Byte]] = RTypes.ReaderResult[Byte]
          Rule.matched(
            Expr
              .quote { Expr.splice(cursor).asByte }
              .asInstanceOf[Expr[Either[ConfigReaderFailures, A]]]
          )
        } else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }
}
