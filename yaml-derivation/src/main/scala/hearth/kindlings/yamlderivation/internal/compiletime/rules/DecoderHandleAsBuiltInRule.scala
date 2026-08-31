package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsBuiltInRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsBuiltInRule extends DecoderDerivationRule("handle as built-in primitive type") {

    implicit val NodeT: Type[Node] = DTypes.Node
    implicit val ConstructErrorT: Type[ConstructError] = DTypes.ConstructError
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
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
        implicit val EitherCEA: Type[Either[ConstructError, A]] = DTypes.DecoderResult[A]
        val node = dctx.node

        if (Type[A] =:= Type[String]) {
          implicit val ResultT: Type[Either[ConstructError, String]] = DTypes.DecoderResult[String]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseString(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Boolean]) {
          implicit val ResultT: Type[Either[ConstructError, Boolean]] = DTypes.DecoderResult[Boolean]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseBoolean(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Int]) {
          implicit val ResultT: Type[Either[ConstructError, Int]] = DTypes.DecoderResult[Int]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseInt(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Long]) {
          implicit val ResultT: Type[Either[ConstructError, Long]] = DTypes.DecoderResult[Long]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseLong(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Double]) {
          implicit val ResultT: Type[Either[ConstructError, Double]] = DTypes.DecoderResult[Double]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseDouble(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Float]) {
          implicit val ResultT: Type[Either[ConstructError, Float]] = DTypes.DecoderResult[Float]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseFloat(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Short]) {
          implicit val ResultT: Type[Either[ConstructError, Short]] = DTypes.DecoderResult[Short]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseShort(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Byte]) {
          implicit val ResultT: Type[Either[ConstructError, Byte]] = DTypes.DecoderResult[Byte]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseByte(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[Char]) {
          implicit val ResultT: Type[Either[ConstructError, Char]] = DTypes.DecoderResult[Char]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseChar(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[BigDecimal]) {
          implicit val ResultT: Type[Either[ConstructError, BigDecimal]] = DTypes.DecoderResult[BigDecimal]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseBigDecimal(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else if (Type[A] =:= Type[BigInt]) {
          implicit val ResultT: Type[Either[ConstructError, BigInt]] = DTypes.DecoderResult[BigInt]
          Rule.matched(
            Expr
              .quote {
                YamlDerivationUtils
                  .getScalarValue(Expr.splice(node))
                  .flatMap(v => YamlDerivationUtils.parseBigInt(v, Expr.splice(node)))
              }
              .asInstanceOf[Expr[Either[ConstructError, A]]]
          )
        } else Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in primitive type")
      }
  }

}
