package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroDecoderHandleAsLiteralTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsLiteralTypeRule extends DecoderDerivationRule("handle as literal type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralDecoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def decodeLiteral[A: DecoderCtx, U](
        codec: TypeCodec[U],
        decode: Expr[Any] => Expr[U]
    )(implicit exprCodec: ExprCodec[U], ut: Type[U]): Option[Expr[A]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        Expr.quote {
          val actual = Expr.splice(decode(dctx.avroValue))
          if (actual != Expr.splice(Expr(constant)))
            throw new org.apache.avro.AvroTypeException(
              "Expected literal value " + Expr.splice(Expr(constant)) + " but got " + actual
            )
          actual.asInstanceOf[A]
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx]: Option[Expr[A]] = {
      implicit val StringT: Type[String] = DecTypes.String
      implicit val IntT: Type[Int] = DecTypes.Int
      implicit val LongT: Type[Long] = DecTypes.Long
      implicit val BooleanT: Type[Boolean] = DecTypes.Boolean
      implicit val DoubleT: Type[Double] = DecTypes.Double
      decodeLiteral(Type.StringCodec, v => Expr.quote(AvroDerivationUtils.decodeCharSequence(Expr.splice(v))))
        .orElse(decodeLiteral(Type.IntCodec, v => Expr.quote(Expr.splice(v).asInstanceOf[Int])))
        .orElse(decodeLiteral(Type.LongCodec, v => Expr.quote(Expr.splice(v).asInstanceOf[Long])))
        .orElse(decodeLiteral(Type.BooleanCodec, v => Expr.quote(Expr.splice(v).asInstanceOf[Boolean])))
        .orElse(decodeLiteral(Type.DoubleCodec, v => Expr.quote(Expr.splice(v).asInstanceOf[Double])))
    }
  }
}
