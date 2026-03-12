package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsLiteralTypeRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsLiteralTypeRule extends DecoderDerivationRule("handle as literal type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralDecoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def decodeLiteral[A: DecoderCtx, U](
        codec: TypeCodec[U],
        read: Expr[JsonReader] => Expr[U]
    )(implicit exprCodec: ExprCodec[U], ut: Type[U]): Option[Expr[A]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        Expr.quote {
          val actual = Expr.splice(read(dctx.reader))
          if (actual != Expr.splice(Expr(constant)))
            Expr
              .splice(dctx.reader)
              .decodeError(s"Expected literal value " + Expr.splice(Expr(constant)) + " but got " + actual)
          actual.asInstanceOf[A]
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx]: Option[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val LongT: Type[Long] = CTypes.Long
      implicit val DoubleT: Type[Double] = CTypes.Double
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      decodeLiteral(Type.StringCodec, r => Expr.quote(Expr.splice(r).readString(null)))
        .orElse(decodeLiteral(Type.IntCodec, r => Expr.quote(Expr.splice(r).readInt())))
        .orElse(decodeLiteral(Type.LongCodec, r => Expr.quote(Expr.splice(r).readLong())))
        .orElse(decodeLiteral(Type.BooleanCodec, r => Expr.quote(Expr.splice(r).readBoolean())))
        .orElse(decodeLiteral(Type.DoubleCodec, r => Expr.quote(Expr.splice(r).readDouble())))
    }
  }
}
