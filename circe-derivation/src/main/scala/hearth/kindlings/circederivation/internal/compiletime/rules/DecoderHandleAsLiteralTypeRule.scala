package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.{DecodingFailure, HCursor}

trait DecoderHandleAsLiteralTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsLiteralTypeRule extends DecoderDerivationRule("handle as literal type when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
        implicit val DFT: Type[DecodingFailure] = DTypes.DecodingFailure
        extractLiteralDecoder[A] match {
          case Some(decoderExpr) => MIO.pure(Rule.matched(decoderExpr))
          case None              => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def decodeLiteral[A: DecoderCtx, U: Type](
        codec: TypeCodec[U],
        decode: Expr[HCursor] => Expr[Either[DecodingFailure, U]]
    )(implicit
        EitherDFA: Type[Either[DecodingFailure, A]],
        DFT: Type[DecodingFailure],
        exprCodec: ExprCodec[U]
    ): Option[Expr[Either[DecodingFailure, A]]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        Expr.quote {
          Expr.splice(decode(dctx.cursor)) match {
            case Right(v) if v == Expr.splice(Expr(constant)) =>
              Right(v.asInstanceOf[A])
            case Right(v) =>
              Left(
                DecodingFailure(
                  s"Expected literal value ${Expr.splice(Expr(constant))} but got " + v,
                  Expr.splice(dctx.cursor).history
                )
              )
            case Left(err) => Left(err)
          }
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx](implicit
        EitherDFA: Type[Either[DecodingFailure, A]],
        DFT: Type[DecodingFailure]
    ): Option[Expr[Either[DecodingFailure, A]]] = {
      implicit val StringT: Type[String] = DTypes.String
      implicit val BooleanT: Type[Boolean] = DTypes.Boolean
      implicit val IntT: Type[Int] = DTypes.Int
      implicit val LongT: Type[Long] = DTypes.Long
      implicit val DoubleT: Type[Double] = DTypes.Double

      decodeLiteral(Type.StringCodec, c => Expr.quote(Expr.splice(c).as[String](io.circe.Decoder.decodeString)))
        .orElse(
          decodeLiteral(Type.IntCodec, c => Expr.quote(Expr.splice(c).as[Int](io.circe.Decoder.decodeInt)))
        )
        .orElse(
          decodeLiteral(Type.LongCodec, c => Expr.quote(Expr.splice(c).as[Long](io.circe.Decoder.decodeLong)))
        )
        .orElse(
          decodeLiteral(
            Type.BooleanCodec,
            c => Expr.quote(Expr.splice(c).as[Boolean](io.circe.Decoder.decodeBoolean))
          )
        )
        .orElse(
          decodeLiteral(
            Type.DoubleCodec,
            c => Expr.quote(Expr.splice(c).as[Double](io.circe.Decoder.decodeDouble))
          )
        )
    }
  }
}
