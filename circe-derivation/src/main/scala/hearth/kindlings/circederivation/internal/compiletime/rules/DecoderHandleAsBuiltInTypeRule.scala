package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.{Decoder, DecodingFailure, Json, JsonObject}

trait DecoderHandleAsBuiltInTypeRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsBuiltInTypeRule extends DecoderDerivationRule("handle as built-in type") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[DecodingFailure, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a built-in type") >> {
        implicit val EitherDFA: Type[Either[DecodingFailure, A]] = DTypes.DecoderResult[A]
        val cursor = dctx.cursor

        val result: Option[Expr[Either[DecodingFailure, A]]] =
          if (Type[A] =:= Type.of[String])
            Some(
              Expr.quote(Expr.splice(cursor).as[String](Decoder.decodeString).asInstanceOf[Either[DecodingFailure, A]])
            )
          else if (Type[A] =:= Type.of[Int])
            Some(Expr.quote(Expr.splice(cursor).as[Int](Decoder.decodeInt).asInstanceOf[Either[DecodingFailure, A]]))
          else if (Type[A] =:= Type.of[Long])
            Some(Expr.quote(Expr.splice(cursor).as[Long](Decoder.decodeLong).asInstanceOf[Either[DecodingFailure, A]]))
          else if (Type[A] =:= Type.of[Double])
            Some(
              Expr.quote(Expr.splice(cursor).as[Double](Decoder.decodeDouble).asInstanceOf[Either[DecodingFailure, A]])
            )
          else if (Type[A] =:= Type.of[Float])
            Some(
              Expr.quote(Expr.splice(cursor).as[Float](Decoder.decodeFloat).asInstanceOf[Either[DecodingFailure, A]])
            )
          else if (Type[A] =:= Type.of[Boolean])
            Some(
              Expr.quote(
                Expr.splice(cursor).as[Boolean](Decoder.decodeBoolean).asInstanceOf[Either[DecodingFailure, A]]
              )
            )
          else if (Type[A] =:= Type.of[Short])
            Some(
              Expr.quote(Expr.splice(cursor).as[Short](Decoder.decodeShort).asInstanceOf[Either[DecodingFailure, A]])
            )
          else if (Type[A] =:= Type.of[Byte])
            Some(Expr.quote(Expr.splice(cursor).as[Byte](Decoder.decodeByte).asInstanceOf[Either[DecodingFailure, A]]))
          else if (Type[A] =:= Type.of[Char])
            Some(Expr.quote(Expr.splice(cursor).as[Char](Decoder.decodeChar).asInstanceOf[Either[DecodingFailure, A]]))
          else if (Type[A] =:= Type.of[BigDecimal])
            Some(
              Expr.quote(
                Expr.splice(cursor).as[BigDecimal](Decoder.decodeBigDecimal).asInstanceOf[Either[DecodingFailure, A]]
              )
            )
          else if (Type[A] =:= Type.of[BigInt])
            Some(
              Expr.quote(Expr.splice(cursor).as[BigInt](Decoder.decodeBigInt).asInstanceOf[Either[DecodingFailure, A]])
            )
          else if (Type[A] =:= Type.of[Unit])
            Some(Expr.quote(Right(().asInstanceOf[A])))
          else if (Type[A] =:= Type.of[Json])
            Some(Expr.quote(Expr.splice(cursor).as[Json](Decoder.decodeJson).asInstanceOf[Either[DecodingFailure, A]]))
          else if (Type[A] =:= Type.of[JsonObject])
            Some(
              Expr.quote(
                Expr.splice(cursor).as[JsonObject](Decoder.decodeJsonObject).asInstanceOf[Either[DecodingFailure, A]]
              )
            )
          else
            None

        MIO.pure(result match {
          case Some(expr) => Rule.matched(expr)
          case None       => Rule.yielded(s"The type ${Type[A].prettyPrint} is not a built-in type")
        })
      }
  }

}
