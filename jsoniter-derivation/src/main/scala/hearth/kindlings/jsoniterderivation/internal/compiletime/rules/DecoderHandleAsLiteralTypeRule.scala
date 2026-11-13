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

    /** Reads a literal value and checks it, with jsoniter-scala's `expected value: 2` / `expected value: "VVV"` errors.
      * Numbers and booleans are read from JSON strings with `@stringified` / `isStringified`.
      */
    private def decodeLiteral[A: DecoderCtx, U](
        codec: TypeCodec[U],
        read: Expr[JsonReader] => Expr[U],
        readFromString: Option[Expr[JsonReader] => Expr[U]],
        quoted: Boolean
    )(implicit exprCodec: ExprCodec[U], ut: Type[U]): Option[Expr[A]] =
      codec.fromType(Type[A]).map { e =>
        val constant: U = e.value
        def check(readExpr: Expr[U], msg: String): Expr[A] = Expr.quote {
          val actual = Expr.splice(readExpr)
          if (actual != Expr.splice(Expr(constant))) Expr.splice(dctx.reader).decodeError(Expr.splice(Expr(msg)))
          actual.asInstanceOf[A]
        }
        val plainMsg = if (quoted) s"expected value: \"$constant\"" else s"expected value: $constant"
        val plain = check(read(dctx.reader), plainMsg)
        readFromString match {
          case None             => plain
          case Some(fromString) =>
            val stringified = check(fromString(dctx.reader), s"expected value: \"$constant\"")
            dctx.stringifiedMode match {
              case Some(true)  => stringified
              case Some(false) => plain
              case None        =>
                Expr.quote(if (Expr.splice(dctx.config).isStringified) Expr.splice(stringified) else Expr.splice(plain))
            }
        }
      }

    private def extractLiteralDecoder[A: DecoderCtx]: Option[Expr[A]] = {
      implicit val StringT: Type[String] = CTypes.String
      implicit val IntT: Type[Int] = CTypes.Int
      implicit val LongT: Type[Long] = CTypes.Long
      implicit val DoubleT: Type[Double] = CTypes.Double
      implicit val FloatT: Type[Float] = CTypes.Float
      implicit val ShortT: Type[Short] = CTypes.Short
      implicit val ByteT: Type[Byte] = CTypes.Byte
      implicit val BooleanT: Type[Boolean] = CTypes.Boolean
      implicit val CharT: Type[Char] = CTypes.Char
      decodeLiteral(Type.StringCodec, r => Expr.quote(Expr.splice(r).readString(null)), None, quoted = true)
        .orElse(decodeLiteral(Type.CharCodec, r => Expr.quote(Expr.splice(r).readChar()), None, quoted = true))
        .orElse(
          decodeLiteral(
            Type.IntCodec,
            r => Expr.quote(Expr.splice(r).readInt()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsInt())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.LongCodec,
            r => Expr.quote(Expr.splice(r).readLong()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsLong())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.BooleanCodec,
            r => Expr.quote(Expr.splice(r).readBoolean()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsBoolean())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.DoubleCodec,
            r => Expr.quote(Expr.splice(r).readDouble()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsDouble())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.FloatCodec,
            r => Expr.quote(Expr.splice(r).readFloat()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsFloat())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.ShortCodec,
            r => Expr.quote(Expr.splice(r).readShort()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsShort())),
            quoted = false
          )
        )
        .orElse(
          decodeLiteral(
            Type.ByteCodec,
            r => Expr.quote(Expr.splice(r).readByte()),
            Some((r: Expr[JsonReader]) => Expr.quote(Expr.splice(r).readStringAsByte())),
            quoted = false
          )
        )
    }
  }
}
