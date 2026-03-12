package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import io.circe.Json

trait EncoderHandleAsLiteralTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralJson[A] match {
          case Some(jsonExpr) => MIO.pure(Rule.matched(jsonExpr))
          case None           => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralJson[A: EncoderCtx]: Option[Expr[Json]] = {
      implicit val JsonT: Type[Json] = Types.Json

      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value; Expr.quote(Json.fromString(Expr.splice(Expr(v))))
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v))))
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value; Expr.quote(Json.fromLong(Expr.splice(Expr(v))))
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value; Expr.quote(Json.fromDoubleOrNull(Expr.splice(Expr(v))))
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value; Expr.quote(Json.fromFloatOrNull(Expr.splice(Expr(v))))
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value; Expr.quote(Json.fromBoolean(Expr.splice(Expr(v))))
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v)).toInt))
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value; Expr.quote(Json.fromInt(Expr.splice(Expr(v)).toInt))
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value; Expr.quote(Json.fromString(Expr.splice(Expr(v)).toString))
      }
    }
  }
}
