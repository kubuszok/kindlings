package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroEncoderHandleAsLiteralTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralEncoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralEncoder[A: EncoderCtx]: Option[Expr[Any]] =
      // For Avro, primitives encode as their Java equivalents cast to Any
      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value; Expr.quote(Expr.splice(Expr(v)): Any)
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value; Expr.quote(Expr.splice(Expr(v)).toInt: Any)
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value; Expr.quote(Expr.splice(Expr(v)).toInt: Any)
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value; Expr.quote(Expr.splice(Expr(v)).toString: Any)
      }
  }
}
