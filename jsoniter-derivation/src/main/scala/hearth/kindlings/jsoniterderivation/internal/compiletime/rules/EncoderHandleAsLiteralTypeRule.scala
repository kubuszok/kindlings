package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EncoderHandleAsLiteralTypeRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        extractLiteralEncoder[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralEncoder[A: EncoderCtx]: Option[Expr[Unit]] = {
      val writer = ectx.writer
      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v))))
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value
        Expr.quote(Expr.splice(writer).writeVal(Expr.splice(Expr(v)).toString))
      }
    }
  }
}
