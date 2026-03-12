package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.virtuslab.yaml.Node

trait EncoderHandleAsLiteralTypeRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsLiteralTypeRule extends EncoderDerivationRule("handle as literal type when possible") {

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a literal type") >> {
        implicit val NodeT: Type[Node] = Types.Node
        extractLiteralNode[A] match {
          case Some(expr) => MIO.pure(Rule.matched(expr))
          case None       => MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a literal type"))
        }
      }

    private def extractLiteralNode[A: EncoderCtx](implicit NodeT: Type[Node]): Option[Expr[Node]] =
      Type.StringCodec.fromType(Type[A]).map { e =>
        val v: String = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v))): Node))
      } orElse Type.IntCodec.fromType(Type[A]).map { e =>
        val v: Int = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.LongCodec.fromType(Type[A]).map { e =>
        val v: Long = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.DoubleCodec.fromType(Type[A]).map { e =>
        val v: Double = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.BooleanCodec.fromType(Type[A]).map { e =>
        val v: Boolean = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.FloatCodec.fromType(Type[A]).map { e =>
        val v: Float = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.ShortCodec.fromType(Type[A]).map { e =>
        val v: Short = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.ByteCodec.fromType(Type[A]).map { e =>
        val v: Byte = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      } orElse Type.CharCodec.fromType(Type[A]).map { e =>
        val v: Char = e.value
        Expr.quote((Node.ScalarNode(Expr.splice(Expr(v)).toString): Node))
      }
  }
}
