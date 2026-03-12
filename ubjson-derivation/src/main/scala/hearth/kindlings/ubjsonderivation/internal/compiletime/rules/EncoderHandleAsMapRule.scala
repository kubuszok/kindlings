package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonWriter
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait EncoderHandleAsMapRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit
    implicit val StringT: Type[String] = CTypes.String
    implicit val UBJsonWriterT: Type[UBJsonWriter] = CTypes.UBJsonWriter

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Unit]]] = {
      import isMap.{Key, Value}
      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .map { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils.writeMapStringKeyed[Value](
                Expr.splice(ectx.writer),
                Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]],
                Expr.splice(valueLambda)
              )
            })
          }
      } else {
        // Non-String keys — encode key via .toString
        LambdaBuilder
          .of1[Value]("mapValue")
          .traverse { valueExpr =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr))
          }
          .map { valueBuilder =>
            val valueLambda = valueBuilder.build[Unit]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils.writeMapWithKeyEncoder[Key, Value](
                Expr.splice(ectx.writer),
                Expr.splice(iterableExpr).asInstanceOf[Iterable[(Key, Value)]],
                (k: Key) => k.toString,
                Expr.splice(valueLambda)
              )
            })
          }
      }
    }
  }

}
