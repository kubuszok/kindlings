package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait AvroEncoderHandleAsMapRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object AvroEncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val AnyT: Type[Any] = EncTypes.Any
    implicit val StringT: Type[String] = EncTypes.String
    implicit val StringAnyPairT: Type[(String, Any)] = Type.of[(String, Any)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Any]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueEncoded =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueEncoded))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, Any)]
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              val map = new java.util.HashMap[String, Any]()
              Expr.splice(iterableExpr).foreach { pair =>
                val encoded = Expr.splice(pairLambda).apply(pair)
                map.put(encoded._1, encoded._2)
              }
              map: Any
            })
          }
      }
    }
  }
}
