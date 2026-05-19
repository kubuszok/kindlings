package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
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

    @scala.annotation.nowarn("msg=is never used")
    private def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Any]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else {
        val dummyValue: Expr[Value] = Expr.quote(null.asInstanceOf[Value])
        deriveEncoderRecursively[Value](using ectx.nest(dummyValue)).flatMap { _ =>
          ectx.getHelper[Value].map { helperOpt =>
            val helper = helperOpt.get
            val iterableExpr = isMap.asIterable(ectx.value)
            Rule.matched(Expr.quote {
              val map = new java.util.HashMap[String, Any]()
              val iter = Expr.splice(iterableExpr).asInstanceOf[Iterable[(String, Value)]].iterator
              while (iter.hasNext) {
                val entry = iter.next()
                val _ = map.put(entry._1, Expr.splice(helper(Expr.quote(entry._2), ectx.config)))
              }
              map: Any
            })
          }
        }
      }
    }
  }
}
