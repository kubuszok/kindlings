package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.UBJsonReader
import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait DecoderHandleAsMapRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsMapRule extends DecoderDerivationRule("handle as map when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            decodeMapEntries[A, Pair](isMap.value)
          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def decodeMapEntries[A: DecoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[A]]] = {
      import isMap.{Key, Value, CtorResult}
      implicit val StringT: Type[String] = CTypes.String
      implicit val UBJsonReaderT: Type[UBJsonReader] = CTypes.UBJsonReader

      if (Key <:< Type[String]) {
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory
            Rule.matched(Expr.quote {
              UBJsonDerivationUtils
                .readMap[Value, A](
                  Expr.splice(dctx.reader),
                  Expr.splice(decodeFn),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(String, Value), A]],
                  Expr.splice(dctx.config).mapMaxInsertNumber
                )
                .asInstanceOf[A]
            })
          }
      } else {
        // Non-String keys — parse from string
        LambdaBuilder
          .of1[UBJsonReader]("valueReader")
          .traverse { valueReaderExpr =>
            deriveDecoderRecursively[Value](using dctx.nest[Value](valueReaderExpr))
          }
          .map { valueBuilder =>
            val decodeFn = valueBuilder.build[Value]
            val factoryExpr = isMap.factory

            // Build a String => Key function based on the key type
            val keyDecoder: Expr[String => Key] =
              if (Key =:= Type.of[Int]) Expr.quote((s: String) => s.toInt.asInstanceOf[Key])
              else if (Key =:= Type.of[Long]) Expr.quote((s: String) => s.toLong.asInstanceOf[Key])
              else if (Key =:= Type.of[Double]) Expr.quote((s: String) => s.toDouble.asInstanceOf[Key])
              else Expr.quote((s: String) => s.asInstanceOf[Key])

            Rule.matched(Expr.quote {
              UBJsonDerivationUtils
                .readMapWithKeyDecoder[Key, Value, A](
                  Expr.splice(dctx.reader),
                  Expr.splice(keyDecoder),
                  Expr.splice(decodeFn),
                  Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[(Key, Value), A]],
                  Expr.splice(dctx.config).mapMaxInsertNumber
                )
                .asInstanceOf[A]
            })
          }
      }
    }
  }

}
