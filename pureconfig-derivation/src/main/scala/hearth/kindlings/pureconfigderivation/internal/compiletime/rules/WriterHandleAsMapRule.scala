package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils

trait WriterHandleAsMapRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object WriterHandleAsMapRule extends WriterDerivationRule("handle as map when possible") {
    implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue
    implicit val StringT: Type[String] = WTypes.String
    implicit val StringValuePairT: Type[(String, ConfigValue)] = Type.of[(String, ConfigValue)]

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    private def deriveMapEntries[A: WriterCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[ConfigValue]]] = {
      import isMap.{Key, Value}
      if (!(Key <:< Type[String]))
        MIO.pure(Rule.yielded(s"Map key type ${Key.prettyPrint} is not String"))
      else
        LambdaBuilder
          .of1[Pair]("pair")
          .traverse { pairExpr =>
            val keyExpr = isMap.key(pairExpr)
            val valueExpr = isMap.value(pairExpr)
            deriveWriterRecursively[Value](using wctx.nest(valueExpr)).map { valueResult =>
              Expr.quote {
                (Expr.splice(keyExpr).asInstanceOf[String], Expr.splice(valueResult))
              }
            }
          }
          .map { builder =>
            val pairLambda = builder.build[(String, ConfigValue)]
            val iterableExpr = isMap.asIterable(wctx.value)
            Rule.matched(Expr.quote {
              PureConfigDerivationUtils.writeMappedPairs[Pair](
                Expr.splice(iterableExpr),
                Expr.splice(pairLambda)
              )
            })
          }
    }
  }
}
