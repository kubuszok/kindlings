package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.Node

trait EncoderHandleAsMapRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  @scala.annotation.nowarn("msg=Infinite loop")
  object EncoderHandleAsMapRule extends EncoderDerivationRule("handle as map when possible") {
    implicit val NodeT: Type[Node] = Types.Node
    implicit val StringT: Type[String] = Types.String
    implicit val StringNodePairT: Type[(String, Node)] = Type.of[(String, Node)]

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Node]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapEntries[A, Pair](isMap.value)

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a map"))
        }
      }

    // Exposed so the combined collection-or-map rule can call it after a single IsCollection parse.
    private[rules] def deriveMapEntries[A: EncoderCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[Node]]] = {
      import isMap.{Key, Value}
      implicit val NodePairT: Type[(Node, Node)] = Type.of[(Node, Node)]
      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          // `String` keys are written as scala-yaml's `YamlEncoder.forString` writes them; keys of any other type with
          // the encoder derived for them (as `YamlEncoder.forMap[K, V]` uses the key's instance).
          val keyNodeMIO: MIO[Expr[Node]] =
            if (Key <:< Type[String])
              MIO.pure(Expr.quote {
                YamlDerivationUtils.stringKeyNode(Expr.splice(keyExpr).asInstanceOf[String])
              })
            else deriveEncoderRecursively[Key](using ectx.nest(keyExpr))
          keyNodeMIO.flatMap { keyNode =>
            deriveEncoderRecursively[Value](using ectx.nest(valueExpr)).map { valueNode =>
              Expr.quote {
                (Expr.splice(keyNode), Expr.splice(valueNode))
              }
            }
          }
        }
        .map { builder =>
          val pairLambda = builder.build[(Node, Node)]
          val iterableExpr = isMap.asIterable(ectx.value)
          Rule.matched(Expr.quote {
            YamlDerivationUtils.encodeMapEntries[Pair](
              Expr.splice(iterableExpr),
              Expr.splice(pairLambda)
            )
          })
        }
    }
  }
}
