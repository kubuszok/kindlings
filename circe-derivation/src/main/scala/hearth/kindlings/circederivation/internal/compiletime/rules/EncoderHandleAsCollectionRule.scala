package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.circederivation.internal.runtime.CirceDerivationUtils
import io.circe.Json

trait EncoderHandleAsCollectionRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection or map when possible") {
    implicit val JsonT: Type[Json] = Types.Json

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Json]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of the old map-rule (`IsMap.parse` = `IsCollection.parse` + cast) followed by a separate
            // collection rule that parsed `IsCollection` again — every non-map field paid for the parse twice.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                EncoderHandleAsMapRule.deriveMapEntries[A, Item](isMapOf)
              case None =>
                LambdaBuilder
                  .of1[Item]("item")
                  .traverse { itemExpr =>
                    deriveEncoderRecursively[Item](using ectx.nest(itemExpr))
                  }
                  .map { builder =>
                    val lambda = builder.build[Json]
                    val iterableExpr = isCollection.value.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      CirceDerivationUtils.encodeIterable[Item](
                        Expr.splice(iterableExpr),
                        Expr.splice(lambda)
                      )
                    })
                  }
            }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection or map"))
        }
      }
  }
}
