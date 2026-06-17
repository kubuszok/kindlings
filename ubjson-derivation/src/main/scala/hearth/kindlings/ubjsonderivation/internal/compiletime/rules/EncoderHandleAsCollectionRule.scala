package hearth.kindlings.ubjsonderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.ubjsonderivation.internal.runtime.UBJsonDerivationUtils

trait EncoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object EncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection or map when possible") {
    implicit val UnitT: Type[Unit] = CTypes.Unit

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Unit]]] =
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
                    val lambda = builder.build[Unit]
                    val iterableExpr = isCollection.value.asIterable(ectx.value)
                    Rule.matched(Expr.quote {
                      UBJsonDerivationUtils.writeArray[Item](
                        Expr.splice(ectx.writer),
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
