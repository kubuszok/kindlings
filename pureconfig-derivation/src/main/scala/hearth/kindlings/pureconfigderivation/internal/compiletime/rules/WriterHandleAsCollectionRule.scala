package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import com.typesafe.config.ConfigValue
import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils

trait WriterHandleAsCollectionRuleImpl {
  this: WriterMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object WriterHandleAsCollectionRule extends WriterDerivationRule("handle as collection or map when possible") {
    implicit val ConfigValueT: Type[ConfigValue] = WTypes.ConfigValue

    def apply[A: WriterCtx]: MIO[Rule.Applicability[Expr[ConfigValue]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection or map") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            // A map is an `IsCollectionOf` whose proof is an `IsMapOf`. Parse `IsCollection` ONCE and dispatch on
            // that, instead of the old map-rule (`IsMap.parse` = `IsCollection.parse` + cast) followed by a separate
            // collection rule that parsed `IsCollection` again — every non-map field paid for the parse twice.
            import isCollection.Underlying as Item
            isCollection.value.asMap match {
              case Some(isMapOf) =>
                WriterHandleAsMapRule.deriveMapEntries[A, Item](isMapOf)
              case None =>
                LambdaBuilder
                  .of1[Item]("item")
                  .traverse { itemExpr =>
                    deriveWriterRecursively[Item](using wctx.nest(itemExpr))
                  }
                  .map { builder =>
                    val lambda = builder.build[ConfigValue]
                    val iterableExpr = isCollection.value.asIterable(wctx.value)
                    Rule.matched(Expr.quote {
                      PureConfigDerivationUtils.writeIterable[Item](
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
