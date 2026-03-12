package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroEncoderHandleAsCollectionRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroEncoderHandleAsCollectionRule extends EncoderDerivationRule("handle as collection when possible") {
    implicit val AnyT: Type[Any] = EncTypes.Any

    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Expr[Any]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            LambdaBuilder
              .of1[Item]("item")
              .traverse { itemExpr =>
                deriveEncoderRecursively[Item](using ectx.nest(itemExpr))
              }
              .map { builder =>
                val lambda = builder.build[Any]
                val iterableExpr = isCollection.value.asIterable(ectx.value)
                Rule.matched(Expr.quote {
                  AvroDerivationUtils.encodeIterable[Item](
                    Expr.splice(iterableExpr),
                    (item: Item) => Expr.splice(lambda).apply(item)
                  ): Any
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
