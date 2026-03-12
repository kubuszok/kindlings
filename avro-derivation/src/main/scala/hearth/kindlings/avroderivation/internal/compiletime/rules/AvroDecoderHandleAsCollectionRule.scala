package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.avroderivation.internal.runtime.AvroDerivationUtils

trait AvroDecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  object AvroDecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val AnyT: Type[Any] = DecTypes.Any

            LambdaBuilder
              .of1[Any]("itemRaw")
              .traverse { itemRawExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemRawExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  AvroDerivationUtils
                    .decodeCollection(
                      Expr.splice(dctx.avroValue),
                      Expr.splice(decodeFn),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[A]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
