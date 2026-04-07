package hearth.kindlings.sconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.sconfigderivation.ConfigDecodingError
import hearth.kindlings.sconfigderivation.internal.runtime.SConfigDerivationUtils
import org.ekrich.config.ConfigValue

trait ReaderHandleAsCollectionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCollectionRule extends ReaderDerivationRule("handle as collection when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigDecodingError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val ConfigValueT: Type[ConfigValue] = RTypes.ConfigValue
            implicit val EitherItem: Type[Either[ConfigDecodingError, Item]] = RTypes.ReaderResult[Item]

            LambdaBuilder
              .of1[ConfigValue]("itemValue")
              .traverse { itemValueExpr =>
                deriveReaderRecursively[Item](using rctx.nest[Item](itemValueExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConfigDecodingError, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  SConfigDerivationUtils
                    .decodeCollectionWith[Item, A](
                      Expr.splice(rctx.value),
                      SConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[ConfigDecodingError, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
