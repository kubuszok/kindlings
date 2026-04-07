package hearth.kindlings.pureconfigderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.pureconfigderivation.internal.runtime.PureConfigDerivationUtils
import pureconfig.ConfigCursor
import pureconfig.error.ConfigReaderFailures

trait ReaderHandleAsCollectionRuleImpl {
  this: ReaderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object ReaderHandleAsCollectionRule extends ReaderDerivationRule("handle as collection when possible") {

    def apply[A: ReaderCtx]: MIO[Rule.Applicability[Expr[Either[ConfigReaderFailures, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val ConfigCursorT: Type[ConfigCursor] = RTypes.ConfigCursor
            implicit val EitherItem: Type[Either[ConfigReaderFailures, Item]] = RTypes.ReaderResult[Item]

            LambdaBuilder
              .of1[ConfigCursor]("itemCursor")
              .traverse { itemCursorExpr =>
                deriveReaderRecursively[Item](using rctx.nest[Item](itemCursorExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConfigReaderFailures, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  PureConfigDerivationUtils
                    .decodeCollectionWith[Item, A](
                      Expr.splice(rctx.cursor),
                      PureConfigDerivationUtils.readerFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[ConfigReaderFailures, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
