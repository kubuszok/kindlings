package hearth.kindlings.jsoniterderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.jsoniterderivation.internal.runtime.JsoniterDerivationUtils
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader

trait DecoderHandleAsCollectionRuleImpl {
  this: CodecMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            implicit val JsonReaderT: Type[JsonReader] = CTypes.JsonReader

            LambdaBuilder
              .of1[JsonReader]("itemReader")
              .traverse { itemReaderExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemReaderExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Item]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  JsoniterDerivationUtils
                    .readCollection[Item, A](
                      Expr.splice(dctx.reader),
                      Expr.splice(decodeFn),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]],
                      Expr.splice(dctx.config).setMaxInsertNumber
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
