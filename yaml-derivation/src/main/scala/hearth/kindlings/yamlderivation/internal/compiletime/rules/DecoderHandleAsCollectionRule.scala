package hearth.kindlings.yamlderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.yamlderivation.internal.runtime.YamlDerivationUtils
import org.virtuslab.yaml.{ConstructError, Node}

trait DecoderHandleAsCollectionRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  object DecoderHandleAsCollectionRule extends DecoderDerivationRule("handle as collection when possible") {

    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Expr[Either[ConstructError, A]]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            import isCollection.value.CtorResult
            @scala.annotation.nowarn("msg=is never used")
            implicit val NodeT: Type[Node] = DTypes.Node
            @scala.annotation.nowarn("msg=is never used")
            implicit val EitherCEItem: Type[Either[ConstructError, Item]] = DTypes.DecoderResult[Item]

            LambdaBuilder
              .of1[Node]("itemNode")
              .traverse { itemNodeExpr =>
                deriveDecoderRecursively[Item](using dctx.nest[Item](itemNodeExpr))
              }
              .map { builder =>
                val decodeFn = builder.build[Either[ConstructError, Item]]
                val factoryExpr = isCollection.value.factory
                Rule.matched(Expr.quote {
                  YamlDerivationUtils
                    .decodeCollectionWith(
                      Expr.splice(dctx.node),
                      YamlDerivationUtils.decoderFromFn(Expr.splice(decodeFn)),
                      Expr.splice(factoryExpr).asInstanceOf[scala.collection.Factory[Item, A]]
                    )
                    .asInstanceOf[Either[ConstructError, A]]
                })
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a collection"))
        }
      }
  }
}
