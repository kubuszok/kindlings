package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

trait FastShowPrettyHandleAsCollectionRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsCollectionRule extends DerivationRule("handle as collection when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
        Type[A] match {
          case IsCollection(isCollection) =>
            import isCollection.Underlying as Item
            deriveCollectionItems(isCollection.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveCollectionItems[A: DerivationCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      val name = Expr(Type[A].shortName)
      val iterableExpr = isCollection.asIterable(ctx.value)

      LambdaBuilder
        .of1[Item]("item")
        .traverse { itemExpr =>
          deriveResultRecursively[Item](using ctx.incrementLevel.nest(itemExpr)).map { result =>
            Expr.quote {
              val _ = FastShowPrettyUtils.appendIndent(
                Expr.splice(ctx.sb),
                Expr.splice(ctx.config).indentString,
                Expr.splice(ctx.level) + 1
              )
              Expr.splice(result)
            }
          }
        }
        .map { builder =>
          val lambda = builder.build[StringBuilder]
          Rule.matched(Expr.quote {
            val _ = FastShowPrettyUtils.openCollection(Expr.splice(ctx.sb), Expr.splice(name))
            val _ = FastShowPrettyUtils.fillCollection(
              Expr.splice(ctx.sb),
              Expr.splice(iterableExpr),
              Expr.splice(ctx.config).indentString,
              Expr.splice(ctx.level)
            )(Expr.splice(lambda))
            FastShowPrettyUtils.closeCollection(Expr.splice(ctx.sb))
          })
        }
    }

    private def yieldUnsupportedType[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a collection"))
  }
}
