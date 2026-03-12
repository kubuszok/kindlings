package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait ShowCollectionRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowCollectionRule extends ShowDerivationRule("Show as collection") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking collection for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsCollection(isCollectionExist) =>
            import isCollectionExist.Underlying as Item
            deriveShowCollection[A, Item](isCollectionExist.value)
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a collection"))
        }
      }

    private def deriveShowCollection[A: ShowCtx, Item: Type](
        isCollection: IsCollectionOf[A, Item]
    ): MIO[Rule.Applicability[Expr[String]]] = {
      implicit val StringType: Type[String] = ShowTypes.String
      val name = Type[A].shortName
      val iterableExpr = isCollection.asIterable(sctx.value)

      LambdaBuilder
        .of1[Item]("item")
        .traverse { itemExpr =>
          deriveShowRecursively[Item](using sctx.nest(itemExpr))
        }
        .map { builder =>
          val lambda = builder.build[String]
          Rule.matched(Expr.quote {
            val items = Expr.splice(iterableExpr).map(item => Expr.splice(lambda).apply(item))
            Expr.splice(Expr(name)) + "(" + items.mkString(", ") + ")"
          })
        }
    }
  }
}
