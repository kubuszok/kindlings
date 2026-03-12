package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait ShowMapRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowMapRule extends ShowDerivationRule("Show as map") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking map for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsMap(isMapExist) =>
            import isMapExist.Underlying as Pair
            deriveShowMap[A, Pair](isMapExist.value)
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a map"))
        }
      }

    @scala.annotation.nowarn("msg=is never used")
    private def deriveShowMap[A: ShowCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[String]]] = {
      import isMap.{Key, Value}
      implicit val StringType: Type[String] = ShowTypes.String
      val name = Type[A].shortName
      val iterableExpr = isMap.asIterable(sctx.value)

      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          for {
            keyStr <- deriveShowRecursively[Key](using sctx.nest(keyExpr))
            valueStr <- deriveShowRecursively[Value](using sctx.nest(valueExpr))
          } yield Expr.quote(Expr.splice(keyStr) + " -> " + Expr.splice(valueStr))
        }
        .map { builder =>
          val lambda = builder.build[String]
          Rule.matched(Expr.quote {
            val items = Expr.splice(iterableExpr).map(pair => Expr.splice(lambda).apply(pair))
            Expr.splice(Expr(name)) + "(" + items.mkString(", ") + ")"
          })
        }
    }
  }
}
