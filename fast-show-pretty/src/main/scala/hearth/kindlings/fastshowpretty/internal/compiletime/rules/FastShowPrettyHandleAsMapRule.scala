package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

import hearth.kindlings.fastshowpretty.internal.runtime.FastShowPrettyUtils

trait FastShowPrettyHandleAsMapRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsMapRule extends DerivationRule("handle as map when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a map") >> {
        Type[A] match {
          case IsMap(isMap) =>
            import isMap.Underlying as Pair
            deriveMapItems[A, Pair](isMap.value)

          case _ =>
            yieldUnsupportedType[A]
        }
      }

    private def deriveMapItems[A: DerivationCtx, Pair: Type](
        isMap: IsMapOf[A, Pair]
    ): MIO[Rule.Applicability[Expr[StringBuilder]]] = {
      import isMap.{Key, Value}
      val name = Expr(Type[A].shortName)
      val iterableExpr = isMap.asIterable(ctx.value)

      LambdaBuilder
        .of1[Pair]("pair")
        .traverse { pairExpr =>
          val keyExpr = isMap.key(pairExpr)
          val valueExpr = isMap.value(pairExpr)
          for {
            keyResult <- deriveResultRecursively[Key](using ctx.incrementLevel.nest(keyExpr))
            valueResult <- deriveResultRecursively[Value](using ctx.incrementLevel.nest(valueExpr))
          } yield Expr.quote {
            val _ = FastShowPrettyUtils.appendIndent(
              Expr.splice(ctx.sb),
              Expr.splice(ctx.config).indentString,
              Expr.splice(ctx.level) + 1
            )
            val _ = FastShowPrettyUtils.openMapEntry(Expr.splice(ctx.sb))
            val _ = Expr.splice(keyResult)
            val _ = FastShowPrettyUtils.appendMapArrow(Expr.splice(ctx.sb))
            val _ = Expr.splice(valueResult)
            FastShowPrettyUtils.closeMapEntry(Expr.splice(ctx.sb))
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
      MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a map"))
  }
}
