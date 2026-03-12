package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait FastShowPrettyHandleAsOptionRuleImpl { this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  object FastShowPrettyHandleAsOptionRule extends DerivationRule("handle as Option when possible") {
    implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
      Log.info(s"Attempting to handle ${Type[A].prettyPrint} as Option") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveResultRecursively[Inner](using ctx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[StringBuilder]
                Rule.matched(
                  isOption.value.fold[StringBuilder](ctx.value)(
                    onEmpty = Expr.quote(Expr.splice(ctx.sb).append("None")),
                    onSome = innerExpr =>
                      Expr.quote {
                        val _ = Expr.splice(ctx.sb).append("Some(")
                        val _ = Expr.splice(lambda).apply(Expr.splice(innerExpr))
                        Expr.splice(ctx.sb).append(")")
                      }
                  )
                )
              }

          case _ =>
            MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
