package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

trait ShowOptionRuleImpl {
  this: ShowMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShowOptionRule extends ShowDerivationRule("Show as Option") {

    def apply[A: ShowCtx]: MIO[Rule.Applicability[Expr[String]]] =
      Log.info(s"Checking Option for Show[${Type[A].prettyPrint}]") >> {
        Type[A] match {
          case IsOption(isOption) =>
            import isOption.Underlying as Inner
            implicit val StringType: Type[String] = ShowTypes.String
            LambdaBuilder
              .of1[Inner]("inner")
              .traverse { innerExpr =>
                deriveShowRecursively[Inner](using sctx.nest(innerExpr))
              }
              .map { builder =>
                val lambda = builder.build[String]
                Rule.matched(
                  isOption.value.fold[String](sctx.value)(
                    onEmpty = Expr("None"),
                    onSome = innerExpr => Expr.quote("Some(" + Expr.splice(lambda).apply(Expr.splice(innerExpr)) + ")")
                  )
                )
              }
          case _ =>
            MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
        }
      }
  }
}
