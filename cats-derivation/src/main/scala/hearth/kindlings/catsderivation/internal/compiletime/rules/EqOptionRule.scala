package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

@scala.annotation.nowarn("msg=is never used")
trait EqOptionRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqOptionRule extends EqDerivationRule("Eq as Option") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          LambdaBuilder
            .of2[Inner, Inner]("ix", "iy")
            .traverse { case (ix, iy) =>
              deriveEqRecursively[Inner](using eqctx.nest(ix, iy))
            }
            .map { builder =>
              val lambda = builder.build[Boolean]
              Rule.matched(
                isOption.value.fold[Boolean](eqctx.x)(
                  onEmpty = isOption.value.fold[Boolean](eqctx.y)(
                    onEmpty = Expr(true),
                    onSome = _ => Expr(false)
                  ),
                  onSome = xInner =>
                    isOption.value.fold[Boolean](eqctx.y)(
                      onEmpty = Expr(false),
                      onSome = yInner => Expr.quote(Expr.splice(lambda).apply(Expr.splice(xInner), Expr.splice(yInner)))
                    )
                )
              )
            }
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
      }
  }
}
