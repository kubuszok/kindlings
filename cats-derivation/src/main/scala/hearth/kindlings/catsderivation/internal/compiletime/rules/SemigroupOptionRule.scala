package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.fp.syntax.*
import hearth.std.*

/** `Option[X]` without an implicit `Semigroup[Option[X]]` - i.e. `X` has no `Semigroup` in scope, typically because it
  * is the (recursive) type being derived, e.g. `Recursive(i: Int, is: Option[Recursive])` - combined like cats' own
  * instance, with the derived `Semigroup[X]`.
  */
@scala.annotation.nowarn("msg=is never used")
trait SemigroupOptionRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupOptionRule extends SemigroupDerivationRule("Semigroup as Option") {
    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] =
      Type[A] match {
        case IsOption(isOption) =>
          import isOption.Underlying as Inner
          LambdaBuilder
            .of2[Inner, Inner]("ix", "iy")
            .traverse { case (ix, iy) =>
              deriveSemigroupRecursively[Inner](using sgctx.nest(ix, iy))
            }
            .map { builder =>
              val lambda = builder.build[Inner]
              Rule.matched(
                isOption.value.fold[A](sgctx.x)(
                  onEmpty = sgctx.y,
                  onSome = xInner =>
                    isOption.value.fold[A](sgctx.y)(
                      onEmpty = sgctx.x,
                      onSome = yInner =>
                        isOption.value.of(
                          Expr.quote(Expr.splice(lambda).apply(Expr.splice(xInner), Expr.splice(yInner)))
                        )
                    )
                )
              )
            }
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not an Option"))
      }
  }
}
