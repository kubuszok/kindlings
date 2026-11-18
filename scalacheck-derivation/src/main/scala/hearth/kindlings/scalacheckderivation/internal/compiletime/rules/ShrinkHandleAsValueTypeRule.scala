package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsValueTypeRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShrinkHandleAsValueTypeRule extends ShrinkDerivationRule("handle as value type when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

          // Build wrap/unwrap outside quote to avoid path-dependent type staging issues
          val unwrapBuilder: Expr[A] => Expr[Inner] = isValueType.value.unwrap
          valueTypeWrap[A, Inner](isValueType.value) match {
            case Some(Left(plainWrap)) =>
              Log.info(s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint}") >>
                deriveShrinkRecursively[Inner](using shrinkctx.nest[Inner]).map { innerShrink =>
                  Rule.matched(Expr.quote {
                    _root_.org.scalacheck.Shrink { (a: A) =>
                      val inner: Inner = Expr.splice(unwrapBuilder(Expr.quote(a)))
                      Expr.splice(innerShrink).shrink(inner).map { shrunkInner =>
                        Expr.splice(plainWrap(Expr.quote(shrunkInner)))
                      }
                    }
                  })
                }
            case Some(Right(smartWrap)) =>
              // The smart constructor can reject a value: drop rejected candidates instead of casting the `Either`
              // to `A`.
              Log.info(
                s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint} (smart constructor)"
              ) >>
                deriveShrinkRecursively[Inner](using shrinkctx.nest[Inner]).map { innerShrink =>
                  Rule.matched(Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils.shrinkValueType(
                      Expr.splice(innerShrink),
                      (a: A) => Expr.splice(unwrapBuilder(Expr.quote(a))),
                      Expr.splice(smartWrap)
                    )
                  })
                }
            case None =>
              MIO.pure(Rule.yielded(unsupportedValueTypeWrap[A](isValueType.value)))
          }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
      }
  }
}
