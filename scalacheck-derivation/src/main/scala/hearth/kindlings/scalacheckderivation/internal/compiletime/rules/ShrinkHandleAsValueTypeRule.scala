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

          Log.info(s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint}") >>
            deriveShrinkRecursively[Inner](using shrinkctx.nest[Inner]).map { innerShrink =>
              // Build wrap/unwrap outside quote to avoid path-dependent type staging issues
              val unwrapBuilder: Expr[A] => Expr[Inner] = isValueType.value.unwrap
              val wrapBuilder: Expr[Inner] => Expr[A] = { innerExpr =>
                val raw = isValueType.value.wrap.apply(innerExpr)
                Expr.quote(Expr.splice(raw.asInstanceOf[Expr[Any]]).asInstanceOf[A])
              }
              Rule.matched(Expr.quote {
                _root_.org.scalacheck.Shrink { (a: A) =>
                  val inner: Inner = Expr.splice(unwrapBuilder(Expr.quote(a)))
                  Expr.splice(innerShrink).shrink(inner).map { shrunkInner =>
                    Expr.splice(wrapBuilder(Expr.quote(shrunkInner)))
                  }
                }
              })
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
      }
  }
}
