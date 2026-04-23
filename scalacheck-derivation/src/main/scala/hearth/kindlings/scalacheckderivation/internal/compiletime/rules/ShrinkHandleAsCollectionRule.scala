package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Shrink

trait ShrinkHandleAsCollectionRuleImpl { this: ShrinkMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ShrinkHandleAsCollectionRule extends ShrinkDerivationRule("handle as Collection when possible") {
    def apply[A: ShrinkCtx]: MIO[Rule.Applicability[Expr[Shrink[A]]]] =
      Type[A] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as ElemType
          import isCollection.value.CtorResult
          implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as Collection") >>
            deriveShrinkRecursively[ElemType](using shrinkctx.nest[ElemType]).flatMap { elemShrink =>
              val factoryExpr = isCollection.value.factory
              // Cast factory to Any to avoid CtorResult type leakage in the runtime helper
              val factoryAsAny: Expr[Any] = Expr.quote(Expr.splice(factoryExpr).asInstanceOf[Any])

              MIO.pure(Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
                  .shrinkCollection(
                    Expr.splice(elemShrink).asInstanceOf[Shrink[Any]],
                    Expr.splice(factoryAsAny)
                  )
                  .asInstanceOf[Shrink[A]]
              }))
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Collection"))
      }
  }
}
