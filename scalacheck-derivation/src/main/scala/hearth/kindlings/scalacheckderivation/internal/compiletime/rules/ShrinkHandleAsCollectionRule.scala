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
          implicit val ShrinkA: Type[Shrink[A]] = ShrinkTypes.Shrink[A]

          // Read through `asIterable` and rebuild candidates through `build`, dropping the rejected ones (issue #218).
          collectionBuildFn[A, ElemType](isCollection.value) match {
            case Some(buildFn) =>
              val toIterableFn = collectionToIterableFn[A, ElemType](isCollection.value)
              Log.info(s"Handling ${Type[A].prettyPrint} as Collection") >>
                deriveShrinkRecursively[ElemType](using shrinkctx.nest[ElemType]).map { elemShrink =>
                  Rule.matched(Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ShrinkUtils
                      .shrinkCollectionWithBuild(
                        Expr.splice(elemShrink),
                        Expr.splice(toIterableFn),
                        Expr.splice(buildFn)
                      )
                  })
                }
            case None =>
              MIO.pure(Rule.yielded(unsupportedCollectionBuild[A](isCollection.value)))
          }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Collection"))
      }
  }
}
