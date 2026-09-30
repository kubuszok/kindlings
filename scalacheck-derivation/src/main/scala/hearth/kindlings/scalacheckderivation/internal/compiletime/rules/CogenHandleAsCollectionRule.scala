package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsCollectionRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object CogenHandleAsCollectionRule extends CogenDerivationRule("handle as Collection when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      Type[A] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as ElemType
          implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

          // Read through `asIterable` instead of casting to `Iterable` (issue #218).
          val toIterableFn = collectionToIterableFn[A, ElemType](isCollection.value)
          Log.info(s"Handling ${Type[A].prettyPrint} as Collection") >>
            deriveCogenRecursively[ElemType](using cogenctx.nest[ElemType]).map { elemCogen =>
              Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                  .cogenCollectionWithIterable(Expr.splice(elemCogen), Expr.splice(toIterableFn))
              })
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Collection"))
      }
  }
}
