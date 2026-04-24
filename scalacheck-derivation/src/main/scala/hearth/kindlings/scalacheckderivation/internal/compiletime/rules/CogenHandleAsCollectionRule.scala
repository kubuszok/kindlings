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
          import isCollection.value.CtorResult
          implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as Collection") >>
            deriveCogenRecursively[ElemType](using cogenctx.nest[ElemType]).flatMap { elemCogen =>
              MIO.pure(Rule.matched(Expr.quote {
                hearth.kindlings.scalacheckderivation.internal.runtime.CogenUtils
                  .cogenCollection(
                    Expr.splice(elemCogen).asInstanceOf[Cogen[Any]]
                  )
                  .asInstanceOf[Cogen[A]]
              }))
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Collection"))
      }
  }
}
