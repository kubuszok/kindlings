package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsCollectionRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryHandleAsCollectionRule extends ArbitraryDerivationRule("handle as Collection when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Type[A] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as ElemType
          import isCollection.value.CtorResult

          Log.info(s"Handling ${Type[A].prettyPrint} as Collection with element type ${ElemType.prettyPrint}") >>
            deriveArbitraryRecursively[ElemType](using arbctx.nest[ElemType]).flatMap { elemGen =>
              // Get the factory expression for building the collection
              val factoryExpr = isCollection.value.factory

              MIO.pure(Rule.matched(Expr.quote {
                // Use Gen.sized to halve the size budget for element generation,
                // preventing infinite recursion on types like List[TreeNode]
                _root_.org.scalacheck.Gen
                  .sized { n =>
                    _root_.org.scalacheck.Gen.resize(
                      scala.math.max(n / 2, 0),
                      _root_.org.scalacheck.Gen.listOf(Expr.splice(elemGen))
                    )
                  }
                  .map { list =>
                    Expr.splice(factoryExpr).fromSpecific(list).asInstanceOf[CtorResult]
                  }
                  .asInstanceOf[Gen[A]]
              }))
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a Collection"))
      }
  }
}
