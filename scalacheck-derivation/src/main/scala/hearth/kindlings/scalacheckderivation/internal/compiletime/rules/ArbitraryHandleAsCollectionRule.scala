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
          implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

          // Construct through the provider's `build` (not by treating `CtorResult` as `A`), see issue #218.
          collectionBuildFn[A, ElemType](isCollection.value) match {
            case Some(buildFn) =>
              Log.info(s"Handling ${Type[A].prettyPrint} as Collection with element type ${ElemType.prettyPrint}") >>
                deriveArbitraryRecursively[ElemType](using arbctx.nest[ElemType]).map { elemGen =>
                  Rule.matched(Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                      .genCollection(Expr.splice(elemGen), Expr.splice(buildFn))
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
