package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Gen

trait ArbitraryHandleAsValueTypeRuleImpl { this: ArbitraryMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object ArbitraryHandleAsValueTypeRule extends ArbitraryDerivationRule("handle as value type when possible") {
    def apply[A: ArbitraryCtx]: MIO[Rule.Applicability[Expr[Gen[A]]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          implicit val GenA: Type[Gen[A]] = ArbitraryTypes.Gen[A]

          valueTypeWrap[A, Inner](isValueType.value) match {
            case Some(Left(plainWrap)) =>
              Log.info(s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint}") >>
                deriveArbitraryRecursively[Inner](using arbctx.nest[Inner]).map { innerGen =>
                  Rule.matched(Expr.quote {
                    Expr
                      .splice(innerGen)
                      .map { inner =>
                        Expr.splice(plainWrap(Expr.quote(inner)))
                      }
                      .asInstanceOf[Gen[A]]
                  })
                }
            case Some(Right(smartWrap)) =>
              // The smart constructor can reject a value: regenerate rejected candidates instead of casting the
              // `Either` to `A`.
              Log.info(
                s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint} (smart constructor)"
              ) >>
                deriveArbitraryRecursively[Inner](using arbctx.nest[Inner]).map { innerGen =>
                  Rule.matched(Expr.quote {
                    hearth.kindlings.scalacheckderivation.internal.runtime.ScalaCheckUtils
                      .genValueType(Expr.splice(innerGen), Expr.splice(smartWrap))
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
