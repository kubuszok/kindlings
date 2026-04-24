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

          Log.info(s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint}") >>
            deriveArbitraryRecursively[Inner](using arbctx.nest[Inner]).map { innerGen =>
              // wrap is a compile-time CtorLikeOf — apply it inside Expr.quote
              // Use the runtime helper which casts at JVM erasure level
              // Build the wrap expression outside the quote to avoid path-dependent type staging issues
              val wrapExprBuilder: Expr[Inner] => Expr[A] = { innerExpr =>
                val raw = isValueType.value.wrap.apply(innerExpr)
                Expr.quote(Expr.splice(raw.asInstanceOf[Expr[Any]]).asInstanceOf[A])
              }
              Rule.matched(Expr.quote {
                Expr
                  .splice(innerGen)
                  .map { inner =>
                    Expr.splice(wrapExprBuilder(Expr.quote(inner)))
                  }
                  .asInstanceOf[Gen[A]]
              })
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
      }
  }
}
