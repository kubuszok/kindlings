package hearth.kindlings.scalacheckderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import org.scalacheck.Cogen

trait CogenHandleAsValueTypeRuleImpl { this: CogenMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object CogenHandleAsValueTypeRule extends CogenDerivationRule("handle as value type when possible") {
    def apply[A: CogenCtx]: MIO[Rule.Applicability[Expr[Cogen[A]]]] =
      Type[A] match {
        case IsValueType(isValueType) =>
          import isValueType.Underlying as Inner
          implicit val CogenA: Type[Cogen[A]] = CogenTypes.Cogen[A]

          Log.info(s"Handling ${Type[A].prettyPrint} as value type wrapping ${Inner.prettyPrint}") >>
            deriveCogenRecursively[Inner](using cogenctx.nest[Inner]).map { innerCogen =>
              Rule.matched(Expr.quote {
                _root_.org.scalacheck.Cogen[A] { (seed: _root_.org.scalacheck.rng.Seed, a: A) =>
                  val inner: Inner = Expr.splice(isValueType.value.unwrap(Expr.quote(a)))
                  Expr.splice(innerCogen).perturb(seed, inner)
                }
              })
            }
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not a value type"))
      }
  }
}
