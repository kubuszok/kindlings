package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupUseImplicitRuleImpl {
  this: SemigroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object SemigroupUseImplicitRule extends SemigroupDerivationRule("use implicit Semigroup") {
    def apply[A: SemigroupCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val SemigroupA: Type[cats.kernel.Semigroup[A]] = SemigroupTypes.Semigroup[A]
      if (sgctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        SemigroupTypes.Semigroup[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            MIO.pure(
              Rule.matched(
                Expr.quote(Expr.splice(instanceExpr).combine(Expr.splice(sgctx.x), Expr.splice(sgctx.y)))
              )
            )
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Semigroup[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
