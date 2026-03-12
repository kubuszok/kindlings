package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait MonoidUseImplicitRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object MonoidUseImplicitRule extends MonoidDerivationRule("use implicit Monoid") {
    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] = {
      implicit val MonoidA: Type[cats.kernel.Monoid[A]] = MonoidTypes.Monoid[A]
      if (moidctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        MonoidTypes.Monoid[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            val empty = Expr.quote(Expr.splice(instanceExpr).empty)
            val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] = (x, y) =>
              MIO.pure(Expr.quote(Expr.splice(instanceExpr).combine(Expr.splice(x), Expr.splice(y))))
            MIO.pure(Rule.matched(MonoidDerivationResult(empty, combine)))
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Monoid[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
