package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EqUseImplicitRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqUseImplicitRule extends EqDerivationRule("use implicit Eq") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] = {
      implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
      if (eqctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        EqTypes.Eq[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            eqctx.cache.buildCachedWith(
              "cached-eq-instance",
              ValDefBuilder.ofLazy[cats.kernel.Eq[A]](s"eq_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              EqUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Eq[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
