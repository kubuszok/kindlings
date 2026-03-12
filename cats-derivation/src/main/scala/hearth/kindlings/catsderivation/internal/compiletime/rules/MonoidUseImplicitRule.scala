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
            moidctx.cache.buildCachedWith(
              "cached-monoid-instance",
              ValDefBuilder.ofLazy[cats.kernel.Monoid[A]](s"monoid_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              MonoidUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Monoid[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
