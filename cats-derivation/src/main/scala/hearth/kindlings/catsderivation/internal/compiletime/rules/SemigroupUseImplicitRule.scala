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
            sgctx.cache.buildCachedWith(
              "cached-semigroup-instance",
              ValDefBuilder.ofLazy[cats.kernel.Semigroup[A]](s"semigroup_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              SemigroupUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Semigroup[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
