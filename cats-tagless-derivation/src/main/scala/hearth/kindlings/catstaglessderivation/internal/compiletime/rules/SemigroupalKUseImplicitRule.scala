package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait SemigroupalKUseImplicitRuleImpl {
  this: SemigroupalKMacrosImpl & MacroCommons & StdExtensions =>

  object SemigroupalKUseImplicitRule extends SemigroupalKDerivationRule("use implicit SemigroupalK") {
    def apply[Alg[_[_]]: SemigroupalKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.SemigroupalK[Alg]]]] = {
      implicit val SKAlgType: Type[cats.tagless.SemigroupalK[Alg]] = skctx.semigroupalKAlgType
      if (skctx.derivedType.exists(_.Underlying =:= SKAlgType))
        MIO.pure(Rule.yielded(s"${SKAlgType.prettyPrint} is the self-type"))
      else
        skctx.semigroupalKAlgType.summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            skctx.cache
              .buildCachedWith(
                "cached-semigroupalk-instance",
                ValDefBuilder.ofLazy[cats.tagless.SemigroupalK[Alg]](
                  s"semigroupalK_${skctx.semigroupalKAlgType.shortName}"
                )
              )(_ => instanceExpr) >>
              SemigroupalKUseCachedRule[Alg]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit ${SKAlgType.prettyPrint}: $reason"))
        }
    }
  }
}
