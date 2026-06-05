package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FunctorKUseImplicitRuleImpl {
  this: FunctorKMacrosImpl & MacroCommons & StdExtensions =>

  object FunctorKUseImplicitRule extends FunctorKDerivationRule("use implicit FunctorK") {
    def apply[Alg[_[_]]: FunctorKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.FunctorK[Alg]]]] = {
      implicit val FKAlgType: Type[cats.tagless.FunctorK[Alg]] = fkctx.functorKAlgType
      if (fkctx.derivedType.exists(_.Underlying =:= FKAlgType))
        MIO.pure(Rule.yielded(s"${FKAlgType.prettyPrint} is the self-type"))
      else
        fkctx.functorKAlgType.summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            fkctx.cache
              .buildCachedWith(
                "cached-functork-instance",
                ValDefBuilder.ofLazy[cats.tagless.FunctorK[Alg]](
                  s"functorK_${fkctx.functorKAlgType.shortName}"
                )
              )(_ => instanceExpr) >>
              FunctorKUseCachedRule[Alg]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit ${FKAlgType.prettyPrint}: $reason"))
        }
    }
  }
}
