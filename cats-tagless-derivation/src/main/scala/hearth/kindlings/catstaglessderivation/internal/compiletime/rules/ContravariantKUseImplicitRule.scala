package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait ContravariantKUseImplicitRuleImpl {
  this: ContravariantKMacrosImpl & MacroCommons & StdExtensions =>

  object ContravariantKUseImplicitRule extends ContravariantKDerivationRule("use implicit ContravariantK") {
    def apply[Alg[_[_]]: ContravariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.ContravariantK[Alg]]]] = {
      implicit val CKAlgType: Type[cats.tagless.ContravariantK[Alg]] = ckctx.contravariantKAlgType
      if (ckctx.derivedType.exists(_.Underlying =:= CKAlgType))
        MIO.pure(Rule.yielded(s"${CKAlgType.prettyPrint} is the self-type"))
      else
        ckctx.contravariantKAlgType.summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            ckctx.cache
              .buildCachedWith(
                "cached-contravariantk-instance",
                ValDefBuilder.ofLazy[cats.tagless.ContravariantK[Alg]](
                  s"contravariantK_${ckctx.contravariantKAlgType.shortName}"
                )
              )(_ => instanceExpr) >>
              ContravariantKUseCachedRule[Alg]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit ${CKAlgType.prettyPrint}: $reason"))
        }
    }
  }
}
