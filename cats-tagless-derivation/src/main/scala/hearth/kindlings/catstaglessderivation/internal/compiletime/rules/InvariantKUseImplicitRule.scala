package hearth.kindlings.catstaglessderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait InvariantKUseImplicitRuleImpl {
  this: InvariantKMacrosImpl & MacroCommons & StdExtensions =>

  object InvariantKUseImplicitRule extends InvariantKDerivationRule("use implicit InvariantK") {
    def apply[Alg[_[_]]: InvariantKCtx]: MIO[Rule.Applicability[Expr[cats.tagless.InvariantK[Alg]]]] = {
      implicit val IKAlgType: Type[cats.tagless.InvariantK[Alg]] = ikctx.invariantKAlgType
      if (ikctx.derivedType.exists(_.Underlying =:= IKAlgType))
        MIO.pure(Rule.yielded(s"${IKAlgType.prettyPrint} is the self-type"))
      else
        ikctx.invariantKAlgType.summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            ikctx.cache
              .buildCachedWith(
                "cached-invariantk-instance",
                ValDefBuilder.ofLazy[cats.tagless.InvariantK[Alg]](
                  s"invariantK_${ikctx.invariantKAlgType.shortName}"
                )
              )(_ => instanceExpr) >>
              InvariantKUseCachedRule[Alg]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit ${IKAlgType.prettyPrint}: $reason"))
        }
    }
  }
}
