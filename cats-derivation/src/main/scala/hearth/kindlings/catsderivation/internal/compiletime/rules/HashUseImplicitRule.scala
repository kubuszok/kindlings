package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait HashUseImplicitRuleImpl {
  this: HashMacrosImpl & MacroCommons & StdExtensions & StrictDerivationSupport =>

  @scala.annotation.nowarn("msg=is never used")
  object HashUseImplicitRule extends HashDerivationRule("use implicit Hash") {

    def apply[A: HashCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val HashA: Type[cats.kernel.Hash[A]] = HashTypes.Hash[A]
      if (hctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        HashTypes.Hash[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            hctx.cache.buildCachedWith(
              "cached-hash-instance",
              ValDefBuilder.ofLazy[cats.kernel.Hash[A]](s"hash_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              HashUseCachedRule[A]
          case Left(reason) =>
            if (isStrictDerivationMode)
              MIO.fail(
                new StrictDerivationError(
                  s"Strict derivation mode: no implicit Hash[${Type[A].prettyPrint}] found. Provide an explicit instance or remove StrictDerivation from scope."
                )
              )
            else
              MIO.pure(Rule.yielded(s"No implicit Hash[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
