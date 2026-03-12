package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EmptyUseImplicitRuleImpl {
  this: EmptyMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object EmptyUseImplicitRule extends EmptyDerivationRule("use implicit Empty") {

    def apply[A: EmptyCtx]: MIO[Rule.Applicability[Expr[A]]] = {
      implicit val EmptyA: Type[alleycats.Empty[A]] = EmptyTypes.Empty[A]
      if (ectx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        EmptyTypes.Empty[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            ectx.cache.buildCachedWith(
              "cached-empty-instance",
              ValDefBuilder.ofLazy[alleycats.Empty[A]](s"empty_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              EmptyUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Empty[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
