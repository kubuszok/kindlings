package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait GroupUseImplicitRuleImpl {
  this: GroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object GroupUseImplicitRule extends GroupDerivationRule("use implicit Group") {
    def apply[A: GroupCtx]: MIO[Rule.Applicability[GroupDerivationResult[A]]] = {
      implicit val GroupA: Type[cats.kernel.Group[A]] = GroupTypes.Group[A]
      if (gctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        GroupTypes.Group[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            gctx.cache.buildCachedWith(
              "cached-group-instance",
              ValDefBuilder.ofLazy[cats.kernel.Group[A]](s"group_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              GroupUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Group[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
