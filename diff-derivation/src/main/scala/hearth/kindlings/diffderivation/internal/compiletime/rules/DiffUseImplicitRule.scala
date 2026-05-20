package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*

trait DiffUseImplicitRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used|unused")
  object DiffUseImplicitRule extends DiffDerivationRule("use implicit Diff") {

    @scala.annotation.nowarn("msg=is never used|unused")
    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = {
      implicit val DiffA: Type[Diff[A]] = DiffTypes.Diff[A]
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      if (dctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        DiffA.summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) =>
            val expr = instanceExpr.asInstanceOf[Expr[Diff[A]]]
            dctx.cache.buildCachedWith(
              "cached-diff-instance",
              ValDefBuilder.ofLazy[Diff[A]](s"diff_${Type[A].shortName}")
            )(_ => expr) >>
              DiffUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Diff[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
