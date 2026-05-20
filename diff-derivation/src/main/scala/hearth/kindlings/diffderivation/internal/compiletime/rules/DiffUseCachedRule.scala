package hearth.kindlings.diffderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*
import hearth.kindlings.diffderivation.*

trait DiffUseCachedRuleImpl { this: DiffMacrosImpl & MacroCommons & StdExtensions =>

  object DiffUseCachedRule extends DiffDerivationRule("use cached Diff") {

    def apply[A: DiffCtx]: MIO[Rule.Applicability[Expr[DiffResult]]] = {
      implicit val DRT: Type[DiffResult] = DiffTypes.DiffResultType
      implicit val DiffA: Type[Diff[A]] = DiffTypes.Diff[A]
      dctx.cache.get0Ary[Diff[A]]("cached-diff-instance").flatMap {
        case Some(instance) =>
          MIO.pure(
            Rule.matched(
              Expr.quote(Expr.splice(instance).diff(Expr.splice(dctx.left), Expr.splice(dctx.right)))
            )
          )
        case None =>
          dctx.cache.get2Ary[A, A, DiffResult]("cached-diff-method").map {
            case Some(helper) =>
              Rule.matched(helper(dctx.left, dctx.right))
            case None =>
              Rule.yielded(s"No cached Diff for ${Type[A].prettyPrint}")
          }
      }
    }
  }
}
