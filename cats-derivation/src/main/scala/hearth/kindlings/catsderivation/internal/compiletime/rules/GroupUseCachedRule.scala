package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait GroupUseCachedRuleImpl {
  this: GroupMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object GroupUseCachedRule extends GroupDerivationRule("use cached Group") {

    def apply[A: GroupCtx]: MIO[Rule.Applicability[GroupDerivationResult[A]]] = {
      implicit val GroupA: Type[cats.kernel.Group[A]] = GroupTypes.Group[A]
      gctx.cache.get0Ary[cats.kernel.Group[A]]("cached-group-instance").flatMap {
        case Some(instance) =>
          val empty = Expr.quote(Expr.splice(instance).empty)
          val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] =
            (x, y) => MIO.pure(Expr.quote(Expr.splice(instance).combine(Expr.splice(x), Expr.splice(y))))
          val inverse: Expr[A] => MIO[Expr[A]] =
            a => MIO.pure(Expr.quote(Expr.splice(instance).inverse(Expr.splice(a))))
          MIO.pure(Rule.matched(GroupDerivationResult(empty, combine, inverse)))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Group for ${Type[A].prettyPrint}"))
      }
    }
  }
}
