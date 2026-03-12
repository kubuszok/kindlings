package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait MonoidUseCachedRuleImpl {
  this: MonoidMacrosImpl & MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  object MonoidUseCachedRule extends MonoidDerivationRule("use cached Monoid") {

    def apply[A: MonoidCtx]: MIO[Rule.Applicability[MonoidDerivationResult[A]]] = {
      implicit val MonoidA: Type[cats.kernel.Monoid[A]] = MonoidTypes.Monoid[A]
      moidctx.cache.get0Ary[cats.kernel.Monoid[A]]("cached-monoid-instance").flatMap {
        case Some(instance) =>
          val empty = Expr.quote(Expr.splice(instance).empty)
          val combine: (Expr[A], Expr[A]) => MIO[Expr[A]] =
            (x, y) => MIO.pure(Expr.quote(Expr.splice(instance).combine(Expr.splice(x), Expr.splice(y))))
          MIO.pure(Rule.matched(MonoidDerivationResult(empty, combine)))
        case None =>
          MIO.pure(Rule.yielded(s"No cached Monoid for ${Type[A].prettyPrint}"))
      }
    }
  }
}
