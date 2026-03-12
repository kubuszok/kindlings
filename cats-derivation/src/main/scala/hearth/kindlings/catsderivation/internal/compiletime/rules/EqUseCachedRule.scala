package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait EqUseCachedRuleImpl {
  this: EqMacrosImpl & MacroCommons & StdExtensions =>

  object EqUseCachedRule extends EqDerivationRule("use cached Eq") {
    def apply[A: EqCtx]: MIO[Rule.Applicability[Expr[Boolean]]] = {
      implicit val EqA: Type[cats.kernel.Eq[A]] = EqTypes.Eq[A]
      eqctx.cache.get0Ary[cats.kernel.Eq[A]]("cached-eq-instance").flatMap {
        case Some(instance) =>
          MIO.pure(Rule.matched(Expr.quote(Expr.splice(instance).eqv(Expr.splice(eqctx.x), Expr.splice(eqctx.y)))))
        case None =>
          implicit val BooleanType: Type[Boolean] = EqTypes.Boolean
          eqctx.cache.get2Ary[A, A, Boolean]("cached-eq-method").flatMap {
            case Some(helper) =>
              MIO.pure(Rule.matched(helper(eqctx.x, eqctx.y)))
            case None =>
              MIO.pure(Rule.yielded(s"No cached Eq for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
