package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait OrderUseCachedRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderUseCachedRule extends OrderDerivationRule("use cached Order") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val OrderA: Type[cats.kernel.Order[A]] = OrderTypes.Order[A]
      octx.cache.get0Ary[cats.kernel.Order[A]]("cached-order-instance").flatMap {
        case Some(instance) =>
          MIO.pure(
            Rule.matched(Expr.quote(Expr.splice(instance).compare(Expr.splice(octx.x), Expr.splice(octx.y))))
          )
        case None =>
          implicit val IntType: Type[Int] = OrderTypes.Int
          octx.cache.get2Ary[A, A, Int]("cached-order-method").flatMap {
            case Some(helper) => MIO.pure(Rule.matched(helper(octx.x, octx.y)))
            case None         => MIO.pure(Rule.yielded(s"No cached Order for ${Type[A].prettyPrint}"))
          }
      }
    }
  }
}
