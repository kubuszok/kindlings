package hearth.kindlings.catsderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait OrderUseImplicitRuleImpl {
  this: OrderMacrosImpl & MacroCommons & StdExtensions =>

  object OrderUseImplicitRule extends OrderDerivationRule("use implicit Order") {
    def apply[A: OrderCtx]: MIO[Rule.Applicability[Expr[Int]]] = {
      implicit val OrderA: Type[cats.kernel.Order[A]] = OrderTypes.Order[A]
      if (octx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is the self-type"))
      else
        OrderTypes.Order[A].summonExprIgnoring().toEither match {
          case Right(instanceExpr) =>
            octx.cache.buildCachedWith(
              "cached-order-instance",
              ValDefBuilder.ofLazy[cats.kernel.Order[A]](s"order_${Type[A].shortName}")
            )(_ => instanceExpr) >>
              OrderUseCachedRule[A]
          case Left(reason) =>
            MIO.pure(Rule.yielded(s"No implicit Order[${Type[A].prettyPrint}]: $reason"))
        }
    }
  }
}
