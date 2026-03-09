package hearth.kindlings.catsderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** PartialOrder derivation: delegates to Order derivation and wraps result in toDouble. */
trait PartialOrderMacrosImpl extends OrderMacrosImpl { this: MacroCommons & StdExtensions =>

  @scala.annotation.nowarn("msg=is never used")
  def derivePartialOrder[A: Type]: Expr[cats.kernel.PartialOrder[A]] = {
    val macroName = "PartialOrder.derived"
    implicit val PartialOrderA: Type[cats.kernel.PartialOrder[A]] = PartialOrderTypes.PartialOrder[A]
    implicit val IntType: Type[Int] = OrderTypes.Int
    implicit val DoubleType: Type[Double] = PartialOrderTypes.Double
    val selfType: Option[??] = Some(Type[A].as_??)

    // PartialOrder can be derived from Order — same lexicographic comparison, just wrapped in toDouble
    deriveOrderEntrypoint[A, cats.kernel.PartialOrder[A]](macroName) { fromCtx =>
      Expr.quote {
        new cats.kernel.PartialOrder[A] {
          def partialCompare(x: A, y: A): Double = {
            val _ = x
            val _ = y
            Expr.splice {
              val result = fromCtx(OrderCtx.from(Expr.quote(x), Expr.quote(y), derivedType = selfType))
              Expr.quote(Expr.splice(result).toDouble)
            }
          }
        }
      }
    }
  }

  protected object PartialOrderTypes {
    def PartialOrder: Type.Ctor1[cats.kernel.PartialOrder] = Type.Ctor1.of[cats.kernel.PartialOrder]
    val Double: Type[Double] = Type.of[Double]
  }
}
