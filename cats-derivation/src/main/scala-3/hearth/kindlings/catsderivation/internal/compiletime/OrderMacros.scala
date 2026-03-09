package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class OrderMacros(q: Quotes) extends MacroCommonsScala3(using q), OrderMacrosImpl
private[catsderivation] object OrderMacros {

  def deriveOrderImpl[A: Type](using q: Quotes): Expr[cats.kernel.Order[A]] =
    new OrderMacros(q).deriveOrder[A]
}
