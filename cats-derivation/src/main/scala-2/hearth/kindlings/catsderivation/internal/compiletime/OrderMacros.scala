package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class OrderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with OrderMacrosImpl {

  def deriveOrderImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Order[A]] = deriveOrder[A]
}
