package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class CommutativeMonoidMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CommutativeMonoidMacrosImpl {

  def deriveCommutativeMonoidImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.CommutativeMonoid[A]] =
    deriveCommutativeMonoid[A]
}
