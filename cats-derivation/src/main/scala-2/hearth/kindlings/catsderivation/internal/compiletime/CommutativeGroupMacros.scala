package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class CommutativeGroupMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CommutativeGroupMacrosImpl {

  def deriveCommutativeGroupImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.CommutativeGroup[A]] =
    deriveCommutativeGroup[A]
}
