package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BoundedSemilatticeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with BoundedSemilatticeMacrosImpl {

  def deriveBoundedSemilatticeImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.BoundedSemilattice[A]] =
    deriveBoundedSemilattice[A]
}
