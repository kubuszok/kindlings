package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class SemilatticeMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemilatticeMacrosImpl {

  def deriveSemilatticeImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Semilattice[A]] =
    deriveSemilattice[A]
}
