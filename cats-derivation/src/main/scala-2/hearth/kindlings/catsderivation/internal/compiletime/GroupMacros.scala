package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class GroupMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with GroupMacrosImpl {

  def deriveGroupImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Group[A]] =
    deriveGroup[A]
}
