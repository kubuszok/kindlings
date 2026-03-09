package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class PartialOrderMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with PartialOrderMacrosImpl {

  def derivePartialOrderImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.PartialOrder[A]] = derivePartialOrder[A]
}
