package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class EmptyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with EmptyMacrosImpl {

  def deriveEmptyImpl[A: c.WeakTypeTag]: c.Expr[alleycats.Empty[A]] = deriveEmpty[A]
}
