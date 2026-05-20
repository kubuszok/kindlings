package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ShowPrettyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ShowPrettyMacrosImpl {

  def deriveShowPrettyImpl[A: c.WeakTypeTag]: c.Expr[hearth.kindlings.catsderivation.ShowPretty[A]] =
    deriveShowPretty[A]
}
