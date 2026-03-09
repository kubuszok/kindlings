package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class ShowMacros(val c: blackbox.Context) extends MacroCommonsScala2 with ShowMacrosImpl {

  def deriveShowImpl[A: c.WeakTypeTag]: c.Expr[cats.Show[A]] = deriveShow[A]
}
