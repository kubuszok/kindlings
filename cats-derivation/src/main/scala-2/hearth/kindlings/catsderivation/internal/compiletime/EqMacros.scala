package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class EqMacros(val c: blackbox.Context) extends MacroCommonsScala2 with EqMacrosImpl {

  def deriveEqImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Eq[A]] = deriveEq[A]
}
