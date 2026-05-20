package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class BandMacros(val c: blackbox.Context) extends MacroCommonsScala2 with BandMacrosImpl {

  def deriveBandImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Band[A]] =
    deriveBand[A]
}
