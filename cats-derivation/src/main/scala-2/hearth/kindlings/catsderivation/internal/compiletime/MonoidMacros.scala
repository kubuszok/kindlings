package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class MonoidMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with MonoidMacrosImpl {

  def deriveMonoidImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Monoid[A]] = deriveMonoid[A]
}
