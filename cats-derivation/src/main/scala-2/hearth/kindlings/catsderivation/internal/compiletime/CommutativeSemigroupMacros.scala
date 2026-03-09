package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class CommutativeSemigroupMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CommutativeSemigroupMacrosImpl {

  def deriveCommutativeSemigroupImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.CommutativeSemigroup[A]] =
    deriveCommutativeSemigroup[A]
}
