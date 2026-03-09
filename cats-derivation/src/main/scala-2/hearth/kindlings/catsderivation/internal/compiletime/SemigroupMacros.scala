package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class SemigroupMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with SemigroupMacrosImpl {

  def deriveSemigroupImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Semigroup[A]] = deriveSemigroup[A]
}
