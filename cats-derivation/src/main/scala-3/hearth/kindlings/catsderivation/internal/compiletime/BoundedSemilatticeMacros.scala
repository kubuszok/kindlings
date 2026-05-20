package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BoundedSemilatticeMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      BoundedSemilatticeMacrosImpl
private[catsderivation] object BoundedSemilatticeMacros {

  def deriveBoundedSemilatticeImpl[A: Type](using q: Quotes): Expr[cats.kernel.BoundedSemilattice[A]] =
    new BoundedSemilatticeMacros(q).deriveBoundedSemilattice[A]
}
