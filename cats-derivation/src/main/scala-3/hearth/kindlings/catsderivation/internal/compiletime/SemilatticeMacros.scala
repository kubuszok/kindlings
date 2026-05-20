package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class SemilatticeMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      SemilatticeMacrosImpl
private[catsderivation] object SemilatticeMacros {

  def deriveSemilatticeImpl[A: Type](using q: Quotes): Expr[cats.kernel.Semilattice[A]] =
    new SemilatticeMacros(q).deriveSemilattice[A]
}
