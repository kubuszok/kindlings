package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class CommutativeSemigroupMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      CommutativeSemigroupMacrosImpl
private[catsderivation] object CommutativeSemigroupMacros {

  def deriveCommutativeSemigroupImpl[A: Type](using q: Quotes): Expr[cats.kernel.CommutativeSemigroup[A]] =
    new CommutativeSemigroupMacros(q).deriveCommutativeSemigroup[A]
}
