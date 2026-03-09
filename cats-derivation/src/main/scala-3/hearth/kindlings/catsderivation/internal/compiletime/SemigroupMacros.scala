package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class SemigroupMacros(q: Quotes) extends MacroCommonsScala3(using q), SemigroupMacrosImpl
private[catsderivation] object SemigroupMacros {

  def deriveSemigroupImpl[A: Type](using q: Quotes): Expr[cats.kernel.Semigroup[A]] =
    new SemigroupMacros(q).deriveSemigroup[A]
}
