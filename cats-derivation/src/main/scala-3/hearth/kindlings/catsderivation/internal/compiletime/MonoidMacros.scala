package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class MonoidMacros(q: Quotes) extends MacroCommonsScala3(using q), MonoidMacrosImpl
private[catsderivation] object MonoidMacros {

  def deriveMonoidImpl[A: Type](using q: Quotes): Expr[cats.kernel.Monoid[A]] =
    new MonoidMacros(q).deriveMonoid[A]
}
