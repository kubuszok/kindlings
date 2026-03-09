package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class CommutativeMonoidMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      CommutativeMonoidMacrosImpl
private[catsderivation] object CommutativeMonoidMacros {

  def deriveCommutativeMonoidImpl[A: Type](using q: Quotes): Expr[cats.kernel.CommutativeMonoid[A]] =
    new CommutativeMonoidMacros(q).deriveCommutativeMonoid[A]
}
