package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class CommutativeGroupMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      CommutativeGroupMacrosImpl
private[catsderivation] object CommutativeGroupMacros {

  def deriveCommutativeGroupImpl[A: Type](using q: Quotes): Expr[cats.kernel.CommutativeGroup[A]] =
    new CommutativeGroupMacros(q).deriveCommutativeGroup[A]
}
