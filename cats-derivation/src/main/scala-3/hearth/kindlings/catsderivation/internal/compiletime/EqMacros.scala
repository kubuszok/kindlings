package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class EqMacros(q: Quotes) extends MacroCommonsScala3(using q), EqMacrosImpl
private[catsderivation] object EqMacros {

  def deriveEqImpl[A: Type](using q: Quotes): Expr[cats.kernel.Eq[A]] =
    new EqMacros(q).deriveEq[A]
}
