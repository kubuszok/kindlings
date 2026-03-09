package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class ShowMacros(q: Quotes) extends MacroCommonsScala3(using q), ShowMacrosImpl
private[catsderivation] object ShowMacros {

  def deriveShowImpl[A: Type](using q: Quotes): Expr[cats.Show[A]] =
    new ShowMacros(q).deriveShow[A]
}
