package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class EmptyMacros(q: Quotes) extends MacroCommonsScala3(using q), EmptyMacrosImpl
private[catsderivation] object EmptyMacros {

  def deriveEmptyImpl[A: Type](using q: Quotes): Expr[alleycats.Empty[A]] =
    new EmptyMacros(q).deriveEmpty[A]
}
