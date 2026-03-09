package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class PartialOrderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      PartialOrderMacrosImpl
private[catsderivation] object PartialOrderMacros {

  def derivePartialOrderImpl[A: Type](using q: Quotes): Expr[cats.kernel.PartialOrder[A]] =
    new PartialOrderMacros(q).derivePartialOrder[A]
}
