package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class BandMacros(q: Quotes) extends MacroCommonsScala3(using q), BandMacrosImpl
private[catsderivation] object BandMacros {

  def deriveBandImpl[A: Type](using q: Quotes): Expr[cats.kernel.Band[A]] =
    new BandMacros(q).deriveBand[A]
}
