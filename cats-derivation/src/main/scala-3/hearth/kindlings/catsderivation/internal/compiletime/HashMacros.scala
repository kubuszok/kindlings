package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[catsderivation] class HashMacros(q: Quotes) extends MacroCommonsScala3(using q), HashMacrosImpl
private[catsderivation] object HashMacros {

  def deriveHashImpl[A: Type](using q: Quotes): Expr[cats.kernel.Hash[A]] =
    new HashMacros(q).deriveHash[A]
}
