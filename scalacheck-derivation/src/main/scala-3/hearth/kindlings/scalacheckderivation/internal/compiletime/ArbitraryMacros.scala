package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[scalacheckderivation] class ArbitraryMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      LoadStandardExtensionsOnce,
      ArbitraryMacrosImpl

private[scalacheckderivation] object ArbitraryMacros {

  def deriveArbitrary[A: Type](using q: Quotes): Expr[org.scalacheck.Arbitrary[A]] =
    new ArbitraryMacros(q).deriveArbitrary[A]
}
