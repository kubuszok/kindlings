package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[scalacheckderivation] class ArbitraryMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with ArbitraryMacrosImpl
    with LoadStandardExtensionsOnce {

  def deriveArbitraryImpl[A: c.WeakTypeTag]: c.Expr[org.scalacheck.Arbitrary[A]] = deriveArbitrary[A]
}
