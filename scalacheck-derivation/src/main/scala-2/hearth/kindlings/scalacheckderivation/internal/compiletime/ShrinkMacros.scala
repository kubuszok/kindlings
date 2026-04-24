package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[scalacheckderivation] class ShrinkMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with LoadStandardExtensionsOnce
    with ShrinkMacrosImpl {

  def deriveShrinkImpl[A: c.WeakTypeTag]: c.Expr[org.scalacheck.Shrink[A]] = deriveShrink[A]
}
