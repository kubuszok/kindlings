package hearth.kindlings.scalacheckderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[scalacheckderivation] class CogenMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with CogenMacrosImpl {

  def deriveCogenImpl[A: c.WeakTypeTag]: c.Expr[org.scalacheck.Cogen[A]] = deriveCogen[A]
}
