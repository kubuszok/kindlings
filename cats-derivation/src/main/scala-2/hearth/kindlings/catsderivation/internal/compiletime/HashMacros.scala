package hearth.kindlings.catsderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[catsderivation] class HashMacros(val c: blackbox.Context) extends MacroCommonsScala2 with HashMacrosImpl {

  def deriveHashImpl[A: c.WeakTypeTag]: c.Expr[cats.kernel.Hash[A]] = deriveHash[A]
}
