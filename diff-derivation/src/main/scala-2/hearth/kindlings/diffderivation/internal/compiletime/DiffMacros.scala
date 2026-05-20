package hearth.kindlings.diffderivation
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[diffderivation] class DiffMacros(val c: blackbox.Context) extends MacroCommonsScala2 with DiffMacrosImpl {

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[Diff[A]] = deriveTypeClass[A]
}
