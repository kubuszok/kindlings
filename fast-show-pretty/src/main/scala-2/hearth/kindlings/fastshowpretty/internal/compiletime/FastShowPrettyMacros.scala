package hearth.kindlings.fastshowpretty
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

private[fastshowpretty] final class FastShowPrettyMacros(val c: blackbox.Context) extends MacroCommonsScala2 with FastShowPrettyMacrosImpl {

  def deriveInlineImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = deriveInline[A](value)

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[FastShowPretty[A]] = deriveTypeClass[A]
}
