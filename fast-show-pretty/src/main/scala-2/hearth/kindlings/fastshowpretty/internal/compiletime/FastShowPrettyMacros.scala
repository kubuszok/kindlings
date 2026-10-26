package hearth.kindlings.fastshowpretty
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[fastshowpretty] class FastShowPrettyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FastShowPrettyMacrosImpl
    with AnnotationSupport {

  def deriveInlineWithConfigImpl[A: c.WeakTypeTag](
      value: c.Expr[A],
      config: c.Expr[RenderConfig]
  ): c.Expr[String] = deriveInline[A](value, config)

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[FastShowPretty[A]] = deriveTypeClass[A]
}
