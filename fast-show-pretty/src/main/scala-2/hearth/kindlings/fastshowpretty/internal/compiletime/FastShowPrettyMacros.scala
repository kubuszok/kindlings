package hearth.kindlings.fastshowpretty
package internal.compiletime

import hearth.MacroCommonsScala2
import scala.reflect.macros.blackbox

final private[fastshowpretty] class FastShowPrettyMacros(val c: blackbox.Context)
    extends MacroCommonsScala2
    with FastShowPrettyMacrosImpl {

  import c.universe.*

  def deriveInlineImpl[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = {
    val configExpr = c.Expr[RenderConfig](q"_root_.hearth.kindlings.fastshowpretty.RenderConfig.Default")
    val levelExpr = c.Expr[Int](q"0")
    deriveInline[A](value, configExpr, levelExpr)
  }

  def deriveInlineWithConfigImpl[A: c.WeakTypeTag](
      value: c.Expr[A],
      config: c.Expr[RenderConfig]
  ): c.Expr[String] = {
    val levelExpr = c.Expr[Int](q"$config.startLevel")
    deriveInline[A](value, config, levelExpr)
  }

  def deriveTypeClassImpl[A: c.WeakTypeTag]: c.Expr[FastShowPretty[A]] = deriveTypeClass[A]
}
