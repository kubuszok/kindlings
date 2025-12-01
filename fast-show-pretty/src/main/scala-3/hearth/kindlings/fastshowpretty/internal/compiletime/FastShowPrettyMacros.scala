package hearth.kindlings.fastshowpretty
package internal.compiletime

import hearth.MacroCommonsScala3
import scala.quoted.*

final private[fastshowpretty] class FastShowPrettyMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      FastShowPrettyMacrosImpl
private[fastshowpretty] object FastShowPrettyMacros {

  def deriveInlineImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[String] =
    new FastShowPrettyMacros(q).deriveInline[A](value)

  def deriveTypeClassImpl[A: Type](using q: Quotes): Expr[FastShowPretty[A]] =
    new FastShowPrettyMacros(q).deriveTypeClass[A]
}
