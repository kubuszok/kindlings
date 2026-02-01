package hearth.kindlings.fastshowpretty

import scala.language.experimental.macros

private[fastshowpretty] trait FastShowPrettyCompanionCompat { this: FastShowPretty.type =>

  /** Renders a value to a String with default indentation (2 spaces). */
  def render[A](value: A): String = macro internal.compiletime.FastShowPrettyMacros.deriveInlineImpl[A]

  /** Renders a value to a String with custom indentation configuration. */
  def renderWith[A](value: A, config: RenderConfig): String =
    macro internal.compiletime.FastShowPrettyMacros.deriveInlineWithConfigImpl[A]

  implicit def derived[A]: FastShowPretty[A] = macro internal.compiletime.FastShowPrettyMacros.deriveTypeClassImpl[A]
}
