package hearth.kindlings.fastshowpretty

import scala.language.experimental.macros

private[fastshowpretty] trait FastShowPrettyCompanionCompat { this: FastShowPretty.type =>

  def render[A](value: A): String = macro internal.compiletime.FastShowPrettyMacros.deriveInlineImpl[A]

  implicit def derived[A]: FastShowPretty[A] = macro internal.compiletime.FastShowPrettyMacros.deriveTypeClassImpl[A]
}
