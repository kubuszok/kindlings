package hearth.kindlings.fastshowpretty

private[fastshowpretty] trait FastShowPrettyCompanionCompat { this: FastShowPretty.type =>

  inline def render[A](inline value: A): String = ${
    internal.compiletime.FastShowPrettyMacros.deriveInlineImpl[A]('{ value })
  }

  inline given derived[A]: FastShowPretty[A] = ${ internal.compiletime.FastShowPrettyMacros.deriveTypeClassImpl[A] }
}
