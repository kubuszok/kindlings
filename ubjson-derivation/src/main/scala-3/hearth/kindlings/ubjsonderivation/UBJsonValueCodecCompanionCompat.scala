package hearth.kindlings.ubjsonderivation

private[ubjsonderivation] trait UBJsonValueCodecCompanionCompat {
  this: UBJsonValueCodec.type =>

  @deprecated("Use .derived instead", "next")
  inline def derive[A](using config: UBJsonConfig): UBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }

  inline given derived[A](using config: UBJsonConfig): UBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }
}
