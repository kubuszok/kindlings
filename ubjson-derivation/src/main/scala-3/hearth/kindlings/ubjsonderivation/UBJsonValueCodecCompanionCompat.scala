package hearth.kindlings.ubjsonderivation

private[ubjsonderivation] trait UBJsonValueCodecCompanionCompat {
  this: UBJsonValueCodec.type =>

  inline def derive[A](using config: UBJsonConfig): UBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }

  inline given derived[A](using config: UBJsonConfig): UBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }
}
