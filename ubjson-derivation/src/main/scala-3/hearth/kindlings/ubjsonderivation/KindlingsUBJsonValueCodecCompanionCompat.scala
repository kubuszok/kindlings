package hearth.kindlings.ubjsonderivation

private[ubjsonderivation] trait KindlingsUBJsonValueCodecCompanionCompat {
  this: KindlingsUBJsonValueCodec.type =>

  inline def derive[A](using config: UBJsonConfig): UBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveCodecImpl[A]('config)
  }

  inline given derived[A](using config: UBJsonConfig): KindlingsUBJsonValueCodec[A] = ${
    internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]('config)
  }
}
