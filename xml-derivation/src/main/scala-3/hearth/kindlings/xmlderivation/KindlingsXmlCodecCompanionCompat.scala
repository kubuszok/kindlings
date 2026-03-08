package hearth.kindlings.xmlderivation

private[xmlderivation] trait KindlingsXmlCodecCompanionCompat { this: KindlingsXmlCodec.type =>

  inline def derive[A](using config: XmlConfig): KindlingsXmlCodec[A] =
    internal.runtime.XmlDerivationUtils.xmlCodec[A](
      KindlingsXmlEncoder.derive[A],
      KindlingsXmlDecoder.derive[A]
    )

  inline given derived[A](using config: XmlConfig): KindlingsXmlCodec[A] =
    internal.runtime.XmlDerivationUtils.xmlCodec[A](
      KindlingsXmlEncoder.derive[A],
      KindlingsXmlDecoder.derive[A]
    )
}
