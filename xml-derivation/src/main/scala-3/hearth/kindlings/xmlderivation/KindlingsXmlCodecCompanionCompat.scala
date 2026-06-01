package hearth.kindlings.xmlderivation

private[xmlderivation] trait KindlingsXmlCodecCompanionCompat { this: KindlingsXmlCodec.type =>

  inline given derived[A](using config: XmlConfig): KindlingsXmlCodec[A] =
    internal.runtime.XmlDerivationUtils.xmlCodec[A](
      KindlingsXmlEncoder.derived[A],
      KindlingsXmlDecoder.derived[A]
    )
}
