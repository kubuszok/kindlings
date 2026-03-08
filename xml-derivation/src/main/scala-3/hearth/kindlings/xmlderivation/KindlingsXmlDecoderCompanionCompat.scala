package hearth.kindlings.xmlderivation

private[xmlderivation] trait KindlingsXmlDecoderCompanionCompat { this: KindlingsXmlDecoder.type =>

  inline def derive[A](using config: XmlConfig): XmlDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveDecoderImpl[A]('config)
  }

  inline def decode[A](elem: scala.xml.Elem)(using config: XmlConfig): Either[XmlDecodingError, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]('elem, 'config)
  }

  inline def fromXmlString[A](xml: String)(using config: XmlConfig): Either[XmlDecodingError, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineFromXmlStringImpl[A]('xml, 'config)
  }

  inline given derived[A](using config: XmlConfig): KindlingsXmlDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]('config)
  }
}
