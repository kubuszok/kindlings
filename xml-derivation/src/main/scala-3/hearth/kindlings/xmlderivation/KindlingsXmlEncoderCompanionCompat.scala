package hearth.kindlings.xmlderivation

private[xmlderivation] trait KindlingsXmlEncoderCompanionCompat { this: KindlingsXmlEncoder.type =>

  inline def derive[A](using config: XmlConfig): XmlEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderImpl[A]('config)
  }

  inline def encode[A](inline value: A, elementName: String)(using config: XmlConfig): scala.xml.Elem = ${
    internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]('value, 'elementName, 'config)
  }

  inline def toXmlString[A](inline value: A, elementName: String)(using config: XmlConfig): String = ${
    internal.compiletime.EncoderMacros.deriveInlineToXmlStringImpl[A]('value, 'elementName, 'config)
  }

  inline given derived[A](using config: XmlConfig): KindlingsXmlEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]('config)
  }
}
