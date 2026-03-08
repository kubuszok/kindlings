package hearth.kindlings.xmlderivation

object syntax {

  extension [A](value: A) {
    inline def toXmlString(elementName: String)(using config: XmlConfig): String = ${
      internal.compiletime.EncoderMacros.deriveInlineToXmlStringImpl[A]('value, 'elementName, 'config)
    }
  }

  extension (xml: String) {
    inline def fromXmlString[A](using config: XmlConfig): Either[XmlDecodingError, A] = ${
      internal.compiletime.DecoderMacros.deriveInlineFromXmlStringImpl[A]('xml, 'config)
    }
  }
}
