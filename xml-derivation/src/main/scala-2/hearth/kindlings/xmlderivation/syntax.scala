package hearth.kindlings.xmlderivation

import scala.language.experimental.macros

object syntax {

  implicit class XmlWriteOps[A](private val value: A) extends AnyVal {
    def toXmlString(elementName: String)(implicit config: XmlConfig): String =
      macro internal.compiletime.EncoderMacros.deriveInlineToXmlStringOpsImpl[A]
  }

  implicit class XmlReadOps(private val xml: String) extends AnyVal {
    def fromXmlString[A](implicit config: XmlConfig): Either[XmlDecodingError, A] =
      macro internal.compiletime.DecoderMacros.deriveInlineFromXmlStringOpsImpl[A]
  }
}
