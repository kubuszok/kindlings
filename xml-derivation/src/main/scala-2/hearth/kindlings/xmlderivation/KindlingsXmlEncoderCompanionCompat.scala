package hearth.kindlings.xmlderivation

import scala.language.experimental.macros

private[xmlderivation] trait KindlingsXmlEncoderCompanionCompat { this: KindlingsXmlEncoder.type =>

  def derive[A](implicit config: XmlConfig): XmlEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveEncoderImpl[A]

  def encode[A](value: A, elementName: String)(implicit config: XmlConfig): scala.xml.Elem =
    macro internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]

  def toXmlString[A](value: A, elementName: String)(implicit config: XmlConfig): String =
    macro internal.compiletime.EncoderMacros.deriveInlineToXmlStringImpl[A]

  implicit def derived[A](implicit config: XmlConfig): KindlingsXmlEncoder[A] =
    macro internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]
}
