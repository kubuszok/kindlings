package hearth.kindlings.xmlderivation

import scala.language.experimental.macros

private[xmlderivation] trait KindlingsXmlDecoderCompanionCompat { this: KindlingsXmlDecoder.type =>

  def derive[A](implicit config: XmlConfig): XmlDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveDecoderImpl[A]

  def decode[A](elem: scala.xml.Elem)(implicit config: XmlConfig): Either[XmlDecodingError, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]

  def fromXmlString[A](xml: String)(implicit config: XmlConfig): Either[XmlDecodingError, A] =
    macro internal.compiletime.DecoderMacros.deriveInlineFromXmlStringImpl[A]

  implicit def derived[A](implicit config: XmlConfig): KindlingsXmlDecoder[A] =
    macro internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]
}
