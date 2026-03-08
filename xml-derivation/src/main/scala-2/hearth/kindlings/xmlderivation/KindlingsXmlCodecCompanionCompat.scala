package hearth.kindlings.xmlderivation

import scala.language.experimental.macros

private[xmlderivation] trait KindlingsXmlCodecCompanionCompat { this: KindlingsXmlCodec.type =>

  def derive[A](implicit config: XmlConfig): KindlingsXmlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: XmlConfig): KindlingsXmlCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
