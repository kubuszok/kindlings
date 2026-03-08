package hearth.kindlings.ubjsonderivation

import scala.language.experimental.macros

private[ubjsonderivation] trait KindlingsUBJsonValueCodecCompanionCompat {
  this: KindlingsUBJsonValueCodec.type =>

  def derive[A](implicit config: UBJsonConfig): UBJsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: UBJsonConfig): KindlingsUBJsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
