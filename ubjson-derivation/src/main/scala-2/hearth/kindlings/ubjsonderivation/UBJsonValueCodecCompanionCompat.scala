package hearth.kindlings.ubjsonderivation

import scala.language.experimental.macros

private[ubjsonderivation] trait UBJsonValueCodecCompanionCompat {
  this: UBJsonValueCodec.type =>

  def derive[A](implicit config: UBJsonConfig): UBJsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]
}
