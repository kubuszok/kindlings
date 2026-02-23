package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import scala.language.experimental.macros

private[jsoniterderivation] trait KindlingsJsonValueCodecCompanionCompat {
  this: KindlingsJsonValueCodec.type =>

  def derive[A](implicit config: JsoniterConfig): JsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: JsoniterConfig): KindlingsJsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]
}
