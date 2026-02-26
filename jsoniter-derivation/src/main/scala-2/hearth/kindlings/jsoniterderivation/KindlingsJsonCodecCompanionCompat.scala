package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonCodec, JsonKeyCodec}
import scala.language.experimental.macros

private[jsoniterderivation] trait KindlingsJsonCodecCompanionCompat { this: KindlingsJsonCodec.type =>

  def derive[A](implicit config: JsoniterConfig): JsonCodec[A] =
    macro internal.compiletime.CodecMacros.deriveJsonCodecImpl[A]

  def deriveKeyCodec[A](implicit config: JsoniterConfig): JsonKeyCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKeyCodecImpl[A]

  implicit def derived[A](implicit config: JsoniterConfig): KindlingsJsonCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsJsonCodecImpl[A]
}
