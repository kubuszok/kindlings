package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReaderException, JsonValueCodec}
import scala.language.experimental.macros

private[jsoniterderivation] trait KindlingsJsonValueCodecCompanionCompat {
  this: KindlingsJsonValueCodec.type =>

  @deprecated("Use .derived instead", "next")
  def derive[A](implicit config: JsoniterConfig): JsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveCodecImpl[A]

  implicit def derived[A](implicit config: JsoniterConfig): KindlingsJsonValueCodec[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecImpl[A]

  def writeToString[A](value: A)(implicit config: JsoniterConfig): String =
    macro internal.compiletime.CodecMacros.deriveInlineWriteToStringImpl[A]

  def readFromString[A](json: String)(implicit config: JsoniterConfig): Either[JsonReaderException, A] =
    macro internal.compiletime.CodecMacros.deriveInlineReadFromStringImpl[A]
}
