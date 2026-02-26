package hearth.kindlings.circederivation

import io.circe.Codec
import scala.language.experimental.macros

private[circederivation] trait KindlingsCodecAsObjectCompanionCompat { this: KindlingsCodecAsObject.type =>

  def derive[A](implicit config: Configuration): Codec.AsObject[A] =
    macro internal.compiletime.CodecMacros.deriveCodecAsObjectImpl[A]

  implicit def derived[A](implicit config: Configuration): KindlingsCodecAsObject[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecAsObjectImpl[A]
}
