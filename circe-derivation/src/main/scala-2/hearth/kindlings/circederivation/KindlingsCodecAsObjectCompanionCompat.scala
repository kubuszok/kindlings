package hearth.kindlings.circederivation

import scala.language.experimental.macros

private[circederivation] trait KindlingsCodecAsObjectCompanionCompat { this: KindlingsCodecAsObject.type =>

  implicit def derived[A](implicit config: Configuration): KindlingsCodecAsObject[A] =
    macro internal.compiletime.CodecMacros.deriveKindlingsCodecAsObjectImpl[A]
}
