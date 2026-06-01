package hearth.kindlings.pureconfigderivation

import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigConvertCompanionCompat { this: KindlingsConfigConvert.type =>

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigConvert[A] =
    macro internal.compiletime.ConvertMacros.deriveKindlingsConvertImpl[A]
}
