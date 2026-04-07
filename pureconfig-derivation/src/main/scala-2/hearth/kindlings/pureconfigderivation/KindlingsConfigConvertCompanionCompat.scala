package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigConvert
import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigConvertCompanionCompat { this: KindlingsConfigConvert.type =>

  def derive[A](implicit config: PureConfig): ConfigConvert[A] =
    macro internal.compiletime.ConvertMacros.deriveConvertImpl[A]

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigConvert[A] =
    macro internal.compiletime.ConvertMacros.deriveKindlingsConvertImpl[A]
}
