package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigReader
import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigReaderCompanionCompat { this: KindlingsConfigReader.type =>

  def derive[A](implicit config: PureConfig): ConfigReader[A] =
    macro internal.compiletime.ReaderMacros.deriveReaderImpl[A]

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigReader[A] =
    macro internal.compiletime.ReaderMacros.deriveKindlingsReaderImpl[A]
}
