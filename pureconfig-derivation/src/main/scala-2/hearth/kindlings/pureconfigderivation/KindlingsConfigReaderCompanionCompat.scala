package hearth.kindlings.pureconfigderivation

import scala.language.experimental.macros

private[pureconfigderivation] trait KindlingsConfigReaderCompanionCompat { this: KindlingsConfigReader.type =>

  implicit def derived[A](implicit config: PureConfig): KindlingsConfigReader[A] =
    macro internal.compiletime.ReaderMacros.deriveKindlingsReaderImpl[A]
}
