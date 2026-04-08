package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigReader

private[pureconfigderivation] trait KindlingsConfigReaderCompanionCompat { this: KindlingsConfigReader.type =>

  inline def derive[A](using config: PureConfig): ConfigReader[A] = ${
    internal.compiletime.ReaderMacros.deriveReaderImpl[A]('config)
  }

  inline given derived[A](using config: PureConfig): KindlingsConfigReader[A] = ${
    internal.compiletime.ReaderMacros.deriveKindlingsReaderImpl[A]('config)
  }
}
