package hearth.kindlings.pureconfigderivation

private[pureconfigderivation] trait KindlingsConfigReaderCompanionCompat { this: KindlingsConfigReader.type =>

  inline given derived[A](using config: PureConfig): KindlingsConfigReader[A] = ${
    internal.compiletime.ReaderMacros.deriveKindlingsReaderImpl[A]('config)
  }
}
