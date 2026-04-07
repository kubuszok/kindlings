package hearth.kindlings.sconfigderivation

private[sconfigderivation] trait ConfigReaderCompanionCompat { this: ConfigReader.type =>

  inline def derive[A](using config: SConfig): ConfigReader[A] = ${
    internal.compiletime.ReaderMacros.deriveReaderImpl[A]('config)
  }

  inline given derived[A](using config: SConfig): ConfigReader[A] = ${
    internal.compiletime.ReaderMacros.deriveReaderImpl[A]('config)
  }
}
