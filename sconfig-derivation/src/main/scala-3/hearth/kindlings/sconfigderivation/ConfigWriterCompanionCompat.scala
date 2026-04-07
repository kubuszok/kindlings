package hearth.kindlings.sconfigderivation

private[sconfigderivation] trait ConfigWriterCompanionCompat { this: ConfigWriter.type =>

  inline def derive[A](using config: SConfig): ConfigWriter[A] = ${
    internal.compiletime.WriterMacros.deriveWriterImpl[A]('config)
  }

  inline given derived[A](using config: SConfig): ConfigWriter[A] = ${
    internal.compiletime.WriterMacros.deriveWriterImpl[A]('config)
  }
}
