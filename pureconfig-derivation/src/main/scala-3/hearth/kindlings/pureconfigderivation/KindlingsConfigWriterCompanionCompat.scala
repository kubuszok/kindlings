package hearth.kindlings.pureconfigderivation

private[pureconfigderivation] trait KindlingsConfigWriterCompanionCompat { this: KindlingsConfigWriter.type =>

  inline given derived[A](using config: PureConfig): KindlingsConfigWriter[A] = ${
    internal.compiletime.WriterMacros.deriveKindlingsWriterImpl[A]('config)
  }
}
