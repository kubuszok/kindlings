package hearth.kindlings.pureconfigderivation

import pureconfig.ConfigWriter

private[pureconfigderivation] trait KindlingsConfigWriterCompanionCompat { this: KindlingsConfigWriter.type =>

  inline def derive[A](using config: PureConfig): ConfigWriter[A] = ${
    internal.compiletime.WriterMacros.deriveWriterImpl[A]('config)
  }

  inline given derived[A](using config: PureConfig): KindlingsConfigWriter[A] = ${
    internal.compiletime.WriterMacros.deriveKindlingsWriterImpl[A]('config)
  }
}
