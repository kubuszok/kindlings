package hearth.kindlings.avroderivation

private[avroderivation] trait AvroEncoderCompanionCompat { this: AvroEncoder.type =>

  inline def derive[A](using config: AvroConfig): AvroEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderImpl[A]('config)
  }

  inline def encode[A](inline value: A)(using config: AvroConfig): Any = ${
    internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]('value, 'config)
  }

  inline given derived[A](using config: AvroConfig): AvroEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderImpl[A]('config)
  }
}
