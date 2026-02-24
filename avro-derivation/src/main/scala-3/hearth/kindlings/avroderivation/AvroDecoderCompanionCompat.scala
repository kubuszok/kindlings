package hearth.kindlings.avroderivation

private[avroderivation] trait AvroDecoderCompanionCompat { this: AvroDecoder.type =>

  inline def derive[A](using config: AvroConfig): AvroDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveDecoderImpl[A]('config)
  }

  inline def decode[A](value: Any)(using config: AvroConfig): A = ${
    internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]('value, 'config)
  }

  inline given derived[A](using config: AvroConfig): AvroDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveDecoderImpl[A]('config)
  }
}
