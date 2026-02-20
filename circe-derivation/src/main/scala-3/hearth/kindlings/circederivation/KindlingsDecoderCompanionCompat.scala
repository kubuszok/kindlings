package hearth.kindlings.circederivation

import io.circe.{Decoder, DecodingFailure, HCursor}

private[circederivation] trait KindlingsDecoderCompanionCompat { this: KindlingsDecoder.type =>

  inline def derive[A](using config: Configuration): Decoder[A] = ${
    internal.compiletime.DecoderMacros.deriveDecoderImpl[A]('config)
  }

  inline def decode[A](cursor: HCursor)(using config: Configuration): Either[DecodingFailure, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]('cursor, 'config)
  }

  inline given derived[A](using config: Configuration): KindlingsDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]('config)
  }
}
