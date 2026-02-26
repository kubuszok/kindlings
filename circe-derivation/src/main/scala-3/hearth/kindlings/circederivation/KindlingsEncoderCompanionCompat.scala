package hearth.kindlings.circederivation

import io.circe.{Encoder, Json, JsonObject}

private[circederivation] trait KindlingsEncoderCompanionCompat { this: KindlingsEncoder.type =>

  inline def derive[A](using config: Configuration): Encoder[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderImpl[A]('config)
  }

  inline def deriveAsObject[A](using config: Configuration): Encoder.AsObject[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderAsObjectImpl[A]('config)
  }

  inline def encode[A](inline value: A)(using config: Configuration): Json = ${
    internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]('value, 'config)
  }

  inline given derived[A](using config: Configuration): KindlingsEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]('config)
  }
}
