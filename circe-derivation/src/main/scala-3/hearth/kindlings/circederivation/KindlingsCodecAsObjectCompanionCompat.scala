package hearth.kindlings.circederivation

import io.circe.Codec

private[circederivation] trait KindlingsCodecAsObjectCompanionCompat { this: KindlingsCodecAsObject.type =>

  inline def derive[A](using config: Configuration): Codec.AsObject[A] =
    internal.runtime.CirceDerivationUtils.codecAsObject[A](
      KindlingsEncoder.deriveAsObject[A],
      KindlingsDecoder.derive[A]
    )

  inline given derived[A](using config: Configuration): KindlingsCodecAsObject[A] =
    internal.runtime.CirceDerivationUtils.codecAsObject[A](
      KindlingsEncoder.deriveAsObject[A],
      KindlingsDecoder.derive[A]
    )
}
