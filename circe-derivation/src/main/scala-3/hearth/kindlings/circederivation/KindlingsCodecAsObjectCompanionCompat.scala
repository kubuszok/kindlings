package hearth.kindlings.circederivation

import io.circe.Codec

private[circederivation] trait KindlingsCodecAsObjectCompanionCompat { this: KindlingsCodecAsObject.type =>

  inline given derived[A](using config: Configuration): KindlingsCodecAsObject[A] =
    internal.runtime.CirceDerivationUtils.codecAsObject[A](
      KindlingsEncoder.deriveAsObject[A],
      KindlingsDecoder.derived[A]
    )
}
