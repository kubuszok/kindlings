package hearth.kindlings.circederivation

import io.circe.Codec

private[circederivation] trait KindlingsCodecAsObjectCompanionCompat { this: KindlingsCodecAsObject.type =>

  /** Like Scala 3 core circe's `ConfiguredCodec.derive(...)`: the configuration given as named arguments. */
  inline def derive[A](
      transformMemberNames: String => String = Configuration.default.transformMemberNames,
      transformConstructorNames: String => String = Configuration.default.transformConstructorNames,
      useDefaults: Boolean = Configuration.default.useDefaults,
      discriminator: Option[String] = Configuration.default.discriminator,
      strictDecoding: Boolean = Configuration.default.strictDecoding,
      unwrapValueClasses: Boolean = Configuration.default.unwrapValueClasses
  ): KindlingsCodecAsObject[A] =
    derived[A](using
      Configuration(
        transformMemberNames,
        transformConstructorNames,
        useDefaults,
        discriminator,
        strictDecoding,
        unwrapValueClasses = unwrapValueClasses
      )
    )

  /** Like Scala 3 core circe's `ConfiguredEnumCodec.derive(transformConstructorNames)`: a codec of a sum of singletons
    * (case objects, parameterless enum cases, also nested) as the (transformed) names of its cases, i.e.
    * `enumAsStrings`. It is a plain `Codec` (not an object codec), since the values are JSON strings.
    */
  inline def deriveEnum[A](transformConstructorNames: String => String): Codec[A] =
    Codec.from(
      KindlingsDecoder.deriveEnum[A](transformConstructorNames),
      KindlingsEncoder.deriveEnum[A](transformConstructorNames)
    )

  /** [[deriveEnum]] with the default (identity) constructor names. */
  inline def deriveEnum[A](): Codec[A] = Codec.from(KindlingsDecoder.deriveEnum[A](), KindlingsEncoder.deriveEnum[A]())

  inline given derived[A](using config: Configuration): KindlingsCodecAsObject[A] =
    internal.runtime.CirceDerivationUtils.codecAsObject[A](
      KindlingsEncoder.deriveAsObject[A],
      KindlingsDecoder.derived[A]
    )
}
