package hearth.kindlings.circederivation

import io.circe.{Encoder, Json, JsonObject}

private[circederivation] trait KindlingsEncoderCompanionCompat { this: KindlingsEncoder.type =>

  inline def deriveAsObject[A](using config: Configuration): Encoder.AsObject[A] = ${
    internal.compiletime.EncoderMacros.deriveEncoderAsObjectImpl[A]('config)
  }

  /** Like Scala 3 core circe's `ConfiguredEncoder.derive(...)`: the configuration given as named arguments. */
  inline def derive[A](
      transformMemberNames: String => String = Configuration.default.transformMemberNames,
      transformConstructorNames: String => String = Configuration.default.transformConstructorNames,
      discriminator: Option[String] = Configuration.default.discriminator,
      enumAsStrings: Boolean = Configuration.default.enumAsStrings,
      unwrapValueClasses: Boolean = Configuration.default.unwrapValueClasses
  ): KindlingsEncoder[A] =
    derived[A](using
      Configuration(
        transformMemberNames,
        transformConstructorNames,
        discriminator = discriminator,
        enumAsStrings = enumAsStrings,
        unwrapValueClasses = unwrapValueClasses
      )
    )

  /** Like Scala 3 core circe's `ConfiguredEnumEncoder.derive(transformConstructorNames)`: a sum of singletons (case
    * objects, parameterless enum cases, also nested) encoded as the (transformed) names of its cases, i.e.
    * `enumAsStrings`.
    */
  inline def deriveEnum[A](transformConstructorNames: String => String): KindlingsEncoder[A] =
    derived[A](using Configuration(transformConstructorNames = transformConstructorNames, enumAsStrings = true))

  /** [[deriveEnum]] with the default (identity) constructor names. (An overload rather than a default argument, so that
    * the configuration is known at compile time.)
    */
  inline def deriveEnum[A](): KindlingsEncoder[A] = derived[A](using Configuration(enumAsStrings = true))

  /** Explicit unwrapping (generic-extras' `deriveUnwrapped*`): a case class with exactly one field is coded as that
    * field's JSON. Automatic unwrapping only applies to `AnyVal` value classes.
    */
  inline def deriveUnwrapped[A](using config: Configuration): KindlingsEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveUnwrappedEncoderImpl[A]('config)
  }

  inline def encode[A](inline value: A)(using config: Configuration): Json = ${
    internal.compiletime.EncoderMacros.deriveInlineEncodeImpl[A]('value, 'config)
  }

  inline given derived[A](using config: Configuration): KindlingsEncoder[A] = ${
    internal.compiletime.EncoderMacros.deriveKindlingsEncoderImpl[A]('config)
  }
}
