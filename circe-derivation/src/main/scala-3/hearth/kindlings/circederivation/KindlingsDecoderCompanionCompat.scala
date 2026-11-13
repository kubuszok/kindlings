package hearth.kindlings.circederivation

import io.circe.{Decoder, DecodingFailure, HCursor, Json}

private[circederivation] trait KindlingsDecoderCompanionCompat extends KindlingsDecoderBinaryCompat {
  this: KindlingsDecoder.type =>

  inline def decode[A](json: Json)(using config: Configuration): Either[DecodingFailure, A] = ${
    internal.compiletime.DecoderMacros.deriveInlineDecodeImpl[A]('json, 'config)
  }

  /** A decoder of patches for a case class, like circe-generic's `deriveFor[A].patch`: each field is optional, an
    * absent or `null` key keeps the original value, and a value of the wrong type fails at decode time.
    */
  inline def patch[A](using config: Configuration): Decoder[A => A] = ${
    internal.compiletime.DecoderMacros.derivePatchDecoderImpl[A]('config)
  }

  /** A decoder of a case class with some fields supplied later, like circe-generic's `deriveFor[F].incomplete` and
    * generic-extras' `deriveConfiguredFor[F].incomplete`: `F` is a function type `(P1, ..., Pn) => C`, each `Pi` stands
    * for the first remaining field of `C` of that type, and the other fields are decoded from the JSON object.
    * {{{
    * case class Qux(i: Int, a: String, j: Int)
    * val decoder: Decoder[Int => Qux] = KindlingsDecoder.incomplete[Int => Qux] // `i` is supplied later
    * }}}
    */
  inline def incomplete[F](using config: Configuration): Decoder[F] = ${
    internal.compiletime.DecoderMacros.deriveIncompleteDecoderImpl[F]('config)
  }

  /** Like Scala 3 core circe's `ConfiguredDecoder.derive(...)`: the configuration given as named arguments. */
  inline def derive[A](
      transformMemberNames: String => String = Configuration.default.transformMemberNames,
      transformConstructorNames: String => String = Configuration.default.transformConstructorNames,
      useDefaults: Boolean = Configuration.default.useDefaults,
      discriminator: Option[String] = Configuration.default.discriminator,
      strictDecoding: Boolean = Configuration.default.strictDecoding,
      enumAsStrings: Boolean = Configuration.default.enumAsStrings,
      unwrapValueClasses: Boolean = Configuration.default.unwrapValueClasses
  ): KindlingsDecoder[A] =
    derived[A](using
      Configuration(
        transformMemberNames,
        transformConstructorNames,
        useDefaults,
        discriminator,
        strictDecoding,
        enumAsStrings,
        unwrapValueClasses
      )
    )

  /** Like Scala 3 core circe's `ConfiguredEnumDecoder.derive(transformConstructorNames)`: a sum of singletons (case
    * objects, parameterless enum cases, also nested) decoded from the (transformed) names of its cases, i.e.
    * `enumAsStrings`.
    */
  inline def deriveEnum[A](transformConstructorNames: String => String): KindlingsDecoder[A] =
    derived[A](using Configuration(transformConstructorNames = transformConstructorNames, enumAsStrings = true))

  /** [[deriveEnum]] with the default (identity) constructor names. (An overload rather than a default argument, so that
    * the configuration is known at compile time.)
    */
  inline def deriveEnum[A](): KindlingsDecoder[A] = derived[A](using Configuration(enumAsStrings = true))

  /** Explicit unwrapping (generic-extras' `deriveUnwrapped*`): a case class with exactly one field is coded as that
    * field's JSON. Automatic unwrapping only applies to `AnyVal` value classes.
    */
  inline def deriveUnwrapped[A](using config: Configuration): KindlingsDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveUnwrappedDecoderImpl[A]('config)
  }

  inline given derived[A](using config: Configuration): KindlingsDecoder[A] = ${
    internal.compiletime.DecoderMacros.deriveKindlingsDecoderImpl[A]('config)
  }
}
