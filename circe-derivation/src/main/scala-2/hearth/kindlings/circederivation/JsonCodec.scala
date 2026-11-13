package hearth.kindlings.circederivation

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.language.experimental.macros

/** Scala 2 macro annotation, like circe-generic's `@JsonCodec`: adds Kindlings-derived instances to the companion of a
  * case class or sealed trait, with `Configuration.default` (a configuration in scope is ignored, as upstream).
  *
  * `@JsonCodec` adds a `KindlingsCodecAsObject`, `@JsonCodec(encodeOnly = true)` an `Encoder.AsObject` and
  * `@JsonCodec(decodeOnly = true)` a `Decoder`. Type parameters get `Encoder`/`Decoder` evidence, as upstream.
  *
  * Requires `-Ymacro-annotations`. On Scala 3, use `derives KindlingsCodecAsObject` (or `KindlingsEncoder` /
  * `KindlingsDecoder`) instead.
  */
@compileTimeOnly("enable macro annotations (-Ymacro-annotations) to expand @JsonCodec")
final class JsonCodec(encodeOnly: Boolean = false, decodeOnly: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro internal.compiletime.JsonCodecMacros.jsonCodec
}

/** Scala 2 macro annotation, like circe-generic-extras' `@ConfiguredJsonCodec`: as [[JsonCodec]], but derived with the
  * implicit [[Configuration]] in scope at the definition site. Requires `-Ymacro-annotations`.
  */
@compileTimeOnly("enable macro annotations (-Ymacro-annotations) to expand @ConfiguredJsonCodec")
final class ConfiguredJsonCodec(encodeOnly: Boolean = false, decodeOnly: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro internal.compiletime.JsonCodecMacros.configuredJsonCodec
}
