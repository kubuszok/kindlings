package hearth.kindlings.jsonfieldconfigext

import hearth.MacroCommons
import hearth.std.StdExtensions

/** Trait that provides a mutable registry for JSON field configuration providers.
  *
  * Mix this into your macro context to enable JSON config discovery via macro extensions. Extensions registered via
  * `ServiceLoader` will call `registerJsonFieldConfig` when they find their library's configuration in implicit scope.
  */
trait JsonFieldConfigSupport { this: MacroCommons & StdExtensions =>

  /** A resolved JSON field configuration from a specific JSON library.
    *
    * Implementations are created by library-specific macro extensions (e.g., circe, jsoniter) and registered into the
    * `JsonFieldConfigSupport` registry during macro expansion.
    */
  trait JsonFieldConfigProvider {

    /** Human-readable library name for diagnostics (e.g., "circe", "jsoniter-scala"). */
    def libraryName: String

    /** The UntypedType of this library's configuration class (e.g., `Configuration` for circe, `JsoniterConfig` for
      * jsoniter).
      *
      * Used by consumers (like tapir-schema-derivation) to disambiguate when multiple JSON configs are found, by
      * matching this against a user-provided preference type.
      */
    def configType: UntypedType

    /** Resolve the encoded JSON field name for a case class parameter.
      *
      * The implementation should check library-specific annotations (e.g., `@fieldName`) first, then fall back to the
      * library's configuration transform (e.g., `config.transformMemberNames`).
      *
      * @param param
      *   the field's Parameter from CaseClass parsing
      * @param scalaName
      *   the field's original Scala name
      * @return
      *   an expression producing the resolved JSON field name at runtime
      */
    def resolveFieldName(param: Parameter, scalaName: String): Expr[String]

    /** Whether a field should be excluded from serialization.
      *
      * Checks for library-specific transient annotations (e.g., `@transientField`).
      */
    def isTransientField(param: Parameter): Boolean

    /** Resolve the encoded name for a sealed trait subtype / ADT constructor.
      *
      * @param scalaName
      *   the subtype's short class name
      * @return
      *   an expression producing the resolved constructor name at runtime
      */
    def resolveConstructorName(scalaName: String): Expr[String]

    /** The discriminator field name expression from the JSON library's configuration, if configured.
      *
      * Returns `Expr[Option[String]]` because the discriminator value is typically a runtime configuration value (e.g.,
      * `config.discriminator`).
      */
    def discriminatorFieldName: Expr[Option[String]]

    /** Whether enums should be encoded as plain strings (vs. wrapped objects).
      *
      * Returns `Expr[Boolean]` because this is a runtime configuration value.
      */
    def enumAsStrings: Expr[Boolean]

    /** Whether default parameter values should be used for missing fields during decoding.
      *
      * Returns `Expr[Boolean]` because this is a runtime configuration value.
      */
    def useDefaults: Expr[Boolean]
  }

  // Mutable registry (same pattern as StdExtensions.ProvidedCompanion)
  private val _jsonFieldConfigs: scala.collection.mutable.ListBuffer[JsonFieldConfigProvider] =
    scala.collection.mutable.ListBuffer.empty[JsonFieldConfigProvider]

  /** Register a JSON field configuration provider from a library-specific macro extension. */
  def registerJsonFieldConfig(provider: JsonFieldConfigProvider): Unit =
    _jsonFieldConfigs += provider

  /** Get all registered JSON field configuration providers. */
  def allJsonFieldConfigs: List[JsonFieldConfigProvider] =
    _jsonFieldConfigs.toList
}
