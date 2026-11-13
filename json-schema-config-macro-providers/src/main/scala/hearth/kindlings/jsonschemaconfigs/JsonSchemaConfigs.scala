package hearth.kindlings.jsonschemaconfigs

import hearth.MacroCommons
import hearth.std.StdExtensions

/** Trait that provides a mutable registry for JSON schema configuration providers.
  *
  * Mix this into your macro context to enable JSON config discovery via macro extensions. Extensions registered via
  * `ServiceLoader` will call `JsonSchemaConfig.register` when they find their library's configuration in implicit
  * scope.
  */
trait JsonSchemaConfigs { this: MacroCommons & StdExtensions =>

  /** A resolved JSON schema configuration from a specific JSON library.
    *
    * Implementations are created by library-specific macro extensions (e.g., circe, jsoniter) and registered into the
    * `JsonSchemaConfigs` registry during macro expansion.
    */
  trait JsonSchemaConfig {

    /** Human-readable library name for diagnostics (e.g., "circe", "jsoniter-scala"). */
    def libraryName: String

    /** The UntypedType of this library's configuration class (e.g., `Configuration` for circe, `JsoniterConfig` for
      * jsoniter).
      *
      * Used by consumers (like tapir-schema-derivation) to disambiguate when multiple JSON configs are found, by
      * matching this against a user-provided preference type.
      */
    def configType: UntypedType

    /** The fully-qualified class name of this library's configuration type. Used to reconstruct a fresh UntypedType
      * from a new context (avoids stale caching).
      */
    def configClassName: String

    /** Recompute configType using a fresh MacroCommons context. ServiceLoader-cached extensions must NOT cache
      * UntypedType across expansions.
      */
    def freshConfigType(freshCtx: MacroCommons & StdExtensions): UntypedType =
      freshCtx.UntypedType.fromClassName(configClassName).asInstanceOf[UntypedType]

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

    /** Whether fields with default values should be marked as optional in the schema.
      *
      * When true, fields that have Scala default values are treated as optional in the generated schema, because the
      * JSON decoder will fill in defaults for missing fields.
      *
      * Circe: maps to `useDefaults`. Jsoniter: maps to `transientDefault`.
      */
    def fieldsWithDefaultsAreOptional: Expr[Boolean]

    /** Whether empty collection/map fields should be marked as optional in the schema.
      *
      * When true, fields typed as `Iterable[_]`, `Map[_,_]`, or `String` are treated as optional, because the JSON
      * encoder may omit them when empty.
      *
      * Circe: always false. Jsoniter: maps to `transientEmpty`.
      */
    def emptyFieldsAreOptional: Expr[Boolean]

    /** Whether maps should be encoded as arrays of key-value pairs instead of objects.
      *
      * When true, `Map[K, V]` produces an array schema of `[K, V]` tuples instead of an open product schema.
      *
      * Circe: always false. Jsoniter: maps to `mapAsArray`.
      */
    def mapsAreArrays: Expr[Boolean]

    /** Whether numeric fields should be represented as strings in the schema.
      *
      * When true, numeric field types get a `"string"` format annotation on their schema.
      *
      * Circe: always false. Jsoniter: maps to `isStringified`.
      */
    def numericFieldsAsStrings: Expr[Boolean]

    // The members below describe further encoding choices of a JSON library. They have defaults (keeping the schemas
    // as they were before they were added), so that implementations only override what their library does.

    /** Resolve the encoded name of `Child`, an ADT leaf / enum value of `Parent` (`scalaName` is its short name).
      *
      * Unlike [[resolveConstructorName]], an implementation can use the types, e.g. to read the leaf's annotations,
      * pass its fully-qualified name to the configured name mapper, or name Java enum / `scala.Enumeration` values as
      * the library does. Defaults to [[resolveConstructorName]].
      */
    def resolveSubtypeName[Parent: Type, Child: Type](scalaName: String): Expr[String] =
      resolveConstructorName(scalaName)

    /** Whether nested sealed hierarchies are flattened to their leaves (the codecs read and write only the leaves, e.g.
      * `{"type":"Truck"}` rather than an intermediate `MotorVehicle` subtype). Defaults to `false`.
      */
    def flattensNestedSubtypes: Boolean = false

    /** Whether Java enum and `scala.Enumeration` values are JSON strings. Defaults to [[enumAsStrings]]. */
    def valueEnumsAsStrings: Expr[Boolean] = enumAsStrings

    /** Whether `scala.Enumeration` values are encoded as their (integer) `id`s. Defaults to `false`. */
    def enumerationValuesAsIds: Expr[Boolean] = falseExpr

    /** Whether, without a discriminator, singleton subtypes (case objects) of ADTs which also have other subtypes are
      * JSON strings (their encoded names). Defaults to `false`.
      */
    def singletonSubtypesAsStrings: Expr[Boolean] = falseExpr

    /** Whether, without a discriminator, the subtypes of an ADT are wrapped in a single-field object whose key is the
      * subtype's encoded name (`{"Circle":{"radius":1.0}}`). Defaults to `false` (the subtypes are not wrapped).
      */
    def wrapsSubtypesWithoutDiscriminator: Expr[Boolean] = falseExpr

    /** Whether tuples are JSON arrays (`[1,"a"]`) rather than objects with `_1`, `_2`, ... fields. Defaults to `false`.
      */
    def tuplesAsArrays: Boolean = false

    /** How an `Option` nested directly in an `Option` is encoded, when the library encodes it as an ADT (`Some` with a
      * value field, and `None`), so that `Some(None)` and `None` differ. Defaults to `None` (encoded as an `Option`).
      */
    def nestedOptionsAsAdts: Option[NestedOptionEncoding] = None
  }

  /** An `Option` nested in an `Option`, encoded (when `enabled`) as an ADT of `Some` (an object with `valueFieldName`)
    * and `None` (a singleton), named `someName` and `noneName`, using the config's ADT encoding (discriminator,
    * wrapping, singletons as strings).
    */
  final class NestedOptionEncoding(
      val enabled: Expr[Boolean],
      val someName: Expr[String],
      val noneName: Expr[String],
      val valueFieldName: Expr[String]
  )

  private def falseExpr: Expr[Boolean] = Expr(false)

  object JsonSchemaConfig extends JsonSchemaConfigModule

  trait JsonSchemaConfigModule { this: JsonSchemaConfig.type =>

    private val _providers: scala.collection.mutable.ListBuffer[JsonSchemaConfig] =
      scala.collection.mutable.ListBuffer.empty[JsonSchemaConfig]

    /** Register a JSON schema configuration provider from a library-specific macro extension. */
    def register(provider: JsonSchemaConfig): Unit =
      _providers += provider

    /** Get all registered JSON schema configuration providers. */
    def all: List[JsonSchemaConfig] =
      _providers.toList
  }
}
