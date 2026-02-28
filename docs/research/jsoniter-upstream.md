# Jsoniter-Scala Upstream Library Research

> Research date: 2026-02-28
> Sources: GitHub repository, Javadoc, README, test specs, source code
> Repository: https://github.com/plokhotnyuk/jsoniter-scala

## Library Version

- jsoniter-scala: **2.38.9** (latest release as of research date)
- Scala versions: 2.12.x, 2.13.x, 3.3+ (LTS)
- Platforms: JVM (Java 11+), Scala.js 1.0+, Scala Native 0.5+
- GraalVM native-image: supported

## Module Structure

| Module | Artifact | Purpose |
|--------|----------|---------|
| `jsoniter-scala-core` | Runtime | JsonReader, JsonWriter, JsonValueCodec, JsonKeyCodec, ReaderConfig, WriterConfig, exceptions |
| `jsoniter-scala-macros` | Compile-time only | JsonCodecMaker, CodecMakerConfig, annotations, macro-generated codecs |
| `jsoniter-scala-circe` | Optional | Circe integration / booster |

Runtime dependency: only `scala-library` (all platforms) + `scala-java-time` (Scala.js / Scala Native for `java.time.*`).

---

## Core Traits

### JsonValueCodec[A]

```scala
trait JsonValueCodec[@sp A] {
  def decodeValue(in: JsonReader, default: A): A
  def encodeValue(x: A, out: JsonWriter): Unit
  def nullValue: A
}
```

### JsonKeyCodec[A]

```scala
trait JsonKeyCodec[@sp A] {
  def decodeKey(in: JsonReader): A
  def encodeKey(x: A, out: JsonWriter): Unit
}
```

### JsonCodec[A]

```scala
trait JsonCodec[@sp A] extends JsonValueCodec[A] with JsonKeyCodec[A]
```

---

## CodecMakerConfig -- All Fields

31 configuration fields (as of 2.38.x). The companion object `CodecMakerConfig` extends the class with all defaults, serving as the base for the builder pattern.

| # | Field | Type | Default | Description |
|---|-------|------|---------|-------------|
| 1 | `fieldNameMapper` | `PartialFunction[String, String]` | `partialIdentity` | Mapping from case class field name to JSON key. Predefined: `enforce_snake_case`, `enforce_snake_case2`, `enforce-kebab-case`, `enforce-kebab-case2`, `enforceCamelCase`, `EnforcePascalCase` |
| 2 | `javaEnumValueNameMapper` | `PartialFunction[String, String]` | `partialIdentity` | Mapping from Java enum value name to JSON string |
| 3 | `adtLeafClassNameMapper` | `PartialFunction[String, String]` (or `String => String`) | `simpleClassName` | Mapping from case class/object full name to discriminator field value |
| 4 | `discriminatorFieldName` | `Option[String]` | `Some("type")` | Name of discriminator field for ADTs. `None` = wrapper-object encoding |
| 5 | `isStringified` | `Boolean` | `false` | Stringify number/boolean values of collections, options, and value classes |
| 6 | `mapAsArray` | `Boolean` | `false` | Serialize/parse maps as JSON arrays `[[k,v],...]` instead of JSON objects |
| 7 | `skipUnexpectedFields` | `Boolean` | `true` | Skip unexpected fields during parsing. When `false`, throws parse exception on unknown fields |
| 8 | `transientDefault` | `Boolean` | `true` | Skip serialization of fields whose values equal their constructor defaults |
| 9 | `transientEmpty` | `Boolean` | `true` | Skip serialization of fields with empty arrays/collections |
| 10 | `transientNone` | `Boolean` | `true` | Skip serialization of fields with `None` option values |
| 11 | `requireCollectionFields` | `Boolean` | `false` | Require collection/array fields in JSON input (throws if missing). Forces serialization of empty collections |
| 12 | `bigDecimalPrecision` | `Int` | `34` | Precision for `BigDecimal` values (34 = decimal128 precision) |
| 13 | `bigDecimalScaleLimit` | `Int` | `6178` | Exclusive limit for accepted scale in `BigDecimal` (6178 = decimal128 range) |
| 14 | `bigDecimalDigitsLimit` | `Int` | `308` | Exclusive limit for accepted mantissa digits before rounding |
| 15 | `bigIntDigitsLimit` | `Int` | `308` | Exclusive limit for accepted decimal digits in `BigInt` values |
| 16 | `bitSetValueLimit` | `Int` | `1024` | Exclusive limit for accepted numeric values in bit sets |
| 17 | `mapMaxInsertNumber` | `Int` | `1024` | Max number of inserts into maps (DoS protection) |
| 18 | `setMaxInsertNumber` | `Int` | `1024` | Max number of inserts into sets, excluding bit sets (DoS protection) |
| 19 | `allowRecursiveTypes` | `Boolean` | `false` | Allow recursive type definitions. Off by default to prevent stack overflow with untrusted input |
| 20 | `requireDiscriminatorFirst` | `Boolean` | `true` | Require discriminator field to be the first field of the JSON object. When `false`, discriminator can appear anywhere |
| 21 | `useScalaEnumValueId` | `Boolean` | `false` | Use numeric IDs instead of names for Scala Enumeration values |
| 22 | `skipNestedOptionValues` | `Boolean` | `false` | Skip some values for options nested more than 2 times |
| 23 | `circeLikeObjectEncoding` | `Boolean` | `false` | Serialize Scala objects as `{"ClassName":{}}` instead of `"ClassName"` in ADTs |
| 24 | `decodingOnly` | `Boolean` | `false` | Generate only the decoding implementation (no encoder) |
| 25 | `encodingOnly` | `Boolean` | `false` | Generate only the encoding implementation (no decoder) |
| 26 | `requireDefaultFields` | `Boolean` | `false` | Require fields with default values to be present in JSON input |
| 27 | `checkFieldDuplication` | `Boolean` | `true` | Check for duplicated field names during parsing. When `false`, generates smaller/faster codecs |
| 28 | `scalaTransientSupport` | `Boolean` | `false` | Support Scala's `@transient` annotation (in addition to jsoniter's own `@transient`) |
| 29 | `inlineOneValueClasses` | `Boolean` | `false` | Inline non-value classes that have a primary constructor with just one argument |
| 30 | `alwaysEmitDiscriminator` | `Boolean` | `false` | Always emit the discriminator field, even when it could be inferred |
| 31 | `transientNull` | `Boolean` | `false` | Skip serialization of fields with null values (relevant for Scala 3 union types with null) |

### Builder Pattern

Each field has a corresponding `with<FieldName>(value): CodecMakerConfig` method. All methods return a new immutable instance via internal `copy()`.

### PrintCodec

`CodecMakerConfig.PrintCodec` is a special variant that outputs the generated codec source code at compile time for inspection/debugging.

---

## Annotations

Only **3 annotations** are defined in the library, all in the `com.github.plokhotnyuk.jsoniter_scala.macros` package:

| Annotation | Target | Parameters | Purpose |
|------------|--------|------------|---------|
| `@named(name: String)` | Field (constructor param) or class | `name: String` | Override the JSON field name / ADT discriminator value |
| `@transient` | Field (constructor param) | None | Exclude field from parsing and serialization. Field must have a default value |
| `@stringified` | Field (constructor param) | None | Read/write numeric or boolean field values from/to string values |

### `@named` Details

- On a **field**: overrides the JSON key used for serialization/deserialization
- On a **case class/object** (ADT leaf): overrides the discriminator value
- Applied via `@field` meta-annotation (targets constructor parameters)
- The name parameter must be a compile-time constant (no references to uncompiled code)

### `@transient` Details

- The annotated field MUST have a default value in the constructor
- The field is completely excluded from both parsing and serialization
- Different from `transientDefault` config: `@transient` always excludes the field, `transientDefault` only skips serialization when the value equals the default

### `@stringified` Details

- Applies to numeric types (`Int`, `Long`, `BigDecimal`, etc.) and `Boolean`
- The JSON value is read/written as a string: `{"age":"25"}` instead of `{"age":25}`
- Works on fields in case classes and on elements in collections/options/value classes

---

## JsonCodecMaker -- Entry Points

### Primary Method

```scala
// Scala 3
inline def make[A]: JsonValueCodec[A]
inline def make[A](inline config: CodecMakerConfig): JsonValueCodec[A]

// Scala 2
def make[A]: JsonValueCodec[A]  // macro
def make[A](config: CodecMakerConfig): JsonValueCodec[A]  // macro
```

### Convenience Methods

| Method | Equivalent Config |
|--------|-------------------|
| `makeWithoutDiscriminator[A]` | `withDiscriminatorFieldName(None)` |
| `makeWithRequiredCollectionFields[A]` | `withRequireCollectionFields(true)` |
| `makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName[A]` | `withRequireCollectionFields(true).withDiscriminatorFieldName(Some("name"))` |
| `makeWithRequiredDefaultFields[A]` | `withRequireDefaultFields(true)` |
| `makeWithSkipNestedOptionValues[A]` | `withSkipNestedOptionValues(true)` |
| `makeCirceLike[A]` | Circe-compatible defaults |
| `makeCirceLikeSnakeCased[A]` | Circe-compatible + snake_case |
| `makeOpenapiLike[A]` | OpenAPI-compatible defaults |
| `makeOpenapiLike[A](discriminatorFieldName)` | OpenAPI + custom discriminator |
| `makeOpenapiLike[A](discriminatorFieldName, adtLeafClassNameMapper)` | OpenAPI + custom discriminator + mapper |
| `makeOpenapiLikeWithoutDiscriminator[A]` | OpenAPI without discriminator |

### ConfiguredJsonValueCodec (Scala 3 only, `derives` support)

```scala
trait ConfiguredJsonValueCodec[A] extends JsonValueCodec[A]

object ConfiguredJsonValueCodec:
  inline def derived[A](using inline config: CodecMakerConfig = CodecMakerConfig):
    ConfiguredJsonValueCodec[A]
```

Usage with `derives`:

```scala
given CodecMakerConfig = CodecMakerConfig.withFieldNameMapper(enforce_snake_case)
enum Color derives ConfiguredJsonValueCodec:
  case Red, Green, Blue
```

**Important**: Requires runtime dependency on `jsoniter-scala-macros` despite being compile-time macro.

### Name Mapping Functions

| Function | Example Output |
|----------|----------------|
| `partialIdentity` | `fieldName` -> `fieldName` |
| `enforceCamelCase` | `field_name` -> `fieldName` |
| `EnforcePascalCase` | `field_name` -> `FieldName` |
| `enforce_snake_case` | `fieldName` -> `field_name` |
| `enforce_snake_case2` | `fieldName` -> `field_name` (variant) |
| `` `enforce-kebab-case` `` | `fieldName` -> `field-name` |
| `` `enforce-kebab-case2` `` | `fieldName` -> `field-name` (variant) |
| `simpleClassName` | `com.example.Foo$Bar` -> `Bar` |

---

## Supported Types (Detailed)

### Primitives and Boxed

`Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String` and their boxed Java equivalents (`java.lang.Boolean`, `java.lang.Integer`, etc.)

### Numeric Types

- `BigInt` -- configurable digit limit (default 308)
- `BigDecimal` -- configurable precision (34), scale limit (6178), digit limit (308)

### Standard Library Types

- `Option[A]` -- `None` serialized as null (or skipped with `transientNone`)
- `Either[L, R]` -- requires custom codec
- `Unit` -- supported

### Java Types

- `java.util.UUID` -- serialized as string
- `java.time.Duration`
- `java.time.Instant`
- `java.time.LocalDate`
- `java.time.LocalDateTime`
- `java.time.LocalTime`
- `java.time.MonthDay`
- `java.time.OffsetDateTime`
- `java.time.OffsetTime`
- `java.time.Period`
- `java.time.Year`
- `java.time.YearMonth`
- `java.time.ZonedDateTime`
- `java.time.ZoneId`
- `java.time.ZoneOffset`
- All `java.time.*` types use **ISO-8601 representation only**

### Java Enumerations

- `java.lang.Enum` subclasses
- Supports custom name mapping via `javaEnumValueNameMapper`
- Compile-time error on duplicated transformed names
- Serialized as string values

### Tuples

- Standard tuples `Tuple1` through `Tuple22`
- Scala 3: tuples with arity > 22 (backed by `TupleXXL`)
- Scala 3: generic tuples with `*:` / `EmptyTuple`
- Scala 3: `Tuple.Concat`, `Tuple.Append`, `Tuple.Drop`, `Tuple.Take`, `Tuple.Zip`, `Tuple.Map`, `Tuple.InverseMap`
- Scala 3.7+: **Named tuples** with field names as JSON keys (encoded as JSON object, not array)
- Tuples serialized as JSON arrays (unnamed) or JSON objects (named)

### Collections -- Immutable

| Type | Notes |
|------|-------|
| `List[A]` | |
| `::[A]` | Non-empty list; parse error on empty input |
| `Vector[A]` | |
| `Seq[A]` | |
| `IndexedSeq[A]` | |
| `Set[A]` | |
| `SortedSet[A]` | |
| `immutable.TreeSet[A]` | |
| `immutable.ListSet[A]` | |
| `immutable.HashSet[A]` | |
| `immutable.HashMap[K, V]` | |
| `immutable.ListMap[K, V]` | |
| `immutable.SortedMap[K, V]` | |
| `immutable.TreeMap[K, V]` | |
| `immutable.IntMap[V]` | Int keys |
| `immutable.LongMap[V]` | Long keys |
| `immutable.BitSet` | |
| `Stream[A]` / `LazyList[A]` | |
| `Queue[A]` | |
| `Iterable[A]` | |
| `ImmutableArray[A]` | Scala 3 only |

### Collections -- Mutable

| Type | Notes |
|------|-------|
| `mutable.ArrayBuffer[A]` | |
| `mutable.Buffer[A]` | |
| `mutable.ListBuffer[A]` | |
| `mutable.ArraySeq[A]` | |
| `mutable.Seq[A]` | |
| `mutable.IndexedSeq[A]` | |
| `mutable.Set[A]` | |
| `mutable.SortedSet[A]` | |
| `mutable.TreeSet[A]` | |
| `mutable.HashSet[A]` | |
| `mutable.LinkedHashSet[A]` | Preserves insertion order |
| `mutable.Map[K, V]` | |
| `mutable.HashMap[K, V]` | |
| `mutable.AnyRefMap[K, V]` | |
| `mutable.ListMap[K, V]` | |
| `mutable.LinkedHashMap[K, V]` | Preserves insertion order |
| `mutable.OpenHashMap[K, V]` | |
| `mutable.IntMap[V]` | Int keys |
| `mutable.LongMap[V]` | Long keys |
| `mutable.BitSet` | |
| `mutable.UnrolledBuffer[A]` | |
| `mutable.ArrayStack[A]` | |

### Collections -- JVM Only

| Type | Notes |
|------|-------|
| `concurrent.TrieMap[K, V]` | Thread-safe |
| `java.util.WeakHashMap[K, V]` | Weak references |

### Arrays

- `Array[A]` for all supported `A` types
- Nested arrays: `Array[Array[A]]`
- Specialized for primitives

### Iterators

- `Iterator[A]` -- serialized as JSON array; deserialized lazily

### Map Key Types Supported

Maps require keys that can be serialized as JSON object keys (strings). Supported key types:

- `String`
- `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`
- Boxed primitives
- `BigInt`, `BigDecimal`
- `java.util.UUID`
- All `java.time.*` types
- Java enums
- Scala Enumerations
- Literal types
- Value classes (wrapping a supported key type)
- Custom types with `JsonKeyCodec[A]` in implicit scope
- **NOT supported**: `Option[A]` as map key (compile error)

### Value Classes

- AnyVal subclasses with single field
- Serialized as the wrapped value directly (no wrapping object)
- Can be inlined with `inlineOneValueClasses` for non-AnyVal single-field classes
- Value classes as map keys: supported if wrapped type is a supported key type

### ADT (Sealed Hierarchies)

- Sealed traits with case class / case object leaves
- Sealed abstract classes with implementations
- Nested ADTs (sealed trait within sealed trait)
- GADTs with type parameters
- Polymorphic sealed traits (e.g., `Fruit[T]`)
- Recursive ADTs (requires `allowRecursiveTypes = true`)

### Scala 3 Specific Types

- **Enums**: simple, parameterized, Java-style (`extends Enum`), ADT-hybrid, generic, higher-kinded, recursive
- **Opaque types**: transparent, bounded (`<:`), with custom constructors
- **Union types**: `A | B` -- NOT auto-derived, requires custom codec. `transientNull` controls null serialization for union types including `Null`
- **Named tuples**: `(name: String, age: Int)` -- serialized as JSON objects with field names
- **Literal types**: `"VVV"`, `true`, `'1'`, `2`, `3L`, `4.0f`, `5.0` as types
- **`derives` keyword**: via `ConfiguredJsonValueCodec`

### Enumeratum (Scala 2 only)

- Support for `enumeratum` library enums via macro derivation and custom codecs

### Scala Enumerations (legacy)

- `scala.Enumeration` values
- Serialized by name (default) or by id (`useScalaEnumValueId = true`)
- Compile-time error on duplicate names after mapping

### Non-Case Classes

- Scala classes with `var`/`val` in primary constructor
- Requires getter accessors
- Module classes (objects)

### Type Aliases

- Supported transparently
- Opaque type aliases (Scala 3)

### Generic Types

- First-order type parameters: `case class Wrapper[A](value: A)`
- Higher-kinded types: `case class HK[F[_]](value: F[Int])`
- Bounded type parameters

---

## ADT Encoding Styles (Detailed)

### Style 1: Discriminator Field (default)

Default discriminator field name: `"type"`. The discriminator appears as a regular field in the JSON object.

```json
{"type": "Circle", "radius": 5.0}
{"type": "Rectangle", "width": 3.0, "height": 4.0}
```

Configurable:
- `discriminatorFieldName = Some("kind")` -- custom field name
- `adtLeafClassNameMapper` -- maps full class name to discriminator value
- `requireDiscriminatorFirst = true` (default) -- discriminator must be first field
- `requireDiscriminatorFirst = false` -- discriminator can be anywhere in object
- `alwaysEmitDiscriminator = true` -- always include discriminator even when inferrable

### Style 2: Wrapper Object (no discriminator field)

When `discriminatorFieldName = None`:
- Case objects serialized as string values: `"Axe"`
- Case classes serialized as wrapper objects: `{"Circle": {"radius": 5.0}}`

### Style 3: Circe-like Object Encoding

When `circeLikeObjectEncoding = true`:
- Case objects serialized as `{"Axe": {}}` (object with empty value)
- Case classes serialized as `{"Circle": {"radius": 5.0}}`

### Discriminator Value Mapping

The `adtLeafClassNameMapper` function maps from the full class name to the discriminator value:
- Default: `simpleClassName` -- strips package prefix
- Custom: any `String => String` or `PartialFunction[String, String]`
- `@named("custom_name")` on a leaf class overrides the mapper

### Compile-Time Validation

- All leaf classes must have **unique** discriminator values (after mapping)
- Compile error on collision

---

## DoS Protection Features

### Compile-Time Limits (CodecMakerConfig)

| Limit | Default | Purpose |
|-------|---------|---------|
| `mapMaxInsertNumber` | 1024 | Max entries in a deserialized map |
| `setMaxInsertNumber` | 1024 | Max entries in a deserialized set (excluding BitSet) |
| `bitSetValueLimit` | 1024 | Max numeric value in a BitSet |
| `bigDecimalPrecision` | 34 | Max precision for BigDecimal |
| `bigDecimalScaleLimit` | 6178 | Max absolute scale for BigDecimal |
| `bigDecimalDigitsLimit` | 308 | Max mantissa digits for BigDecimal |
| `bigIntDigitsLimit` | 308 | Max decimal digits for BigInt |
| `allowRecursiveTypes` | false | Prevent stack overflow from recursive structures |

When limits are exceeded, a `JsonReaderException` is thrown at runtime.

### Runtime Limits (ReaderConfig)

| Field | Type | Default | Purpose |
|-------|------|---------|---------|
| `preferredBufSize` | `Int` | 32768 | Preferred internal input buffer size (bytes). Min: 12 |
| `preferredCharBufSize` | `Int` | 4096 | Preferred char buffer size. Min: 0 |
| `maxBufSize` | `Int` | 33554432 (32MB) | Maximum internal input buffer size (bytes). Max: 2,147,483,645 |
| `maxCharBufSize` | `Int` | 4194304 (4MB) | Maximum char buffer size. Max: 2,147,483,645 |
| `checkForEndOfInput` | `Boolean` | true | Check for trailing content after valid JSON |
| `throwReaderExceptionWithStackTrace` | `Boolean` | false | Include stack traces in parse exceptions (off by default for performance) |
| `appendHexDumpToParseException` | `Boolean` | true | Include hex dump in parse error messages |
| `hexDumpSize` | `Int` | 2 | Number of 16-byte lines in hex dump. Min: 1 |

Constraints:
- `preferredBufSize <= maxBufSize`
- `preferredCharBufSize <= maxCharBufSize`

### Runtime Limits (WriterConfig)

| Field | Type | Default | Purpose |
|-------|------|---------|---------|
| `indentionStep` | `Int` | 0 | Indentation step for pretty-printing (0 = compact) |
| `preferredBufSize` | `Int` | 32768 | Preferred output buffer size (bytes). Min: 1 |
| `escapeUnicode` | `Boolean` | false | Escape non-ASCII characters as `\uXXXX` |
| `throwWriterExceptionWithStackTrace` | `Boolean` | false | Include stack traces in writer exceptions |

### Security Design Principles

1. **No runtime reflection** -- codecs are fully generated at compile time
2. **Fixed class set** -- only instantiates classes known at compile time (prevents RCE)
3. **Null serialization prohibited** -- throws `NullPointerException` on null values
4. **UTF-8 validation** -- malformed bytes cause parse exceptions
5. **Duplicate key detection** -- prevents confusion attacks (configurable via `checkFieldDuplication`)
6. **Stack-less exceptions** -- default for production performance; stack traces opt-in for development

---

## Error Handling

### Exception Types

| Exception | Package | When Thrown |
|-----------|---------|------------|
| `JsonReaderException` | `core` | Parsing errors (malformed JSON, type mismatches, missing fields, limit violations) |
| `JsonWriterException` | `core` | Serialization errors (buffer overflow, etc.) |
| `NullPointerException` | JDK | Attempting to serialize null values |
| `StackOverflowError` | JDK | Deeply nested recursive structures (even with `allowRecursiveTypes = true`) |

Both `JsonReaderException` and `JsonWriterException`:
- Extend `RuntimeException`
- Constructor: `private[jsoniter_scala](msg: String, cause: Throwable, withStackTrace: Boolean)`
- Stack-less by default (`withStackTrace = false`)
- Stack traces enabled via `throwReaderExceptionWithStackTrace` / `throwWriterExceptionWithStackTrace`

### Runtime Parse Error Messages (from tests)

| Error Message Pattern | Trigger |
|-----------------------|---------|
| `expected '"'` | Missing quotes where string expected |
| `expected '{'` | Missing object start |
| `expected '['` | Missing array start |
| `expected ':'` | Missing colon in object |
| `expected '}' or ','` | Invalid object structure |
| `expected ']' or ','` | Invalid array structure |
| `illegal escape sequence` | Bad JSON escape in string |
| `malformed byte(s)` | Invalid UTF-8 encoding |
| `unexpected end of input` | Truncated JSON |
| `illegal number` | Non-numeric value where number expected |
| `expected hex digit` | Invalid UUID format |
| `illegal enum value` | Unknown enum variant |
| `duplicated field` | Repeated object key (when `checkFieldDuplication = true`) |
| `missing required field` | Absent required field (no default value) |
| `expected non-empty JSON array` | Empty array for `::` (non-empty list) type |
| `too many set inserts` | Exceeded `setMaxInsertNumber` |
| `too many map inserts` | Exceeded `mapMaxInsertNumber` |
| `expected non-escaping code point` | Invalid escape sequence |
| `expected year > 1900` | Example of custom codec validation |

Error messages always include:
- **Hexadecimal offset** in the input where the error occurred
- **Optional hex dump** of the surrounding bytes (configurable)

### Compile-Time Error Messages

| Error Message Pattern | Trigger |
|-----------------------|---------|
| `Only sealed traits or abstract classes are supported as ADT base` | Non-sealed trait/class used as ADT base |
| `No implicit 'JsonValueCodec[_]' defined for [type]` | Missing codec for a type |
| `No implicit 'JsonKeyCodec[_]' defined for [type]` | Missing key codec for a map key type |
| `Duplicated JSON value(s) defined for enum` | Enum values collide after name mapping |
| `Please consider sealing the [type]` | Non-sealed trait used where ADT expected |
| `Result values should be unique per enum class` | Duplicate values in enumeration |
| Macro parameter depends on uncompiled code | Config/annotation references code in same compilation unit |
| `Recursive types require allowRecursiveTypes` | Recursive type without config flag |

---

## Null Handling Rules

### Serialization

- **Null values**: `NullPointerException` thrown (serialization of null is prohibited)
- **`Option` fields**: `None` serialized as `null` (or skipped entirely with `transientNone = true`)
- **`transientNull`**: When `true`, fields with `null` values are skipped during serialization (relevant for Scala 3 union types with `Null`)

### Deserialization

- **`Option` fields**: `null` in JSON -> `None`
- **Collection fields**: `null` in JSON -> empty collection
- **Fields with defaults**: `null` in JSON -> default value (if default is non-null)
- **Required fields without defaults**: `null` in JSON -> `JsonReaderException`
- **Nested options** (`Option[Option[A]]`):
  - Missing field -> `None`
  - `null` value -> `Some(None)`
  - Present value -> `Some(Some(value))`
  - Requires `skipNestedOptionValues = false` (default) to distinguish

---

## Default Value Handling

### Serialization

- `transientDefault = true` (default): fields with values equal to their constructor defaults are **omitted**
- `transientDefault = false`: all fields serialized regardless of value

### Deserialization

- Fields with constructor defaults are **optional** in JSON input
- `requireDefaultFields = true`: fields with defaults are **required** (parse error if missing)
- Missing optional fields get their constructor default value

---

## Field Ordering

- Serialization preserves the order of fields as declared in the constructor
- Deserialization accepts fields in any order (hash-based lookup)
- Generated code ordered for deterministic checksums (cache-friendly)

---

## Empty Collection Handling

### Serialization

- `transientEmpty = true` (default): fields with empty collections/arrays are **omitted**
- `transientEmpty = false`: empty collections serialized as `[]` or `{}`

### Deserialization

- `requireCollectionFields = false` (default): missing collection fields -> empty collection
- `requireCollectionFields = true`: missing collection fields -> parse error

---

## Map Encoding

### Object Encoding (default)

```json
{"key1": "value1", "key2": "value2"}
```

- Keys must be types with `JsonKeyCodec` (strings, primitives, enums, UUID, java.time.*, value classes)
- Key types are stringified in JSON

### Array Encoding (`mapAsArray = true`)

```json
[["key1", "value1"], ["key2", "value2"]]
```

- Allows any type as key (not limited to string-representable types)
- Each entry is a 2-element JSON array

---

## Stringification

When `isStringified = true` or `@stringified` annotation is applied:
- Numeric values written as strings: `"123"` instead of `123`
- Boolean values written as strings: `"true"` instead of `true`
- Applies to: collections, options, value classes, and their elements
- Useful for JavaScript interop (JS numbers have limited precision for Long/BigInt)

---

## Recursive Types

- **Default**: compile-time error for recursive types
- **`allowRecursiveTypes = true`**: permits recursive types but risks `StackOverflowError` with deeply nested input
- JVM-specific test: 1,000,000 levels deep -> `StackOverflowError` with minimal stack frame accumulation
- Macro generates trampolined/stack-efficient code where possible

---

## I/O Methods (package object)

### Reading

| Method | Input Source | Notes |
|--------|-------------|-------|
| `readFromStream[A](in, config)` | `InputStream` | Uses thread-local buffer |
| `readFromStreamReentrant[A](in, config)` | `InputStream` | Safe for nested/reentrant calls |
| `readFromArray[A](buf, config)` | `Array[Byte]` | Uses thread-local buffer |
| `readFromArrayReentrant[A](buf, config)` | `Array[Byte]` | Safe for nested/reentrant calls |
| `readFromSubArray[A](buf, from, to, config)` | `Array[Byte]` slice | |
| `readFromSubArrayReentrant[A](buf, from, to, config)` | `Array[Byte]` slice | |
| `readFromByteBuffer[A](bbuf, config)` | `ByteBuffer` | |
| `readFromByteBufferReentrant[A](bbuf, config)` | `ByteBuffer` | |
| `readFromString[A](s, config)` | `String` | Fallback for non-UTF-8 |
| `readFromStringReentrant[A](s, config)` | `String` | |

### Writing

| Method | Output Target | Return |
|--------|---------------|--------|
| `writeToStream[A](x, out, config)` | `OutputStream` | `Unit` |
| `writeToStreamReentrant[A](x, out, config)` | `OutputStream` | `Unit` |
| `writeToArray[A](x, config)` | n/a | `Array[Byte]` |
| `writeToArrayReentrant[A](x, config)` | n/a | `Array[Byte]` |
| `writeToSubArray[A](x, buf, from, to, config)` | `Array[Byte]` slice | `Int` (bytes written) |
| `writeToSubArrayReentrant[A](x, buf, from, to, config)` | `Array[Byte]` slice | `Int` |
| `writeToByteBuffer[A](x, bbuf, config)` | `ByteBuffer` | `Unit` |
| `writeToByteBufferReentrant[A](x, bbuf, config)` | `ByteBuffer` | `Unit` |
| `writeToString[A](x, config)` | n/a | `String` |
| `writeToStringReentrant[A](x, config)` | n/a | `String` |

### Streaming/Scanning

| Method | Purpose |
|--------|---------|
| `scanJsonValuesFromStream[A](in, config)(f: A => Boolean)` | Parse sequence of JSON values from stream |
| `scanJsonValuesFromStreamReentrant[A](in, config)(f)` | Reentrant variant |
| `scanJsonArrayFromStream[A](in, config)(f: A => Boolean)` | Parse JSON array elements from stream |
| `scanJsonArrayFromStreamReentrant[A](in, config)(f)` | Reentrant variant |

- Callback `f` returns `true` to continue, `false` to stop
- Does NOT require holding all values in memory

### Reentrant vs Non-Reentrant

- Non-reentrant methods use thread-local buffers for performance
- Reentrant methods allocate fresh buffers, safe for nested parsing/serialization
- **Important**: do not use non-reentrant methods inside callbacks of other non-reentrant methods

---

## Edge Cases Tested (from test specs)

### Numeric Boundaries

- `Byte.MinValue` (-128) through `Byte.MaxValue` (127)
- `Short.MinValue` (-32768) through `Short.MaxValue` (32767)
- `Int.MinValue` (-2147483648) through `Int.MaxValue` (2147483647)
- `Long.MinValue` through `Long.MaxValue`
- Very large `BigInt` / `BigDecimal` (100,000+ character representations)
- `Float`/`Double` shortest representation without precision loss
- `BigDecimal` with `MathContext.DECIMAL64` precision

### Unicode and Encoding

- UTF-8 multibyte sequences
- Surrogate pairs
- Escaped characters (`\n`, `\t`, `\\`, `\"`, `\uXXXX`)
- Non-ASCII characters
- Illegal UTF-8 byte sequences -> parse error
- Escaped Unicode output mode (`escapeUnicode = true`)

### Collection Edge Cases

- Empty collections (serialized or skipped based on config)
- `::` (cons cell / non-empty list) -- parse error on empty JSON array
- Null JSON values deserialized as empty collections
- `Iterator` serialization (consumed during write)
- Nested collections (`List[List[Int]]`, `Map[Int, Map[String, A]]`)
- Collection with more elements than `setMaxInsertNumber` / `mapMaxInsertNumber`

### Field Edge Cases

- Duplicate field names in JSON input (detected or ignored via config)
- Unknown/unexpected fields (skipped or error via config)
- Missing required fields
- Missing optional fields (with and without defaults)
- Field name case transformations
- Fields with `@transient` annotation
- Fields with `@named` annotation

### ADT Edge Cases

- Case objects in sealed hierarchies
- Discriminator as first field vs any position
- Unknown discriminator value -> parse error
- Wrapper-object encoding with case objects as strings
- Recursive ADT structures
- GADT with type parameters
- Polymorphic ADTs (`Status[+E, +A]`)

### Option Edge Cases

- `Option[Option[A]]` -- distinguishing null from missing
- Nested options more than 2 levels deep
- Options as collection elements
- Options NOT allowed as map keys
- `transientNone` behavior

### Value Class Edge Cases

- Value classes wrapping primitives
- Value classes wrapping strings
- Value classes as map keys
- Value classes in collections
- `inlineOneValueClasses` for non-value single-field classes

### Recursive Structure Edge Cases

- 1,000,000 levels deep -> `StackOverflowError`
- Recursive sealed traits (enums)
- Self-referential types

### Decimal64 Specific

- Mantissa boundaries: -4503599627370496L to 4503599627370496L
- Scale range: -256 to 256
- Canonization of values
- Numeric vs string representation based on mantissa bits

---

## Scala 3 Specific Features Tested

### Enums

| Category | Types Tested |
|----------|-------------|
| Simple enums | `TrafficLight` (Red, Yellow, Green) |
| Java-style enums | `Java` extends `Enum[Java]` |
| Parameterized enums | `Color(rgb: Int)`, `MediaType(name: String)` |
| Multi-param enums | `Planet(mass: Double, radius: Double)` |
| ADT-hybrid enums | `ColorADT` with mixed cases |
| Generic enums | `GEnum[A]` with type parameter |
| F-bounded enums | `FruitEnum[T]` with self-recursive bounds |
| Higher-kinded enums | `FooEnum[A[_]]` |
| Annotated enums | `Side` with `@named`, `ClientOut` with `@transient` |
| Recursive enums | `LinkedList[+T]` (requires `allowRecursiveTypes`) |

### Opaque Types

| Type | Pattern |
|------|---------|
| `Gram` | Transparent alias for `Double` |
| `Meter <: Double` | Bounded opaque type |
| `Year` | Opaque `Int` with validated constructor |

- Support `@stringified` annotation
- Custom value codecs supported
- Collections of opaque types: `Array[Meter]`, `Map[Meter, Gram]`
- Compile error for opaque types that hide type arguments

### Union Types

- **NOT auto-derived** -- requires custom `JsonValueCodec` implementation
- Custom codec with `setMark`, `resetMark`, `rollbackToMark` for backtracking
- Recursive union types with custom codecs
- `transientNull` for union types including `Null`
- Compile error with proper message when attempting auto-derivation

### Named Tuples (Scala 3.7+)

- Empty named tuples
- Single and multi-field named tuples
- Generic named tuples: `(a: A, b: B)`
- Higher-kinded named tuples: `(i: F[Int], s: G[String])`
- `NamedTuple.From[CaseClass]` -- conversion from case class
- Operations: `Reverse`, `Concat`, `Tail`, `Init`, `Drop`, `Take`, `Split`, `Zip`, `Map`
- Nested named tuples
- Field name mapping with `enforce_snake_case`
- High-arity named tuples (23 fields)
- Named tuples in case classes and collections
- Recursive named tuples (with `allowRecursiveTypes`)
- `NamedTuple.DropNames` -- converts to regular tuple

### Literal Types

- String literals: `"VVV"` as a type
- Boolean literals: `true` as a type
- Char literals: `'1'` as a type
- Int literals: `2` as a type
- Long literals: `3L` as a type
- Float literals: `4.0f` as a type
- Double literals: `5.0` as a type
- In case class fields, AnyVal wrappers, top-level, as map keys
- Stringified encoding mode for literals
- Parse error with expected value on mismatch

### `derives` Keyword

```scala
enum MyEnum derives ConfiguredJsonValueCodec:
  case A, B, C
```

- Custom config via `given CodecMakerConfig`
- Works with enums and sealed traits
- Requires runtime dependency on macros module

### Given/Using Integration

- Named given constants for codec generation
- Given functions for generic codec derivation
- Bounded type parameters with context bounds
- Anonymous given constants NOT supported (compile error with guidance)

---

## Custom Codec Injection

### Value Codecs

Custom `JsonValueCodec[A]` in implicit scope overrides macro generation for type `A`:

```scala
implicit val customIntCodec: JsonValueCodec[Int] = new JsonValueCodec[Int] {
  def decodeValue(in: JsonReader, default: Int): Int = ???
  def encodeValue(x: Int, out: JsonWriter): Unit = ???
  def nullValue: Int = 0
}
```

Used for:
- `Either[L, R]` (requires custom codec)
- Non-sealed traits
- Union types (Scala 3)
- Custom numeric formats
- Custom date/time formats
- ADT case objects with special encoding
- Collection types from external libraries (e.g., `zio.Chunk`)
- Nullable types (distinguishing null from missing)

### Key Codecs

Custom `JsonKeyCodec[A]` in implicit scope for map key types:

```scala
implicit val customKeyCodec: JsonKeyCodec[MyType] = new JsonKeyCodec[MyType] {
  def decodeKey(in: JsonReader): MyType = ???
  def encodeKey(x: MyType, out: JsonWriter): Unit = ???
}
```

---

## Known Limitations and Restrictions

1. **No validation for JSON representation length during parsing** -- length is bounded only by buffer limits
2. **Only acyclic graphs** -- circular object references cause infinite loops/stack overflow
3. **UTF-8 only** for byte-based I/O (fallback to `String` parsing available)
4. **Constructor constraints**: primary constructor must not have default values in non-first parameter lists
5. **ADT definitions** cannot be nested inside classes, traits, or functions (must be top-level or in objects)
6. **Macro config circularity**: config parameter cannot reference the result of the macro call being configured
7. **Scala.js**: no Java enum support from Java sources
8. **Scala 3 generic types** with concrete parameters need named codecs (anonymous codecs may conflict)
9. **Non-reentrant methods** must not be nested (use `*Reentrant` variants)
10. **Escaped character parsing** not supported for strings mapped to byte arrays, numeric types, or `java.time.*` types
11. **Union types** cannot be auto-derived -- must provide custom codec
12. **Opaque types hiding type arguments** cause compile error
13. **Scala 3 compile-time expression limitations** -- not all Scala 3 expressions evaluated at compile time in config
14. **No support for non-concrete ADTs** with free type parameters

---

## Base Encoding Support (Core module)

- `Base16` encoding/decoding for byte arrays
- `Base64` encoding/decoding for byte arrays (RFC 4648)

---

## Serialization Format Details

### Numbers

- Float/Double: shortest textual representation without precision loss
- SWAR (SIMD Within A Register) techniques for fast parsing
- 8-byte word parsing for strings and numeric checking

### Strings

- Escaped character support: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`
- Optional ASCII-only output with Unicode escaping

### Pretty Printing

- Configurable indentation step via `WriterConfig.indentionStep`
- `0` = compact (default), positive integer = spaces per level

### java.io.Serializable

- Both codecs and runtime configurations implement `java.io.Serializable`
- Enables use in distributed computing frameworks (Spark, etc.)
