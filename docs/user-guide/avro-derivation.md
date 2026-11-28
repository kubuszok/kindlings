# Avro Derivation

**JVM-only** module. Drop-in replacement for [avro4s](https://github.com/sksamuel/avro4s) -- derives `AvroSchemaFor`, `AvroEncoder`, and `AvroDecoder` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-avro-derivation" % "{{ kindlings_version() }}"
    ```

    Avro derivation is **JVM-only** (no `%%%`). The Apache Avro runtime is pulled in transitively.

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Encoding and decoding a case class"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._

    case class Person(name: String, age: Int)

    // Semi-automatic derivation
    val encoder: AvroEncoder[Person] = AvroEncoder.derived[Person]
    val decoder: AvroDecoder[Person] = AvroDecoder.derived[Person]

    // Encode to Avro GenericRecord and decode back
    val record: Any = encoder.encode(Person("Alice", 30))
    val person: Person = decoder.decode(record)
    println(person)
    // expected output:
    // Person(Alice,30)

    // Binary serialization via AvroIO
    val bytes: Array[Byte] = AvroIO.toBinary(Person("Bob", 25))(encoder)
    val decoded: Person = AvroIO.fromBinary[Person](bytes)(decoder)
    println(decoded)
    // expected output:
    // Person(Bob,25)
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `AvroSchemaFor.derived[A]` | `AvroSchemaFor[A]` | Semi-automatic schema derivation |
| `AvroSchemaFor.schemaOf[A]` | `Schema` | Inline schema generation (no instance allocation) |
| `AvroSchemaFor.derived[A]` | `AvroSchemaFor[A]` | Sanely-automatic (given/implicit) |
| `AvroEncoder.derived[A]` | `AvroEncoder[A]` | Semi-automatic encoder |
| `AvroEncoder.encode[A](value)` | `Any` | Inline encoding (no instance allocation) |
| `AvroEncoder.derived[A]` | `AvroEncoder[A]` | Sanely-automatic (given/implicit) |
| `AvroDecoder.derived[A]` | `AvroDecoder[A]` | Semi-automatic decoder |
| `AvroDecoder.decode[A](value)` | `A` | Inline decoding (no instance allocation) |
| `AvroDecoder.derived[A]` | `AvroDecoder[A]` | Sanely-automatic (given/implicit) |

All methods take an implicit/using `AvroConfig` parameter (defaults to `AvroConfig.default`).

### Serialization helpers

`AvroIO` provides convenience methods for binary, JSON and data-file Avro serialization:

| Method | Description |
|--------|-------------|
| `AvroIO.toBinary[A](value)` | Encode to Avro binary format |
| `AvroIO.fromBinary[A](bytes)` | Decode from Avro binary format |
| `AvroIO.toJson[A](value)` | Encode to Avro JSON format |
| `AvroIO.fromJson[A](json)` | Decode from Avro JSON format |
| `AvroIO.toDataFile[A](values, codec)` | Write an Avro object container file (the schema in the header, then the records; `codec` is optional, e.g. `CodecFactory.deflateCodec(6)`) — avro4s' `AvroOutputStream.data` |
| `AvroIO.fromDataFile[A](bytes)` | Read every record of an Avro object container file, resolving the writer's schema (from the header) to the decoder's — avro4s' `AvroInputStream.data` |

### Type hierarchy

`AvroEncoder[A]` extends `AvroSchemaFor[A]` and `AvroDecoder[A]` extends `AvroSchemaFor[A]`, so every encoder and decoder also provides the Avro schema.

## Configuration

All derivation methods accept an implicit `AvroConfig`:

```scala
import hearth.kindlings.avroderivation._

implicit val config: AvroConfig = AvroConfig.default
  .withNamespace("com.example")
  .withSnakeCaseFieldNames
  .withDecimalConfig(precision = 10, scale = 2)
```

| Builder method | Description |
|---------------|-------------|
| `withNamespace(ns)` | Set the Avro namespace for generated schemas |
| `withTransformFieldNames(f)` | Custom field name transform |
| `withSnakeCaseFieldNames` | `fieldName` -> `field_name` |
| `withKebabCaseFieldNames` | `fieldName` -> `field-name` |
| `withPascalCaseFieldNames` | `fieldName` -> `FieldName` |
| `withTransformConstructorNames(f)` | Custom constructor name transform for sealed traits |
| `withDecimalConfig(precision, scale)` | Global `BigDecimal` precision and scale (BYTES + `decimal` logical type; without it `BigDecimal` is a STRING). A value with more decimal places than the scale, or more digits than the precision, is rejected |
| `withDecimalConfig(precision, scale, roundingMode)` | The same, rounding values with more decimal places than the scale with a `java.math.RoundingMode` (as avro4s 4's implicit `RoundingMode`; the default is `UNNECESSARY`, i.e. rejecting them). Also applies to `@avroScalePrecision` fields; shorthand for `withDecimalConfig(precision, scale).withRoundingMode(roundingMode)` |
| `withRoundingMode(roundingMode)` | Rounding mode of every decimal (`java.math.RoundingMode`, default `UNNECESSARY`): the `DecimalConfig`'s and the `@avroScalePrecision` fields', also without a `DecimalConfig`. See [Decimal rounding](#decimal-rounding) |
| `withInstantPrecision(p)` | `Instant` / `java.sql.Timestamp` precision: `TimestampPrecision.Millis` (default), `Micros` or `Nanos` |
| `withLocalDateTimePrecision(p)` | `LocalDateTime` precision: `TimestampPrecision.Nanos` (default, as avro4s), `Micros` or `Millis` |
| `withOffsetDateTimeAsIsoString` | Encode `OffsetDateTime` as avro4s does: an ISO-8601 string (default: a UTC `timestamp-millis` long) |
| `withoutScalaDefaultsInSchema` / `withScalaDefaultsInSchema(enabled)` | Whether Scala default values become schema defaults (default: yes, as in avro4s). When off, only `@avroDefault` becomes a schema default |

`AvroConfig.avro4sDefaults` is a preset with avro4s' behaviour where Kindlings' defaults differ: `BigDecimal` as
`decimal(8, 2)` and `OffsetDateTime` as an ISO-8601 string (see
[Feature parity](feature-parity.md#avro-derivation)).

### Decimal rounding

A `BigDecimal` encoded as a `decimal` (with `withDecimalConfig` or `@avroScalePrecision`) that has more decimal places
than the scale is rejected with an `ArithmeticException` by default (`RoundingMode.UNNECESSARY`, as avro4s 4). To round
it instead, pick a `java.math.RoundingMode`:

```scala
import java.math.RoundingMode

// every decimal: the DecimalConfig's and the @avroScalePrecision fields' (no DecimalConfig needed)
implicit val config: AvroConfig = AvroConfig.default.withRoundingMode(RoundingMode.HALF_UP)

case class Invoice(
    @avroScalePrecision(10, 2) total: BigDecimal,                       // HALF_UP, from the config
    @avroScalePrecision(10, 4, RoundingMode.HALF_EVEN) rate: BigDecimal  // its own rounding mode
)
```

The rounding mode of a decimal is, from the most specific:

1. the one given to its field's `@avroScalePrecision(precision, scale, roundingMode)`,
2. `AvroConfig.roundingMode`: the one set last with `withRoundingMode(mode)` or
   `withDecimalConfig(precision, scale, mode)` (so `withDecimalConfig(8, 2, FLOOR).withRoundingMode(HALF_UP)` rounds
   `HALF_UP`, and `withRoundingMode(HALF_UP).withDecimalConfig(8, 2, FLOOR)` rounds `FLOOR`; the 2-argument
   `withDecimalConfig(precision, scale)` keeps a rounding mode set before it),
3. without either, the `DecimalConfig`'s own `roundingMode` (`DecimalConfig(8, 2).withRoundingMode(mode)` passed with
   `copy(decimalConfig = ...)`), otherwise `UNNECESSARY`.

`withRoundingMode` does not turn a plain `BigDecimal` into a `decimal`: without a `DecimalConfig` it is still a STRING.

## Annotations

All annotations are in the `hearth.kindlings.avroderivation.annotations` package.

```scala
import hearth.kindlings.avroderivation.annotations._
```

### Field and type naming

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroName("name")` | Type, case object | Override the Avro schema name for a type (highest priority); on a case object of an ENUM, the name of its symbol (used as it is: `transformConstructorNames` does not apply). The annotated name of a type argument is used in a generic record's name (`Box__renamed`) |
| `@fieldName("name")` | Field | Override the Avro field name |
| `@avroErasedName` | Type | Disable generic type parameter encoding in schema name |
| `@avroFqnParamNames` | Type | Use fully qualified names for type parameters in schema name |

### Documentation and metadata

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroDoc("text")` | Type, Field | Add documentation to the schema |
| `@avroDoc("text")` on a value class | Type | Documents the fields of that type (a value class has no schema of its own), unless the field has its own `@avroDoc` |
| `@avroNamespace("ns")` | Type, Field | Set the Avro namespace for a specific type. On a sealed trait it also applies to the subtypes that have no `@avroNamespace` of their own. On a field, it moves **every** named type (record, enum, fixed) below the field to that namespace — through unions, options, collections, maps and nested records — taking precedence over the types' own annotations, as avro4s does |
| `@avroProp("key", "value")` | Type, Field | Add custom Avro properties (stackable). The value is always a String, as in avro4s 4 |
| `@avroAlias("alias")` | Type, Field | Add schema aliases for evolution (stackable) |

### Schema control

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroFixed(size)` | Field (`Array[Byte]` or `String`), value class of one | Use a FIXED type of `size` bytes: a shorter value is padded with zeros (a `String` is written as UTF-8 and read back with the padding), a longer one is rejected. On a value class, the FIXED type is named after the class |
| `@avroError` | Type, Field | Mark record as an Avro error type (on a field: the record of that field) |
| `@avroScalePrecision(precision, scale)` | Field (`BigDecimal`) | Per-field decimal precision and scale (rounded with `AvroConfig.roundingMode`) |
| `@avroScalePrecision(precision, scale, roundingMode)` | Field (`BigDecimal`) | The same, rounded with its own `java.math.RoundingMode` whatever the config says (see [Decimal rounding](#decimal-rounding)) |
| `@avroSortPriority(n)` | Type | Control ordering of subtypes in UNION/ENUM schemas (higher first) |
| `@avroUnionPosition(n)` | Type | Position of a subtype in UNION/ENUM schemas (lower first): new subtypes can be appended without renumbering the others. `@avroSortPriority` on the same subtype takes precedence |
| `@AvroJavaName`, `@AvroJavaNamespace`, `@AvroJavaProp(key, value)`, `@AvroJavaEnumDefault` | Java enum (the last one: a constant) | Java annotations (same package) for Java enums, which cannot carry the Scala ones: the ENUM's name, namespace, a property and the default symbol |

### Default values

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroDefault("json")` | Field | Default value as a JSON string literal |
| `@avroNoDefault` | Field | Suppress the schema default (the field's Scala default value) |
| `@avroEnumDefault("value")` | Type (sealed trait) | Set the default value for an enum schema |
| `@transientField` | Field | Exclude field from schema entirely; decoded as its default value, or as `None` for an `Option` field without one (any other field must have a default value) |

As in avro4s, case class fields' Scala default values become schema defaults (for a union the default's branch is moved
first, as Avro requires); `@avroDefault` overrides the value and `@avroNoDefault` suppresses it. Avro's schema
resolution needs them to read old data with a reader schema that adds fields. `withoutScalaDefaultsInSchema` keeps only
`@avroDefault` defaults in the schema. When a
`GenericRecord` is decoded directly, a field with a Scala default decodes to the default if the record has no such
field or its value cannot be decoded (as avro4s does); a missing field without a default is rejected (an `Option` field
decodes to `None`).

Decoders accept whatever holds a value, regardless of the schema (as avro4s' do): a `String` from a `CharSequence`
(`Utf8`, `String`), UTF-8 bytes (an array or a `ByteBuffer`) or a FIXED value; bytes and decimals from a `ByteBuffer`,
a byte array or a FIXED value; a collection from a Java collection, a Java array or a Scala collection; integral values
with widening; a `null` Boolean is `false`. Besides `String`, `org.apache.avro.util.Utf8` and `CharSequence` fields are Avro strings.

## Usage examples

??? example "Annotated types with documentation and namespaces"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._
    import hearth.kindlings.avroderivation.annotations._

    @avroDoc("A person record")
    @avroNamespace("com.example.models")
    case class Person(
      @avroDoc("The person's full name") name: String,
      @avroDoc("Age in years") age: Int
    )

    val encoder = AvroEncoder.derived[Person]
    val decoder = AvroDecoder.derived[Person]

    val decoded = decoder.decode(encoder.encode(Person("Alice", 30)))
    println(decoded)
    // expected output:
    // Person(Alice,30)
    ```

??? example "Sealed trait with sort priority"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._
    import hearth.kindlings.avroderivation.annotations._

    sealed trait Shape
    @avroSortPriority(1)
    case class Rectangle(width: Double, height: Double) extends Shape
    @avroSortPriority(2)
    case class Circle(radius: Double) extends Shape

    // Rectangle appears first in the union schema thanks to @avroSortPriority
    val encoder = AvroEncoder.derived[Shape]
    val decoder = AvroDecoder.derived[Shape]

    val decoded = decoder.decode(encoder.encode(Circle(5.0): Shape))
    println(decoded)
    // expected output:
    // Circle(5.0)
    ```

??? example "Default values and field annotations"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._
    import hearth.kindlings.avroderivation.annotations._

    case class Settings(
      host: String,
      @avroDefault("8080") port: Int = 8080,
      @avroDefault("\"info\"") logLevel: String = "info",
      @transientField cache: Option[String] = None
    )

    val encoder = AvroEncoder.derived[Settings]
    val decoder = AvroDecoder.derived[Settings]

    // @transientField excludes `cache` — it is not encoded, and gets its default on decode
    val encoded = encoder.encode(Settings("localhost", cache = Some("hot")))
    val decoded = decoder.decode(encoded)
    println(decoded)
    // expected output:
    // Settings(localhost,8080,info,None)
    ```

??? example "Custom field names with snake_case config"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._

    implicit val config: AvroConfig = AvroConfig.default
      .withSnakeCaseFieldNames
      .withNamespace("com.example")

    case class UserProfile(firstName: String, lastName: String, emailAddress: String)

    // Fields become: first_name, last_name, email_address in the Avro schema
    val encoder = AvroEncoder.derived[UserProfile]
    val decoder = AvroDecoder.derived[UserProfile]

    val decoded = decoder.decode(encoder.encode(UserProfile("Alice", "Smith", "alice@example.com")))
    println(decoded)
    // expected output:
    // UserProfile(Alice,Smith,alice@example.com)
    ```

??? example "Recursive data types"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types work out of the box
    val encoder = AvroEncoder.derived[TreeNode]
    val decoder = AvroDecoder.derived[TreeNode]

    val tree = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, List(TreeNode(4, Nil)))))
    val decoded = decoder.decode(encoder.encode(tree))
    println(decoded)
    // expected output:
    // TreeNode(1,List(TreeNode(2,List()), TreeNode(3,List(TreeNode(4,List())))))
    ```

??? example "Logical types (UUID, Instant, LocalDate, etc.)"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._
    import java.time._
    import java.util.UUID

    case class EventRecord(
      id: UUID,
      timestamp: Instant,
      date: LocalDate,
      time: LocalTime,
      localTimestamp: LocalDateTime
    )

    // Logical types are handled automatically:
    // - UUID -> string with logicalType "uuid"
    // - Instant -> long with logicalType "timestamp-millis"
    // - LocalDate -> int with logicalType "date"
    // - LocalTime -> long with logicalType "time-micros"
    // - LocalDateTime -> long with logicalType "timestamp-nanos"
    // - java.sql.Date -> int with logicalType "date", java.sql.Timestamp -> long with logicalType "timestamp-millis"
    val encoder = AvroEncoder.derived[EventRecord]
    val decoder = AvroDecoder.derived[EventRecord]

    val event = EventRecord(
      UUID.fromString("550e8400-e29b-41d4-a716-446655440000"),
      Instant.ofEpochMilli(1700000000000L),
      LocalDate.of(2024, 1, 15),
      LocalTime.of(10, 30, 0),
      LocalDateTime.of(2024, 1, 15, 10, 30, 0)
    )
    val decoded = decoder.decode(encoder.encode(event))
    println(decoded == event)
    // expected output:
    // true
    ```

??? example "Generic types with name encoding"

    ```scala
    //> using dep com.kubuszok::kindlings-avro-derivation:{{ kindlings_version() }}

    import hearth.kindlings.avroderivation._
    import hearth.kindlings.avroderivation.annotations._

    case class Audited[T](data: T, createdBy: String)
    case class User(name: String)

    // Generic types encode type parameters in the schema name by default, as avro4s does:
    // Audited[User] -> "Audited__User", Pair[String, Int] -> "Pair__String_Int"
    val encoder = AvroEncoder.derived[Audited[User]]
    val decoder = AvroDecoder.derived[Audited[User]]

    val decoded = decoder.decode(encoder.encode(Audited(User("Alice"), "admin")))
    println(decoded)
    // expected output:
    // Audited(User(Alice),admin)
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.avroderivation.debug._
```

This enables `LogDerivation` implicits for `AvroSchemaFor`, `AvroEncoder`, and `AvroDecoder`, printing the derivation steps to the compiler output.

## Comparison with avro4s

### Feature differences

| Feature | avro4s (v4, Scala 2) | avro4s (v5, Scala 3) | Kindlings |
|---------|---------------------|---------------------|-----------|
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |
| Inline schema/encode/decode | No | No | Yes |
| Recursive types | Yes | No (auto-derivation does not recurse) | Just works |
| Named tuples | No | No | Yes |
| Scala 3 enums | No | Yes | Yes |
| Java enums | Yes | Yes | Yes |
| Opaque types | No | Partial | Yes |
| Union types (Scala 3) | No | No | Yes |
| Literal types (Scala 3) | No | No | Yes |
| `@avroName` type renaming | Yes | Yes | Yes |
| `@avroName` of a type argument in generic names | Yes | No (two instantiations can get the same name) | Yes |
| Field `@avroNamespace` on a sealed trait / optional record | Schema only (cannot encode an `Option` below it) | Schema only (cannot build the union's encoder) | Yes |
| `Either` with an optional side (`Either[A, Option[B]]`) | Decodes only the `Left` | No | Yes |
| `@avroUnionPosition`, `@avroFixed` on a `String`, Java enum annotations | Yes | Yes | Yes |
| `@avroFixed` on a value class | Yes | No | Yes |
| Data files (`AvroOutputStream.data` / `AvroIO.toDataFile`) | Yes | Yes | Yes |
| `@avroScalePrecision` per-field | No (v4: implicit `ScalePrecision` only) | Declared (param order `(scale, precision)`) but ignored | Yes (param order `(precision, scale)`) |
| `@avroFqnParamNames` | No | No | Yes |
| Value classes, `scala.Enumeration` | Yes | No | Yes |
| Timestamp precision | Custom `SchemaFor` | Custom `SchemaFor` | `withInstantPrecision` / `withLocalDateTimePrecision` |

### Checked against avro4s

The `upstream-oracle-tests` module derives the same types with Kindlings and with avro4s (4.x on Scala 2.13, 5.x on
Scala 3) and compares the schemas, the binary encodings, each side decoding the other's bytes, hand-built and invalid
`GenericRecord`s and schema evolution — including avro4s' own schema, encoder, decoder, stream and issue test suites
replayed against Kindlings (`AvroUpstreamTestsOracleSpec`, `AvroUpstreamDecodingOracleSpec`,
`AvroUpstreamExtrasOracleSpec`). The differences are documented in
[Feature parity](feature-parity.md#avro-derivation): tuple record names (Kindlings encodes the type arguments, so
different tuple types can share a schema), and two defaults — `BigDecimal` as a STRING and `OffsetDateTime` as a
`timestamp-millis` long — which `AvroConfig.avro4sDefaults` switches to avro4s' behaviour.

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

!!! note
    Kindlings is **1.5-6.7x faster** than avro4s across all benchmarks — both simple and complex nested types.

#### Encode

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 272M | — | 44.6M | **6.1x faster** |
| SimpleCC | 3 | 277M | 48.7M | 50.5M | **5.5x faster** |
| Person | 2.13 | 19.5M | — | 4.5M | **4.3x faster** |
| Person | 3 | 19.1M | 5.8M | 5.8M | **3.3x faster** |
| Voyage | 2.13 | 27.5M | — | 11.8M | **2.3x faster** |
| Voyage | 3 | 15.7M | — | 13.7M | 1.1x (within noise) |

#### Decode

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 119M | — | 17.7M | **6.7x faster** |
| SimpleCC | 3 | 127M | 26.0M | 83.9M | **1.5x faster** |
| Person | 2.13 | 9.8M | — | 3.7M | **2.6x faster** |
| Person | 3 | 9.3M | 3.1M | 4.4M | **2.1x faster** |
| Voyage | 2.13 | 27.3M | — | 9.1M | **3.0x faster** |
| Voyage | 3 | 19.1M | — | 4.6M | **4.2x faster** |

`Voyage` (`AvroParityBenchmark`, measured separately on a shared machine) covers the annotation-driven paths: ENUM
symbols renamed with `@avroName`, a `String` as a FIXED value and a record moved by a field-level `@avroNamespace`.

Note: Kindlings semi-automatic and automatic derivation produce identical performance -- this is the "sanely-automatic" design.
