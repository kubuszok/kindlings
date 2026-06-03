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

`AvroIO` provides convenience methods for binary and JSON Avro serialization:

| Method | Description |
|--------|-------------|
| `AvroIO.toBinary[A](value)` | Encode to Avro binary format |
| `AvroIO.fromBinary[A](bytes)` | Decode from Avro binary format |
| `AvroIO.toJson[A](value)` | Encode to Avro JSON format |
| `AvroIO.fromJson[A](json)` | Decode from Avro JSON format |

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
| `withDecimalConfig(precision, scale)` | Global `BigDecimal` precision and scale |

## Annotations

All annotations are in the `hearth.kindlings.avroderivation.annotations` package.

```scala
import hearth.kindlings.avroderivation.annotations._
```

### Field and type naming

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroName("name")` | Type | Override the Avro schema name for a type (highest priority) |
| `@fieldName("name")` | Field | Override the Avro field name |
| `@avroErasedName` | Type | Disable generic type parameter encoding in schema name |
| `@avroFqnParamNames` | Type | Use fully qualified names for type parameters in schema name |

### Documentation and metadata

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroDoc("text")` | Type, Field | Add documentation to the schema |
| `@avroNamespace("ns")` | Type | Set the Avro namespace for a specific type |
| `@avroProp("key", "value")` | Type, Field | Add custom Avro properties (stackable) |
| `@avroAlias("alias")` | Type, Field | Add schema aliases for evolution (stackable) |

### Schema control

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroFixed(size)` | Field (`Array[Byte]`) | Use fixed-size bytes instead of variable |
| `@avroError` | Type | Mark record as an Avro error type |
| `@avroScalePrecision(precision, scale)` | Field (`BigDecimal`) | Per-field decimal precision and scale |
| `@avroSortPriority(n)` | Type | Control ordering of subtypes in UNION/ENUM schemas |

### Default values

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@avroDefault("json")` | Field | Default value as a JSON string literal |
| `@avroNoDefault` | Field | Suppress default value even if field has a Scala default |
| `@avroEnumDefault("value")` | Type (sealed trait) | Set the default value for an enum schema |
| `@transientField` | Field | Exclude field from schema entirely (must have a default value) |

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
    // - LocalTime -> int with logicalType "time-millis"
    // - LocalDateTime -> long with logicalType "local-timestamp-millis"
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

    // Generic types encode type parameters in the schema name by default:
    // Audited[User] -> "AuditedUser"
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
| Recursive types | Needs workarounds | Yes | Just works |
| Named tuples | No | No | Yes |
| Scala 3 enums | No | Yes | Yes |
| Java enums | No | Yes | Yes |
| Opaque types | No | Partial | Yes |
| Union types (Scala 3) | No | No | Yes |
| Literal types (Scala 3) | No | No | Yes |
| `@avroName` type renaming | Yes | Yes | Yes |
| `@avroScalePrecision` per-field | Yes | Yes | Yes |
| `@avroFqnParamNames` | No | No | Yes |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

!!! note
    Kindlings is **3-5x faster** than avro4s across all benchmarks — both simple and complex nested types.

#### Encode

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 281.7M | — | 45.0M | **6.3x faster** |
| SimpleCC | 3 | 270.5M | 49.3M | 48.1M | **5.5x faster** |
| Person | 2.13 | 19.6M | — | 4.6M | **4.3x faster** |
| Person | 3 | 18.6M | 5.7M | 5.7M | **3.3x faster** |

#### Decode

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 425.3M | — | 17.6M | **24.2x faster** |
| SimpleCC | 3 | 474.1M | 23.2M | 42.9M | **11.1x faster** |
| Person | 2.13 | 14.4M | — | 3.5M | **4.1x faster** |
| Person | 3 | 13.8M | 3.2M | 4.1M | **3.4x faster** |

Note: Kindlings semi-automatic and automatic derivation produce identical performance -- this is the "sanely-automatic" design.
