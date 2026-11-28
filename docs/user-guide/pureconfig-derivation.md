# PureConfig Derivation

**JVM-only** module. Drop-in replacement for `pureconfig-generic` / `pureconfig-generic-scala3` -- derives `ConfigReader`, `ConfigWriter`, and `ConfigConvert` for case classes, sealed traits, Scala 3 enums, and more.

!!! tip "Need cross-platform HOCON?"
    If you need HOCON support on Scala.js or Scala Native, use [kindlings-sconfig-derivation](sconfig-derivation.md) instead — it provides the same configuration API built on [sconfig](https://github.com/ekrich/sconfig), a cross-platform port of Typesafe Config.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-pureconfig-derivation" % "{{ kindlings_version() }}"
    ```

    PureConfig derivation is **JVM-only** (no `%%%`). The PureConfig runtime (`com.github.pureconfig:pureconfig-core`) is pulled in transitively.

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Reading and writing HOCON configuration"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    case class ServerConfig(host: String, port: Int, debug: Boolean)

    // Semi-automatic derivation
    val reader: ConfigReader[ServerConfig] = KindlingsConfigReader.derived[ServerConfig]
    val writer: ConfigWriter[ServerConfig] = KindlingsConfigWriter.derived[ServerConfig]

    // Read from HOCON string
    val result = ConfigSource.string("""
      host = "localhost"
      port = 8080
      debug = false
    """).load[ServerConfig](reader)
    println(result)
    // expected output:
    // Right(ServerConfig(localhost,8080,false))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsConfigReader.derived[A]` | `ConfigReader[A]` | Semi-automatic reader |
| `KindlingsConfigReader.derived[A]` | `KindlingsConfigReader[A]` | Sanely-automatic (given/implicit) |
| `KindlingsConfigWriter.derived[A]` | `ConfigWriter[A]` | Semi-automatic writer |
| `KindlingsConfigWriter.derived[A]` | `KindlingsConfigWriter[A]` | Sanely-automatic (given/implicit) |
| `KindlingsConfigConvert.derived[A]` | `ConfigConvert[A]` | Semi-automatic reader + writer |
| `KindlingsConfigConvert.derived[A]` | `KindlingsConfigConvert[A]` | Sanely-automatic reader + writer |

All methods take an implicit/using `PureConfig` parameter (defaults to `PureConfig.default`).

### Type hierarchy

`KindlingsConfigReader[A]` extends `pureconfig.ConfigReader[A]`, `KindlingsConfigWriter[A]` extends `pureconfig.ConfigWriter[A]`, and `KindlingsConfigConvert[A]` extends `pureconfig.ConfigConvert[A]`. Derived instances are fully drop-in compatible with existing PureConfig code, including `ConfigSource.load[A]`.

## Configuration

All derivation methods accept an implicit `PureConfig`:

```scala
import hearth.kindlings.pureconfigderivation._

implicit val config: PureConfig = PureConfig.default
  .withSnakeCaseMemberNames
  .withDiscriminator("type")
  .withUseDefaults
```

The defaults match upstream PureConfig behavior: kebab-case member names, kebab-case constructor names, `"type"` discriminator, defaults enabled, and unknown keys allowed.

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseMemberNames` | `fieldName` -> `field_name` |
| `withKebabCaseMemberNames` | `fieldName` -> `field-name` (default) |
| `withPascalCaseMemberNames` | `fieldName` -> `FieldName` |
| `withScreamingSnakeCaseMemberNames` | `fieldName` -> `FIELD_NAME` |
| `withCamelCaseMemberNames` | `fieldName` -> `fieldName` (identity) |
| `withTransformMemberNames(f)` | Custom field name transform |
| `withSnakeCaseConstructorNames` | `MyType` -> `my_type` in discriminator |
| `withKebabCaseConstructorNames` | `MyType` -> `my-type` in discriminator (default) |
| `withTransformConstructorNames(f)` | Custom constructor name transform |
| `withDiscriminator(field)` | ADT discriminator field name (default: `"type"`) |
| `withWrappedSubtypes` | No discriminator -- wrap subtypes in single-key objects |
| `withUseDefaults` | Use case class default values for missing keys (default) |
| `withoutUseDefaults` | Require all keys |
| `withAllowUnknownKeys` | Ignore unexpected HOCON keys (default) |
| `withStrictDecoding` | Fail on unexpected HOCON keys |

### Per-type hints

For per-type customization (e.g. one case class uses `snake_case` while everything else stays `kebab-case`), define an implicit `KindlingsProductHint` or `KindlingsCoproductHint` for the specific type. The macro looks them up at derivation time and falls back to the global `PureConfig` when no per-type hint is in scope.

| Per-type hint | Upstream equivalent | Behaviour |
|---|---|---|
| `KindlingsProductHint[A](transformMemberNames, useDefaults, allowUnknownKeys)` | `ProductHint[A](fieldMapping, useDefaultArgs, allowUnknownKeys)` | Field names, defaults, strict keys for one case class |
| `KindlingsCoproductHint.Field[A](fieldName, transformConstructorNames)` | `FieldCoproductHint[A](fieldName)` with `fieldValue` overridden | Discriminator field and values for one sealed family |
| `KindlingsCoproductHint.Wrapped[A](transformConstructorNames)` | (custom `CoproductHint`) | Single-key objects `{ subtype-name { ... } }` |
| `KindlingsCoproductHint.FirstSuccess[A]()` | `FirstSuccessCoproductHint[A]` | No disambiguation: subtypes are attempted in the alphabetical order of their names, the first one read successfully wins; written as is |

Declare a coproduct hint with its concrete variant as the type (`implicit val hint: KindlingsCoproductHint.FirstSuccess[Source] = KindlingsCoproductHint.FirstSuccess()`).

### Failures

Derived readers report failures like upstream's derivation: every failing field of a case class (then each unknown key of a strict one), every failing item of a collection and value of a map, with upstream's failure reasons (`KeyNotFound` with candidate keys, `WrongType`, `UnknownKey`, `UnexpectedValueForFieldCoproductHint` for an unknown discriminator value, `NoValidCoproductOptionFound` when no subtype of a first-success family can be read).

### Tuples

Tuples (`(A, B)`, ... up to `Tuple22`) are read from and written as HOCON **lists**, like upstream's derivations: `(true, 5)` is `[true, 5]`. A list of another size fails with `WrongSizeList`, every failing element is reported at its index, and a value that is neither a list nor an object fails with `WrongType` (expecting a list). A value that is not a list but is an object is read from the tuple's product representation (`{ _1 = true, _2 = 5 }`), as upstream's Scala 2 derivation does (upstream's Scala 3 derivation reads only lists; Kindlings accepts the object form on both).

On Scala 3 every tuple type is supported, as with upstream's Scala 3 derivation: tuples with more than 22 elements
(`TupleXXL` at runtime), tuples written with `*:` (`Int *: String *: EmptyTuple`) and `EmptyTuple` (an empty list).
They are read from and written as lists with the same failures, but - as upstream's Scala 3 derivation - only from lists:
the `{ _1, _2 }` object form is read only for `TupleN` types (`(A, B)`, `Tuple2[A, B]`).

### Missing keys and types without fields

- The instance in scope for a field's type decides how a missing key is handled, as upstream: a reader that is a `pureconfig.ReadsMissingKeys` (upstream's `Option` reader, your own) is given the missing key, any other reader gets a `KeyNotFound`; a writer that is a `pureconfig.WritesMissingKeys` (upstream's `Option` writer, your own) decides whether the key is written at all. So an empty `Option` is omitted by default, but a custom `ConfigWriter[Option[A]]` that is not a `WritesMissingKeys` is used as is (and a custom `ConfigReader[Option[A]]` that is not a `ReadsMissingKeys` requires the key).
- Default values are evaluated only when the key is missing.
- A case object or a case class without parameters is read only from an object, and with `allowUnknownKeys = false` (the global config or a `KindlingsProductHint` for it) every key of that object is an `UnknownKey` (for a subtype of a sealed family: every key but the discriminator).

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@configKey("name")` | Override the HOCON key for a case class field |
| `@transientField` | Exclude a field from reading/writing (must have a default value) |

```scala
import hearth.kindlings.pureconfigderivation.annotations._

case class DatabaseConfig(
  @configKey("connection_string") connectionString: String,
  @transientField cachedPool: Option[Any] = None
)
```

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    sealed trait DatabaseType
    case class Postgres(host: String, port: Int, database: String) extends DatabaseType
    case class Sqlite(path: String) extends DatabaseType

    // Default config uses "type" discriminator and kebab-case names
    val reader = KindlingsConfigReader.derived[DatabaseType]

    val result = ConfigSource.string("""
      type = "postgres"
      host = "localhost"
      port = 5432
      database = "mydb"
    """).load[DatabaseType](reader)
    println(result)
    // expected output:
    // Right(Postgres(localhost,5432,mydb))
    ```

??? example "Wrapped subtypes (no discriminator)"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: PureConfig = PureConfig.default.withWrappedSubtypes

    val reader = KindlingsConfigReader.derived[Shape]

    val result = ConfigSource.string("""
      circle { radius = 5.0 }
    """).load[Shape](reader)
    println(result)
    // expected output:
    // Right(Circle(5.0))
    ```

??? example "Case class with defaults and strict decoding"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    implicit val config: PureConfig = PureConfig.default
      .withUseDefaults
      .withStrictDecoding

    case class AppSettings(
      host: String,
      port: Int = 8080,
      debug: Boolean = false
    )

    val reader = KindlingsConfigReader.derived[AppSettings]

    // Missing fields use Scala defaults
    val result = ConfigSource.string("""host = "localhost" """).load[AppSettings](reader)
    println(result)
    // expected output:
    // Right(AppSettings(localhost,8080,false))

    // Unknown keys cause an error in strict mode
    val strict = ConfigSource.string("""
      host = "localhost"
      unknown-key = "oops"
    """).load[AppSettings](reader)
    println(strict.isLeft)
    // expected output:
    // true
    ```

??? example "Snake case member names"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    implicit val config: PureConfig = PureConfig.default
      .withSnakeCaseMemberNames

    case class UserProfile(firstName: String, lastName: String, emailAddress: String)

    val reader = KindlingsConfigReader.derived[UserProfile]

    val result = ConfigSource.string("""
      first_name = "Alice"
      last_name = "Smith"
      email_address = "alice@example.com"
    """).load[UserProfile](reader)
    println(result)
    // expected output:
    // Right(UserProfile(Alice,Smith,alice@example.com))
    ```

??? example "Nested configuration"

    ```scala
    //> using dep com.kubuszok::kindlings-pureconfig-derivation:{{ kindlings_version() }}

    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    case class HttpConfig(host: String, port: Int)
    case class DatabaseConfig(url: String, pool: Int)
    case class AppConfig(http: HttpConfig, database: DatabaseConfig)

    val reader = KindlingsConfigReader.derived[AppConfig]

    val result = ConfigSource.string("""
      http {
        host = "0.0.0.0"
        port = 8080
      }
      database {
        url = "jdbc:postgresql://localhost/mydb"
        pool = 10
      }
    """).load[AppConfig](reader)
    println(result)
    // expected output:
    // Right(AppConfig(HttpConfig(0.0.0.0,8080),DatabaseConfig(jdbc:postgresql://localhost/mydb,10)))
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.pureconfigderivation.debug._
```

This enables the `LogDerivation` implicits of `KindlingsConfigReader` and `KindlingsConfigWriter` (there is no separate one for `KindlingsConfigConvert`: its derivation logs through the reader and writer ones), printing the derivation steps to the compiler output. Project-wide: `scalacOptions += "-Xmacro-settings:pureconfigDerivation.logDerivation=true"`.

## Comparison with pureconfig-generic

### Feature differences

| Feature | pureconfig-generic (Scala 2) | pureconfig-generic-scala3 | Kindlings |
|---------|----------------------------|--------------------------|-----------|
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |
| `derives` support (Scala 3) | N/A | `ConfigReader` only (Writer/Convert via semiauto) | `KindlingsConfigReader`, `KindlingsConfigWriter`, `KindlingsConfigConvert` |
| Recursive types | Needs `Lazy` | Yes | Just works |
| Tuples as lists | Yes (also read from `{ _1, _2 }` objects) | Yes (lists only) | Yes (also read from `{ _1, _2 }` objects) |
| Tuples with more than 22 elements, `*:`, `EmptyTuple` (Scala 3) | N/A | Yes (lists) | Yes (lists) |
| `ReadsMissingKeys` / `WritesMissingKeys` instances of field types | Yes | Yes | Yes |
| Named tuples | No | No | Yes |
| Opaque types | No | Partial | Yes |
| Scala 3 enums | No | Yes | Yes |
| Java enums | No | Yes | Yes |
| Per-type `ProductHint` / `CoproductHint` | Yes | Yes | Yes |
| `@ConfiguredJsonCodec`-style annotation | No | No | No |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

#### Write

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| SimpleCC | 2.13 | 11.0M | 1.2M | **9.1x faster** |
| SimpleCC | 3 | 11.1M | 1.7M | **6.6x faster** |
| Person | 2.13 | 1.2M | 205K | **5.8x faster** |
| Person | 3 | 1.2M | 248K | **4.7x faster** |

#### Read

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| SimpleCC | 2.13 | 17.2M | 1.4M | **12.5x faster** |
| SimpleCC | 3 | 17.2M | 1.4M | **12.5x faster** |
| Person | 2.13 | 1.0M | 216K | **4.8x faster** |
| Person | 3 | 1.0M | 197K | **5.3x faster** |

#### Tuples and `Option` fields

`PureconfigParityBenchmark` (Scala 3, `-prof gc`; a busy machine, so throughput is approximate, allocation is exact):

| Case | Kindlings | Original semi | Kindlings B/op | Original B/op |
|------|-----------|---------------|----------------|---------------|
| Tuples (as lists): read | 1.2M | 293K | 3824 | 8736 |
| Tuples (as lists): write | 1.6M | 538K | 1912 | 3700 |
| `Option` fields: read | 6.4M | 157K | 536 | 5212 |
| `Option` fields: write | 4.8M | 545K | 744 | 2260 |

!!! note
    Kindlings is 4.7--12.5x faster across the board, for both reads and writes, on both Scala versions and type complexities.

Note: Kindlings semi-automatic and automatic derivation produce identical performance -- this is the "sanely-automatic" design.
