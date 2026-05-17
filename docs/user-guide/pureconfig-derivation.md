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
    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    case class ServerConfig(host: String, port: Int, debug: Boolean)

    // Semi-automatic derivation
    val reader: ConfigReader[ServerConfig] = KindlingsConfigReader.derive[ServerConfig]
    val writer: ConfigWriter[ServerConfig] = KindlingsConfigWriter.derive[ServerConfig]

    // Read from HOCON string
    val result = ConfigSource.string("""
      host = "localhost"
      port = 8080
      debug = false
    """).load[ServerConfig](reader)
    println(result)
    // Right(ServerConfig(localhost,8080,false))

    // Write to ConfigValue
    val config = writer.to(ServerConfig("example.com", 443, true))
    println(config.render())
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsConfigReader.derive[A]` | `ConfigReader[A]` | Semi-automatic reader |
| `KindlingsConfigReader.derived[A]` | `KindlingsConfigReader[A]` | Sanely-automatic (given/implicit) |
| `KindlingsConfigWriter.derive[A]` | `ConfigWriter[A]` | Semi-automatic writer |
| `KindlingsConfigWriter.derived[A]` | `KindlingsConfigWriter[A]` | Sanely-automatic (given/implicit) |
| `KindlingsConfigConvert.derive[A]` | `ConfigConvert[A]` | Semi-automatic reader + writer |
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
    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    sealed trait DatabaseType
    case class Postgres(host: String, port: Int, database: String) extends DatabaseType
    case class Sqlite(path: String) extends DatabaseType

    // Default config uses "type" discriminator and kebab-case names
    val reader = KindlingsConfigReader.derive[DatabaseType]

    val result = ConfigSource.string("""
      type = "postgres"
      host = "localhost"
      port = 5432
      database = "mydb"
    """).load[DatabaseType](reader)
    println(result)
    // Right(Postgres(localhost,5432,mydb))
    ```

??? example "Wrapped subtypes (no discriminator)"

    ```scala
    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: PureConfig = PureConfig.default.withWrappedSubtypes

    val reader = KindlingsConfigReader.derive[Shape]

    val result = ConfigSource.string("""
      circle { radius = 5.0 }
    """).load[Shape](reader)
    println(result)
    // Right(Circle(5.0))
    ```

??? example "Case class with defaults and strict decoding"

    ```scala
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

    val reader = KindlingsConfigReader.derive[AppSettings]

    // Missing fields use Scala defaults
    val result = ConfigSource.string("""host = "localhost" """).load[AppSettings](reader)
    println(result)
    // Right(AppSettings(localhost,8080,false))

    // Unknown keys cause an error in strict mode
    val strict = ConfigSource.string("""
      host = "localhost"
      unknown-key = "oops"
    """).load[AppSettings](reader)
    println(strict)
    // Left(ConfigReaderFailures(...))
    ```

??? example "Snake case member names"

    ```scala
    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    implicit val config: PureConfig = PureConfig.default
      .withSnakeCaseMemberNames

    case class UserProfile(firstName: String, lastName: String, emailAddress: String)

    val reader = KindlingsConfigReader.derive[UserProfile]

    val result = ConfigSource.string("""
      first_name = "Alice"
      last_name = "Smith"
      email_address = "alice@example.com"
    """).load[UserProfile](reader)
    println(result)
    // Right(UserProfile(Alice,Smith,alice@example.com))
    ```

??? example "Nested configuration"

    ```scala
    import hearth.kindlings.pureconfigderivation._
    import pureconfig._

    case class HttpConfig(host: String, port: Int)
    case class DatabaseConfig(url: String, pool: Int)
    case class AppConfig(http: HttpConfig, database: DatabaseConfig)

    val reader = KindlingsConfigReader.derive[AppConfig]

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
    // Right(AppConfig(HttpConfig(0.0.0.0,8080),DatabaseConfig(jdbc:postgresql://localhost/mydb,10)))
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.pureconfigderivation.debug._
```

This enables `LogDerivation` implicits for `KindlingsConfigReader`, `KindlingsConfigWriter`, and `KindlingsConfigConvert`, printing the derivation steps to the compiler output.

## Comparison with pureconfig-generic

### Feature differences

| Feature | pureconfig-generic (Scala 2) | pureconfig-generic-scala3 | Kindlings |
|---------|----------------------------|--------------------------|-----------|
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |
| `derives` support on Scala 2.13 | No | N/A | Yes |
| Recursive types | Needs `Lazy` | Yes | Just works |
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
| SimpleCC | 2.13 | 11.0M | 1.2M | **9.2x faster** |
| SimpleCC | 3 | 10.9M | 1.6M | **6.8x faster** |
| Person | 2.13 | 1.2M | 200.4K | **6.0x faster** |
| Person | 3 | 1.2M | 247.3K | **4.9x faster** |

#### Read

| Type | Scala | Kindlings | Original semi | vs original |
|------|-------|-----------|--------------|------------|
| SimpleCC | 2.13 | 16.8M | 1.4M | **12.0x faster** |
| SimpleCC | 3 | 11.9M | 1.4M | **8.5x faster** |
| Person | 2.13 | 999.8K | 208.9K | **4.8x faster** |
| Person | 3 | 924.8K | 200.8K | **4.6x faster** |

!!! note
    Kindlings is 4.6--12x faster across the board, for both reads and writes, on both Scala versions and type complexities.

Note: Kindlings semi-automatic and automatic derivation produce identical performance -- this is the "sanely-automatic" design.
