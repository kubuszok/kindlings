# sconfig Derivation (HOCON)

Derives `ConfigReader`, `ConfigWriter`, and `ConfigCodec` for HOCON configuration using [sconfig](https://github.com/ekrich/sconfig), a cross-platform port of Typesafe Config. Unlike PureConfig (which depends on JVM-only `com.typesafe:config`), sconfig works on JVM, Scala.js, and Scala Native.

!!! tip "JVM-only projects using PureConfig?"
    If your project is JVM-only and already uses PureConfig, see [kindlings-pureconfig-derivation](pureconfig-derivation.md) — it provides drop-in replacement instances that extend PureConfig's own `ConfigReader`/`ConfigWriter` types.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-sconfig-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-sconfig-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    ```

!!! note
    You also need `sconfig` as a runtime dependency:

    ```scala
    libraryDependencies += "org.ekrich" %%% "sconfig" % "{{ libraries.sconfig }}"
    ```

## Quick start

??? example "Reading and writing HOCON configuration"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep org.ekrich::sconfig:{{ libraries.sconfig }}

    import hearth.kindlings.sconfigderivation._
    import org.ekrich.config.ConfigFactory

    case class DatabaseConfig(hostName: String, portNumber: Int, maxConnections: Int)

    // Derive a reader -- default config uses kebab-case field names
    implicit val reader: ConfigReader[DatabaseConfig] = ConfigReader.derive[DatabaseConfig]

    // Parse HOCON (kebab-case keys map to camelCase fields by default)
    val config = ConfigFactory.parseString("""
      host-name = "localhost"
      port-number = 5432
      max-connections = 10
    """)

    val result = reader.from(config.root)
    println(result)
    // expected output:
    // Right(DatabaseConfig(localhost,5432,10))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `ConfigReader.derive[A]` | `ConfigReader[A]` | Semi-automatic reader |
| `ConfigReader.derived[A]` | `ConfigReader[A]` | Sanely-automatic reader (given on Scala 3) |
| `ConfigWriter.derive[A]` | `ConfigWriter[A]` | Semi-automatic writer |
| `ConfigWriter.derived[A]` | `ConfigWriter[A]` | Sanely-automatic writer (given on Scala 3) |
| `ConfigCodec.derive[A]` | `ConfigCodec[A]` | Semi-automatic codec (reader + writer) |
| `ConfigCodec.derived[A]` | `ConfigCodec[A]` | Sanely-automatic codec (given on Scala 3) |

All methods take an implicit/using `SConfig` parameter (defaults to `SConfig.default`).

### Combinators

`ConfigReader` and `ConfigWriter` provide combinators for transforming instances:

```scala
// Map the result of a reader
val intReader: ConfigReader[Int] = ConfigReader[Int]
val positiveReader: ConfigReader[Int] = intReader.emap { i =>
  if (i > 0) Right(i)
  else Left(s"Expected positive number, got $i")
}

// Contramap a writer
val stringWriter: ConfigWriter[String] = ConfigWriter[String]
val uuidWriter: ConfigWriter[java.util.UUID] = stringWriter.contramap(_.toString)
```

### Type hierarchy

`ConfigReader[A]`, `ConfigWriter[A]`, and `ConfigCodec[A]` are all defined by the kindlings sconfig module. `ConfigCodec[A]` extends both `ConfigReader[A]` and `ConfigWriter[A]`.

## Configuration

All derivation methods accept an implicit `SConfig`. The defaults mirror PureConfig's conventions:

```scala
import hearth.kindlings.sconfigderivation._

implicit val config: SConfig = SConfig.default
// Equivalent to:
// SConfig(
//   transformMemberNames   = ConfigFieldMapping(CamelCase, KebabCase),  // myField -> my-field
//   transformConstructorNames = ConfigFieldMapping(PascalCase, KebabCase), // MyType -> my-type
//   discriminator          = Some("type"),
//   useDefaults            = true,
//   allowUnknownKeys       = true
// )
```

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
| `withWrappedSubtypes` | No discriminator -- use single-key wrapping instead |
| `withUseDefaults` | Use case class default values for missing keys (default) |
| `withoutUseDefaults` | Require all keys |
| `withAllowUnknownKeys` | Ignore unexpected HOCON keys (default) |
| `withStrictDecoding` | Fail on unexpected HOCON keys |

### Per-type hints

For fine-grained control over individual types, use `ProductHint` and `CoproductHint`:

```scala
import hearth.kindlings.sconfigderivation._

case class MyType(myField: String, otherField: Int)

// Override naming for just this type
implicit val myTypeHint: ProductHint[MyType] =
  ProductHint[MyType](transformMemberNames = ConfigFieldMapping(CamelCase, SnakeCase))
```

`CoproductHint` controls sealed trait encoding strategy:

| Hint | Description |
|------|-------------|
| `CoproductHint.Field[A](fieldName, transform)` | Discriminator-based (default) |
| `CoproductHint.Wrapped[A](transform)` | Single-key wrapping: `{"variant-name": {...}}` |
| `CoproductHint.FirstSuccess[A]()` | Try every subtype reader in order |

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@configKey("hocon_key")` | Override HOCON key for a case class field |
| `@transientField` | Exclude a field from reading/writing (must have a default value) |

```scala
import hearth.kindlings.sconfigderivation.annotations._

case class AppConfig(
  @configKey("app-name") name: String,
  @transientField internalId: Long = 0L
)
```

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep org.ekrich::sconfig:{{ libraries.sconfig }}

    import hearth.kindlings.sconfigderivation._
    import org.ekrich.config.ConfigFactory

    sealed trait DbBackend
    case class Postgres(host: String, port: Int) extends DbBackend
    case class Sqlite(path: String) extends DbBackend

    implicit val reader: ConfigReader[DbBackend] = ConfigReader.derive[DbBackend]

    val config = ConfigFactory.parseString("""
      type = "postgres"
      host = "localhost"
      port = 5432
    """)

    println(reader.from(config.root))
    // expected output:
    // Right(Postgres(localhost,5432))
    ```

??? example "Strict decoding and unknown key rejection"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep org.ekrich::sconfig:{{ libraries.sconfig }}

    import hearth.kindlings.sconfigderivation._
    import org.ekrich.config.ConfigFactory

    implicit val config: SConfig = SConfig.default.withStrictDecoding

    case class Server(host: String, port: Int)

    implicit val reader: ConfigReader[Server] = ConfigReader.derive[Server]

    // This will fail -- "debug" is not a field of Server
    val result = reader.from(ConfigFactory.parseString("""
      host = "localhost"
      port = 8080
      debug = true
    """).root)

    println(result.isLeft)
    // expected output:
    // true
    ```

??? example "Nested configuration with defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep org.ekrich::sconfig:{{ libraries.sconfig }}

    import hearth.kindlings.sconfigderivation._
    import org.ekrich.config.ConfigFactory

    case class HttpConfig(host: String = "0.0.0.0", port: Int = 8080)
    case class DbConfig(url: String, poolSize: Int = 10)
    case class AppConfig(http: HttpConfig, db: DbConfig)

    implicit val httpReader: ConfigReader[HttpConfig] = ConfigReader.derive[HttpConfig]
    implicit val dbReader: ConfigReader[DbConfig] = ConfigReader.derive[DbConfig]
    implicit val appReader: ConfigReader[AppConfig] = ConfigReader.derive[AppConfig]

    val result = appReader.from(ConfigFactory.parseString("""
      http {
        port = 9090
      }
      db {
        url = "jdbc:postgresql://localhost/mydb"
      }
    """).root)

    println(result)
    // expected output:
    // Right(AppConfig(HttpConfig(0.0.0.0,9090),DbConfig(jdbc:postgresql://localhost/mydb,10)))
    ```

??? example "Writing configuration back to HOCON"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-sconfig-derivation:{{ kindlings_version() }}
    //> using dep org.ekrich::sconfig:{{ libraries.sconfig }}

    import hearth.kindlings.sconfigderivation._

    case class AppConfig(appName: String, maxRetries: Int, debug: Boolean)

    implicit val writer: ConfigWriter[AppConfig] = ConfigWriter.derive[AppConfig]

    val configValue = writer.to(AppConfig("my-app", 3, false))
    println(configValue.render)
    // {
    //     "app-name" : "my-app",
    //     "debug" : false,
    //     "max-retries" : 3
    // }
    ```

## Debugging

Enable debug logging to see the derivation process:

```scala
import hearth.kindlings.sconfigderivation.debug._
```

## Error handling

`ConfigReader` returns `Either[ConfigDecodingError, A]` with structured errors:

| Error type | Description |
|-----------|-------------|
| `ConfigDecodingError.Missing` | Required key is missing |
| `ConfigDecodingError.WrongType` | Value has unexpected HOCON type |
| `ConfigDecodingError.CannotConvert` | Value cannot be converted to target type |
| `ConfigDecodingError.UnknownKey` | Unexpected key when `withStrictDecoding` is active |
| `ConfigDecodingError.Multiple` | Aggregation of multiple errors |

All errors carry a `path` for pinpointing the offending field:

```scala
val error: ConfigDecodingError = result.left.get
println(error.getMessage)
// Missing required key 'host' (at db.host)
```

## Comparison with PureConfig

### Feature differences

| Feature | PureConfig | Kindlings sconfig |
|---------|-----------|-------------------|
| Same API on Scala 2.13 and 3 | No (different modules, different APIs) | Yes |
| Cross-platform (JS / Native) | No (JVM-only, depends on `com.typesafe:config`) | Yes (via sconfig) |
| Kebab-case field names by default | Yes | Yes |
| Discriminator-based ADTs | Yes | Yes (default: `"type"` field) |
| Wrapped subtypes | Yes | Yes (`withWrappedSubtypes`) |
| First-success coproduct hint | Yes | Yes (`CoproductHint.FirstSuccess`) |
| Per-type `ProductHint` / `CoproductHint` | Yes | Yes |
| Strict decoding | Yes | Yes (`withStrictDecoding`) |
| Default values | Yes | Yes (enabled by default) |
| `@ConfiguredJsonCodec`-style annotation | No | No |
| Recursive types | Needs workarounds | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Scala 3 enums | Partial | Yes |
| Java enums | Partial | Yes |
