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
    implicit val reader: ConfigReader[DatabaseConfig] = ConfigReader.derived[DatabaseConfig]

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
| `ConfigReader.derived[A]` | `ConfigReader[A]` | Semi-automatic reader |
| `ConfigReader.derived[A]` | `ConfigReader[A]` | Sanely-automatic reader (given on Scala 3) |
| `ConfigWriter.derived[A]` | `ConfigWriter[A]` | Semi-automatic writer |
| `ConfigWriter.derived[A]` | `ConfigWriter[A]` | Sanely-automatic writer (given on Scala 3) |
| `ConfigCodec.derived[A]` | `ConfigCodec[A]` | Semi-automatic codec (reader + writer) |
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

| Hint | Upstream PureConfig equivalent | Description |
|------|------|-------------|
| `CoproductHint.Field[A](fieldName, transform)` | `FieldCoproductHint[A]` | Discriminator-based (default): the value must be an object; the subtype is read without the discriminator key |
| `CoproductHint.Wrapped[A](transform)` | (custom `CoproductHint`) | Single-key wrapping: `{"variant-name": {...}}` |
| `CoproductHint.FirstSuccess[A]()` | `FirstSuccessCoproductHint[A]` | No disambiguation: subtypes are attempted in the alphabetical order of their names, the first one read successfully wins; written as is |

Declare a coproduct hint with its concrete variant as the type (`implicit val hint: CoproductHint.FirstSuccess[Source] = CoproductHint.FirstSuccess()`).

Without a discriminator (`withWrappedSubtypes` or a `Wrapped` hint), a sealed family of case objects is read from strings (`north`), like PureConfig's `deriveEnumerationReader`; with a discriminator its values are objects (`{ type = north }`). Writing a subtype that has a field named like the discriminator throws `CoproductHintException`.

### Behaviour follows PureConfig

sconfig has no derivation of its own; derived instances behave like upstream PureConfig's derivation (as `kindlings-pureconfig-derivation` does), with sconfig's own failure types. A test in the module runs the same scenarios as pureconfig-derivation's and asserts the same outcomes. In particular:

- readers report **every** failure: each failing field of a case class (then each unknown key of a strict one), each failing item of a collection, each failing value of a map;
- a missing key fails with `KeyNotFound`, which lists the object's keys that look like the expected key under a different naming (e.g. `firstName` for `first-name`); readers of missing keys (`Option` fields, readers extending `ReadsMissingKeys`, which are called with `null`) read it instead;
- an empty `Option` field is omitted when writing, not written as `null`; more generally the instance in scope for a
  field's type decides how a missing key is handled: a reader extending `ReadsMissingKeys` is given the missing key, a
  writer extending `WritesMissingKeys[A]` decides with `toOpt` whether the key is written (so a custom plain
  `ConfigWriter[Option[A]]` writes empty values too, and a custom plain `ConfigReader[Option[A]]` requires the key);
- tuples are read from and written as **lists** (`(true, 5)` is `[true, 5]`): a list of another size fails with
  `WrongSizeList`, each failing element is reported under its index; an object is read from the product representation
  (`{ _1 = true, _2 = 5 }`);
- a case object or a case class without parameters is read only from an object, and with `allowUnknownKeys = false`
  every key of it is an `UnknownKey` (for a subtype of a sealed family: every key but the discriminator);
- default values are evaluated only when the key is missing;
- as in PureConfig, the implicit instances a derived reader/writer uses for its fields are resolved once, when it is
  created (an `implicit def` instance is called once per derived instance, not on every read/write), and so are the
  subtype names a config's `transformConstructorNames` gives, also for a config only known at runtime.

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

    implicit val reader: ConfigReader[DbBackend] = ConfigReader.derived[DbBackend]

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

    implicit val reader: ConfigReader[Server] = ConfigReader.derived[Server]

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

    implicit val httpReader: ConfigReader[HttpConfig] = ConfigReader.derived[HttpConfig]
    implicit val dbReader: ConfigReader[DbConfig] = ConfigReader.derived[DbConfig]
    implicit val appReader: ConfigReader[AppConfig] = ConfigReader.derived[AppConfig]

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

    implicit val writer: ConfigWriter[AppConfig] = ConfigWriter.derived[AppConfig]

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

| Error type | Upstream PureConfig equivalent | Description |
|-----------|------|-------------|
| `ConfigDecodingError.KeyNotFound` | `KeyNotFound` | Required key is missing (with candidate keys) |
| `ConfigDecodingError.WrongType` | `WrongType` | Value has unexpected HOCON type |
| `ConfigDecodingError.CannotConvert` | `CannotConvert` | Value cannot be converted to target type |
| `ConfigDecodingError.WrongSizeList` | `WrongSizeList` | A tuple read from a list of another size |
| `ConfigDecodingError.UnknownKey` | `UnknownKey` | Unexpected key when `withStrictDecoding` is active |
| `ConfigDecodingError.UnexpectedValueForFieldCoproductHint` | `UnexpectedValueForFieldCoproductHint` | The discriminator names no subtype |
| `ConfigDecodingError.NoValidCoproductOptionFound` | `NoValidCoproductOptionFound` | No subtype of a `FirstSuccess` family could be read (lists every attempt) |
| `ConfigDecodingError.Multiple` | `ConfigReaderFailures` | Aggregation of multiple errors (`ConfigDecodingError.flatten` lists them) |
| `ConfigDecodingError.Missing` | | Required key is missing; not reported by derived readers (kept for hand-written ones) |

All errors carry a `path` for pinpointing the offending value, relative to the value the reader was called with. As in PureConfig, a missing key is reported at the object that lacks it, any other failure at the offending value (a field's key, a list index, a map key):

```scala
val error: ConfigDecodingError = result.left.get
println(error.getMessage)
// Key not found: 'host'. (at db)
```

Writers throw `CoproductHintException` (with `ConfigDecodingError.CollidingKeys`) when a subtype written with a discriminator has a field named like the discriminator.

## Comparison with PureConfig

### Feature differences

| Feature | PureConfig | Kindlings sconfig |
|---------|-----------|-------------------|
| Same API on Scala 2.13 and 3 | No (different modules, different APIs) | Yes |
| Cross-platform (JS / Native) | No (JVM-only, depends on `com.typesafe:config`) | Yes (via sconfig) |
| Kebab-case field names by default | Yes | Yes |
| Discriminator-based ADTs | Yes | Yes (default: `"type"` field) |
| Wrapped subtypes | Yes | Yes (`withWrappedSubtypes`) |
| First-success coproduct hint | Yes | Yes (`CoproductHint.FirstSuccess`, same attempt order) |
| Failure accumulation, candidate keys for missing keys | Yes | Yes (own `ConfigDecodingError` types) |
| Per-type `ProductHint` / `CoproductHint` | Yes | Yes |
| Strict decoding | Yes | Yes (`withStrictDecoding`) |
| Default values | Yes | Yes (enabled by default) |
| `@ConfiguredJsonCodec`-style annotation | No | No |
| Recursive types | Needs workarounds | Just works |
| Tuples as lists | Yes | Yes |
| `ReadsMissingKeys` / `WritesMissingKeys` instances of field types | Yes | Yes (own traits of the same names) |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Scala 3 enums | Partial | Yes |
| Java enums | Partial | Yes |
