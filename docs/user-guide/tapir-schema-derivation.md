# Tapir Schema Derivation

Drop-in replacement for Tapir's built-in `Schema.derived` -- derives `Schema[A]` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-tapir-schema-derivation" % "{{ kindlings_version() }}"
    ```

    You also need `tapir-core`:

    ```scala
    libraryDependencies += "com.softwaremill.sttp.tapir" %%% "tapir-core" % "{{ libraries.tapir }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}
    ```

## Quick start

??? example "Deriving Schema for a case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    case class Person(name: String, age: Int)

    // Semi-automatic
    val schema: Schema[Person] = KindlingsSchema.derive[Person]
    println(schema.name)
    // Some(SName(Person,List()))
    println(schema.schemaType)
    // SProduct(List(SProductField(FieldName(name,name),Schema(...),...),...))

    // Sanely-automatic — resolved by the compiler
    implicitly[Schema[Person]]
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsSchema.derive[A]` | `Schema[A]` | Semi-automatic schema derivation |
| `KindlingsSchema.derived[A]` | `KindlingsSchema[A]` | Sanely-automatic (given/implicit) |

Unlike other Kindlings modules, Tapir Schema derivation takes **no configuration parameter**. Instead, it automatically discovers JSON configuration from sibling modules (see below).

### Automatic JSON configuration discovery

When `kindlings-circe-derivation` or `kindlings-jsoniter-derivation` is on the classpath, `KindlingsSchema` reads their configuration at compile time and applies the same field name transforms. This means your **schema matches your codecs by design** — the API documentation (OpenAPI / Swagger) always reflects the actual JSON payloads, without maintaining a separate configuration.

For example, if your Circe `Configuration` uses `withSnakeCaseMemberNames`, the generated Tapir schema will also use `snake_case` field names — no additional configuration needed, no risk of drift.

#### Disambiguating multiple JSON configs

If both Circe and Jsoniter configurations are on the classpath, the macro finds multiple configs and fails. Import an implicit `PreferSchemaConfig` to tell the macro which one to use:

```scala
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation._

implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]
```

## Tapir annotations

`KindlingsSchema` supports Tapir's standard annotations from `sttp.tapir.Schema.annotations`:

| Annotation | Target | Description |
|-----------|--------|-------------|
| `@description("text")` | Type, Field | Add OpenAPI description |
| `@title("text")` | Type | Set the schema title |
| `@encodedName("name")` | Field | Override the encoded field name |
| `@format("fmt")` | Field | Set the schema format (e.g. `"int32"`, `"email"`) |
| `@validate(validator)` | Field | Add a field validator |
| `@default(value)` | Field | Set the default value |
| `@deprecated` | Type | Mark schema as deprecated |
| `@hidden` | Field | Hide field from the schema |

```scala
import sttp.tapir.Schema.annotations._
import sttp.tapir.Validator

@description("A user account")
@title("User")
case class User(
  @description("The username") name: String,
  @format("int32") @validate(Validator.min(0)) age: Int,
  @hidden internalId: Long
)
```

## Usage examples

??? example "Sealed trait schema"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    val schema: Schema[Shape] = KindlingsSchema.derive[Shape]
    println(schema.name)
    // Some(SName(Shape,List()))
    ```

??? example "Schema with Tapir annotations"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema
    import sttp.tapir.Schema.annotations._
    import sttp.tapir.Validator

    @description("A person with metadata")
    @title("PersonMeta")
    case class AnnotatedPerson(
      @description("The name") name: String,
      @format("int32") age: Int
    )

    val schema: Schema[AnnotatedPerson] = KindlingsSchema.derive[AnnotatedPerson]
    println(schema.description)
    // Some(A person with metadata)
    ```

??? example "Schema with JSON config discovery"

    ```scala
    import hearth.kindlings.circederivation._
    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    // Circe configuration with snake_case
    implicit val circeConfig: Configuration = Configuration.default
      .withSnakeCaseMemberNames

    case class UserProfile(firstName: String, lastName: String)

    // Schema automatically picks up snake_case from Circe config
    val schema: Schema[UserProfile] = KindlingsSchema.derive[UserProfile]
    // Field names: first_name, last_name (matching JSON encoding)
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types work out of the box — uses schema references
    val schema: Schema[TreeNode] = KindlingsSchema.derive[TreeNode]
    ```

??? example "Generic types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import sttp.tapir.Schema

    case class Box[A](value: A)
    case class Person(name: String, age: Int)

    implicit val personSchema: Schema[Person] = KindlingsSchema.derive[Person]
    val boxSchema: Schema[Box[Person]] = KindlingsSchema.derive[Box[Person]]
    println(boxSchema.name)
    // Some(SName(Box,List(Person)))
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.tapirschemaderivation.debug._
```

This enables the `LogDerivation` implicit for `KindlingsSchema`, printing the derivation steps to the compiler output.

## Comparison with Tapir built-in

### Feature differences

| Feature | Tapir built-in (`Schema.derived`) | Kindlings |
|---------|----------------------------------|-----------|
| Scala 2.13 support | No (Scala 3 Mirrors only) | Yes |
| Same API on Scala 2.13 and 3 | No | Yes |
| Sanely-automatic derivation | No | Yes |
| Recursive types | Manual `implicit lazy val` | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Java enums | Partial | Yes |
| Automatic JSON config discovery | No | Yes |
| Tapir annotation support | Yes | Yes |
| Value types (`AnyVal`) | Yes | Yes |

### Benchmarks

Tapir Schema derivation generates a runtime `Schema[A]` value. At runtime, accessing the schema is just a field read, so there is no meaningful performance difference between implementations:

| Benchmark | Kindlings | Tapir built-in |
|-----------|-----------|---------------|
| Schema field access | ~3.7B ops/s | ~3.7B ops/s |

The performance is identical because both approaches produce the same runtime representation -- the work is done entirely at compile time.
