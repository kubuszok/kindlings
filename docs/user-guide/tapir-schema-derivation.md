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
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}
    ```

## Quick start

??? example "Deriving Schema for a case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class Person(name: String, age: Int)

    // Semi-automatic
    val schema: Schema[Person] = KindlingsSchema.derived[Person].schema
    println(schema.name)
    // expected output:
    // Some(SName(Person,List()))
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsSchema.derived[A]` | `KindlingsSchema[A]` | Semi-automatic schema derivation |
| `KindlingsSchema.derived[A].schema` | `Schema[A]` | The underlying Tapir `Schema[A]` |
| `KindlingsSchema.derived[A]` | `KindlingsSchema[A]` | Sanely-automatic (given/implicit) |

Unlike other Kindlings modules, Tapir Schema derivation takes **no configuration parameter**. Instead, it automatically discovers JSON configuration from sibling modules (see below).

### Automatic JSON configuration discovery

When `kindlings-circe-derivation` or `kindlings-jsoniter-derivation` is on the classpath, `KindlingsSchema` reads their configuration at compile time and applies the same field name transforms. This means your **schema matches your codecs by design** — the API documentation (OpenAPI / Swagger) always reflects the actual JSON payloads, without maintaining a separate configuration.

For example, if your Circe `Configuration` uses `withSnakeCaseMemberNames`, the generated Tapir schema will also use `snake_case` field names — no additional configuration needed, no risk of drift.

#### Required and optional fields

A field is optional in the schema exactly when the decoder derived with the same configuration accepts it missing
from the JSON object:

| Field | jsoniter `JsoniterConfig` | circe `Configuration` |
|-------|---------------------------|-----------------------|
| `Option[A]` | optional | optional |
| has a default value | optional unless `requireDefaultFields` | optional with `useDefaults` |
| collection or map that can be empty (no default) | optional unless `requireCollectionFields` | required |
| non-empty collection (`NonEmptyList`, ... from `kindlings-cats-integration`) | required (unless it has a default value, see above) | required (likewise) |
| Scala 3 nullable type `T \| Null` | optional (and nullable) | like any other field of its type |
| anything else | required | required |

The `transient*` settings of a `JsoniterConfig` only change what the encoder writes, so they do not make fields
optional.

#### What a `JsoniterConfig` changes

With a jsoniter `JsoniterConfig` the schemas describe what the jsoniter codecs (derived with the same config) write:

- **ADTs without a discriminator** (Kindlings' default) are described as wrapped: each leaf is a single-field object
  keyed by the leaf's name (`{"Circle":{"radius":1.0}}`), and case objects are strings (`"Origin"`), also in ADTs
  mixing case classes and case objects. With `circeLikeObjectEncoding` case objects are wrapped too (`{"Origin":{}}`).
  With a discriminator, case objects are objects with just the discriminator field (`{"type":"Origin"}`).
- **Nested sealed hierarchies** are flattened to their leaves, as the codecs read and write only the leaves.
- **Leaf names**: `@named` on a leaf is used as is, otherwise `adtLeafClassNameMapper` receives the leaf's
  fully-qualified name (`pkg.Outer.Leaf`, Scala 3 enum cases as `pkg.Enum.Case`), as in the codecs.
- **Java enums and `scala.Enumeration`** values are strings whatever the discriminator (Java enum values named by
  `javaEnumValueNameMapper`, `Enumeration` values by their `toString`), or integer `id`s with `useScalaEnumValueId`.
- **Tuples** are arrays: positional `_1`, `_2`, ... fields marked with Tapir's `Schema.Tuple` attribute, which Tapir's
  docs interpreter renders as an array with `prefixItems`.
- **Fields**: `@named` on a field renames it like `@fieldName`.
- **`Option[Option[A]]`**: the inner `Option` is described as the ADT the codecs write it as (`{"Some":{"value":1}}` /
  `"None"`, or `{"type":"Some","value":1}` / `{"type":"None"}` with a discriminator), unless `skipNestedOptionValues`.

#### What a circe `Configuration` changes

With a circe `Configuration`, the schema also follows how circe codes sealed hierarchies and value classes:

- without a discriminator, every subtype is shown in the wrapped form circe writes (`{"Circle":{"radius":1.0}}`, a
  product with a single field named after the subtype); with a discriminator, a subtype that is not a JSON object is
  shown wrapped (it cannot carry the discriminator);
- nested sealed hierarchies are flattened to their leaves, like circe writes them; an intermediate sealed trait with a
  hand-written circe `Encoder` is kept as one subtype (its schema is the intermediate's `Schema`), one whose instance
  was derived by Kindlings is flattened;
- with `unwrapValueClasses = false` (`withoutValueClassUnwrapping`), `AnyVal` case classes are shown as objects.

#### Disambiguating multiple JSON configs

If both Circe and Jsoniter configurations are on the classpath, the macro finds multiple configs and fails. Import an implicit `PreferSchemaConfig` to tell the macro which one to use:

```scala
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.tapirschemaderivation._
import hearth.kindlings.circederivation._

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
| `@encodedExample(value)` | Type, Field | Add an OpenAPI encoded example |
| `@validateEach(validator)` | Field | Validate the elements of a collection/`Option` field |
| `@customise(schema => schema)` | Type, Field | Apply an arbitrary transform to the derived schema |

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
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    val schema: Schema[Shape] = KindlingsSchema.derived[Shape].schema
    println(schema.name)
    // Some(SName(Shape,List()))
    ```

??? example "Schema with Tapir annotations"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema
    import sttp.tapir.Schema.annotations._
    import sttp.tapir.Validator

    @description("A person with metadata")
    @title("PersonMeta")
    case class AnnotatedPerson(
      @description("The name") name: String,
      @format("int32") age: Int
    )

    val schema: Schema[AnnotatedPerson] = KindlingsSchema.derived[AnnotatedPerson].schema
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
    val schema: Schema[UserProfile] = KindlingsSchema.derived[UserProfile].schema
    // Field names: first_name, last_name (matching JSON encoding)
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class TreeNode(value: Int, children: List[TreeNode])

    // Recursive types work out of the box — uses schema references
    val schema: Schema[TreeNode] = KindlingsSchema.derived[TreeNode].schema
    ```

??? example "Generic types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-tapir-schema-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.softwaremill.sttp.tapir::tapir-core:{{ libraries.tapir }}

    import hearth.kindlings.tapirschemaderivation._
    import hearth.kindlings.circederivation._
    import sttp.tapir.Schema

    case class Box[A](value: A)
    case class Person(name: String, age: Int)

    implicit val personSchema: Schema[Person] = KindlingsSchema.derived[Person].schema
    println(personSchema.name)
    // Some(SName(Person,List()))

    // Box[Person] picks up the Person schema via implicit
    lazy val boxSchema: Schema[Box[Person]] = KindlingsSchema.derived[Box[Person]].schema
    println(boxSchema.name)
    // Some(SName(Box,List(Person)))
    ```

Generic types can also take the schemas of their type parameters as `KindlingsSchema` evidence — e.g. with Scala 3
`case class Box[A](value: A) derives KindlingsSchema`, or on Scala 2.13 with
`implicit def boxSchema[A: KindlingsSchema]: KindlingsSchema[Box[A]] = KindlingsSchema.derived[Box[A]]` — and the schema
name includes the actual type parameter (e.g. `Box[Int]`).

## Maps, collections and other built-in types

Kindlings derives schemas for `Option`, collections, `Map`s and `Either` itself (consistently with the JSON library's
configuration, e.g. jsoniter's `mapAsArray`) instead of using Tapir's built-in `Schema.schemaForOption`,
`schemaForIterable`, `schemaForSet`, `schemaForArray`, `schemaForMap`, `schemaForEither` and
`derivedEnumerationValue`. The result is the same whether or not the element types have an implicit `Schema`:

- `Set`s are marked with unique items (as Tapir does),
- `Map`s with `String` keys are named like Tapir's `Schema.schemaForMap` (`Map_V`, e.g. a `Map_Double` component in
  OpenAPI), and those with other keys like `Schema.schemaForMap[K, V](keyToString)` (`Map_K_V`),
- collections and maps that cannot be empty (cats' `NonEmptyList`, `NonEmptyVector`, `NonEmptyChain`, `NonEmptySet`,
  `NonEmptyMap`, ... with `kindlings-cats-integration`) are required and validated with `Validator.minSize(1)`
  (`minItems: 1` in OpenAPI / JSON Schema), as in tapir-cats,
- `Either` and `scala.Enumeration` values are derived like the sealed hierarchies the JSON libraries encode them as,
- `Map`s are open products (`additionalProperties`) keyed by the strings the JSON codecs write as object keys: `String`
  keys as they are; primitives (and their boxes), `UUID`, `BigInt`, `BigDecimal` and `java.time` keys as their
  `toString` (with circe, through its `KeyEncoder`, also a user-provided one); and value types wrapping any of those —
  `AnyVal` classes, opaque types (e.g. `opaque type Counter = Int`, written as `{"7":...}`), and — with the
  corresponding integration module — refined, Iron, neotype and scala-newtype types — as their underlying value. For
  any other key type, provide a `Schema[Map[K, V]]`, e.g. with Tapir's `Schema.schemaForMap[K, V](keyToString)`.

A value type wrapping a collection or map (an `AnyVal` class, an opaque type, an Iron `List[A] :| MinLength[1]`, ...)
is coded as that collection, but the decoders do not fill it in when it is absent, so — unlike a plain collection — it is
a required field (unless it has a default value).

With a discriminator, each subtype's schema gets the discriminator field appended (unless the subtype already has it),
carrying the subtype's value (`"const": "Circle"` in OpenAPI / JSON Schema), as Tapir's own derivation does.

Your own implicit `Schema` for any of these types (e.g. `Schema[Map[UserId, Int]]`, `Schema[Option[Int]]`) always
takes precedence. Tapir's schemas for primitive and other leaf types (`String`, `Int`, `UUID`, `Instant`, ...) are
used as-is, and `Char` is treated as a `String`.

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
| Scala 2.13 support | Yes (Magnolia) | Yes |
| Same API on Scala 2.13 and 3 | No (Magnolia on 2.13, Mirrors on 3) | Yes |
| Sanely-automatic derivation | No | Yes |
| Recursive types | Manual (`implicit lazy val` on 2.13, `implicit def` on 3) | Just works |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| Java enums | Partial | Yes |
| Automatic JSON config discovery | No | Yes |
| Tapir annotation support | Yes | Yes |
| Value types (`AnyVal`) | Scala 2.13 only (on Scala 3 a hand-written schema is needed) | Yes |
| Non-`String` map keys (`Int`, `UUID`, value types, opaque types, refined, ...) | No (needs `Schema.schemaForMap`) | Yes |
| `Char` | No | Yes (as `String`) |

### Checked against Tapir

The `upstream-oracle-tests` module derives the same types with Kindlings and with Tapir's own derivation (and with
tapir-cats and tapir-iron) and compares the JSON Schemas Tapir's docs interpreter renders for them, on Scala 2.13 and 3.
The only differences are the documented ones: required and optional fields follow the decoder (see "Required and
optional fields" above; e.g. with circe, collections are required, while Tapir marks every collection optional), and
Iron constraints become validators only with tapir-iron's schemas imported (see the Iron integration guide).

### Benchmarks

Tapir Schema derivation generates a runtime `Schema[A]` value. At runtime, accessing the schema is just a field read, so there is no meaningful performance difference between implementations:

| Benchmark | Kindlings | Tapir built-in |
|-----------|-----------|---------------|
| Schema field access | ~3B ops/s | ~3B ops/s |

The performance is identical because both approaches produce the same runtime representation -- the work is done entirely at compile time.
