# Jsoniter Scala Derivation

Drop-in replacement for `jsoniter-scala-macros` `JsonCodecMaker` — derives `JsonValueCodec`, `JsonCodec`, and `JsonKeyCodec` for case classes, sealed traits, Scala 3 enums, Java enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-jsoniter-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-jsoniter-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    ```

!!! note
    You also need `jsoniter-scala-core` as a runtime dependency:

    ```scala
    libraryDependencies += "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "{{ libraries.jsoniterScala }}"
    ```

## Quick start

??? example "Encoding and decoding with JsonValueCodec"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    case class Person(name: String, age: Int)

    implicit val codec: JsonValueCodec[Person] = KindlingsJsonValueCodec.derived[Person]

    val bytes = writeToArray(Person("Alice", 30))
    println(new String(bytes))
    // expected output:
    // {"name":"Alice","age":30}

    val person = readFromArray[Person]("""{"name":"Bob","age":25}""".getBytes)
    println(FastShowPretty.render(person, RenderConfig.Default))
    // expected output:
    // Person(
    //   name = "Bob",
    //   age = 25
    // )
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `KindlingsJsonValueCodec.derived[A]` | `KindlingsJsonValueCodec[A]` | Sanely-automatic codec (given/implicit, also usable as semi-automatic) |
| `KindlingsJsonValueCodec.writeToString[A](value)` | `String` | Inline encoding |
| `KindlingsJsonValueCodec.readFromString[A](json)` | `Either[JsonReaderException, A]` | Inline decoding |
| `KindlingsJsonCodec.derived[A]` | `KindlingsJsonCodec[A]` | Sanely-automatic combined value + key codec (given/implicit, also usable as semi-automatic) |
| `KindlingsJsonCodec.deriveKeyCodec[A]` | `JsonKeyCodec[A]` | Key codec (for map keys) |

All methods take an implicit/using `JsoniterConfig` parameter (defaults to `JsoniterConfig.default`).

## Configuration

```scala
import hearth.kindlings.jsoniterderivation._

implicit val config: JsoniterConfig = JsoniterConfig.default
  .withSnakeCaseFieldNames
  .withDiscriminator("type")
  .withSkipUnexpectedFields(true)
```

| Builder method | Description |
|---------------|-------------|
| `withSnakeCaseFieldNames` | `fieldName` → `field_name` (jsoniter-scala's `enforce_snake_case`: `HTTPServer` → `http_server`, `field1` → `field_1`) |
| `withKebabCaseFieldNames` | `fieldName` → `field-name` (`enforce-kebab-case`) |
| `withPascalCaseFieldNames` | `fieldName` → `FieldName` (`EnforcePascalCase`) |
| `withScreamingSnakeCaseFieldNames` | `fieldName` → `FIELD_NAME` |
| `withFieldNameMapper(f)` | Custom field name transform. As in jsoniter-scala, `f` may be a partial function (`withFieldNameMapper { case "a" => "b" }`): names it is not defined at stay unchanged |
| `withSnakeCaseAdtLeafClassNames` | ADT subtype simple name → `snake_case` |
| `withKebabCaseAdtLeafClassNames` | ADT subtype simple name → `kebab-case` |
| `withAdtLeafClassNameMapper(f)` | Custom ADT subtype name transform. As in jsoniter-scala, `f` receives the **fully-qualified** name (`pkg.Outer.Leaf`); the default is `JsoniterConfig.simpleClassName`. Not applied to Java enums, `Enumeration`s and `@named` leaves. May be a partial function |
| `withDiscriminator(field)` | ADT discriminator field name |
| `withDiscriminatorFieldName(opt)` | jsoniter-scala's builder: `Some(field)` or `None` (wrapper-style ADTs) |
| `withRequireDiscriminatorFirst(false)` | Accept the discriminator anywhere in the object (default `true`: it must be the first key, `expected key: "type"` otherwise). As in jsoniter-scala, the decoder marks the object start, finds the discriminator and re-reads the object |
| `withSkipUnexpectedFields(skip)` | Skip unexpected JSON fields |
| `withEnumAsStrings` | Encode ADTs whose leaves are all case objects (sealed traits, Scala 3 enums) as JSON strings **also with a discriminator** (without one they are strings anyway, see below). Kindlings-only |
| `withMapAsArray` | Encode maps as arrays of key-value pairs |
| `withStringified` | Encode numbers and booleans (also `BitSet` values) as JSON strings, as jsoniter-scala's `isStringified`: from the root value down to the first case class / ADT (top-level values, collection elements, map values, `Option` contents, tuple elements). It never applies to case class fields: those are stringified only by their own `@stringified` |
| `withDecodingOnly` | Derive only decoder |
| `withEncodingOnly` | Derive only encoder |
| `withCirceLikeObjectEncoding` | Without a discriminator, encode case objects as `{"Leaf":{}}` instead of `"Leaf"` (cannot be combined with `withDiscriminator`) |
| `withTransientDefault` | Skip fields equal to their default value during encoding (arrays are compared by contents) |
| `withTransientEmpty` | Skip empty collections, maps and arrays (also `IArray`) during encoding; empty strings are still written |
| `withTransientNone` | Skip `None` fields during encoding |
| `withRequireCollectionFields` | Fail if a collection/map field without a default value is missing (instead of decoding it as empty) |
| `withRequireDefaultFields` | Fail if field with default is missing (`Option` fields still decode as `None`) |
| `withCheckFieldDuplication` | Fail on duplicate JSON field names |
| `withBigDecimalPrecision(n)` | Precision decoded `BigDecimal`s are rounded to (`HALF_EVEN`, default `34` = decimal128) |
| `withBigDecimalScaleLimit(n)` | Decoded `BigDecimal` scale limit (exclusive, as in jsoniter-scala's reader) |
| `withBigDecimalDigitsLimit(n)` | Decoded `BigDecimal` digits limit (exclusive, as in jsoniter-scala's reader) |
| `withBigIntDigitsLimit(n)` | Decoded `BigInt` digits limit (default `308`, as in jsoniter-scala) |
| `withMapMaxInsertNumber(n)` | Max map entries |
| `withSetMaxInsertNumber(n)` | Max number of elements of decoded sets, checked after every insert (`0` rejects any element; other collections and bit sets are not limited) |
| `withBitSetValueLimit(n)` | Max value allowed in a `BitSet` (DoS protection, default `1024`) |
| `withUseScalaEnumValueId` | Use enum value id for Scala enumerations |
| `withJavaEnumValueNameMapper(f)` | Custom transform for Java enum value names (may be a partial function) |
| `withSkipNestedOptionValues` | For `Option[Option[A]]`: `null` decodes as `Some(None)` (and `Some(None)` is written as `null`), so with `withTransientNone` an absent field (`None`) and `null` (`Some(None)`) round-trip. Without it, as in jsoniter-scala, an `Option` nested in an `Option` is encoded as the ADT `Option` (see below) |
| `withAlwaysEmitDiscriminator` | Codecs of ADT leaf classes / case objects (e.g. `derived[Leaf]`) also write the discriminator configured with `withDiscriminator`; their decoders accept it but do not require it |
| `withInlineOneValueClasses` | Inline classes with one (non-transient) field to their wrapped value, also as map keys; transient parameters get their defaults (also with a config only known at runtime, where one-value-class map keys are always inlined) |
| `withTransientNull` | Skip fields of nullable types (Scala 3 `T \| Null`) holding `null` during encoding, as in jsoniter-scala. No effect on other fields (a `null` `String` field still fails to encode) and on Scala 2 |
| `withScalaTransientSupport` | Treat `scala.transient` like `@transientField` (also with a config only known at runtime: then the codec checks the flag) |
| `withDecodingOnly` / `withEncodingOnly` | The other direction throws `NotImplementedError` ("an implementation is missing"), as in jsoniter-scala |

Name mapper helpers with jsoniter-scala's names and semantics live in `JsoniterConfig`: `simpleClassName`,
`enforceCamelCase`, `EnforcePascalCase`, `enforce_snake_case`, `enforce_snake_case2`, `` `enforce-kebab-case` ``,
`` `enforce-kebab-case2` `` and `screamingSnakeCase`. Partial-function mappers are supported by the `with*Mapper`
builders (not by the `JsoniterConfig(...)` constructor), also as literals written inside the config argument of
`derived(...)`.

Presets equivalent to jsoniter-scala's `JsonCodecMaker.makeXxx[A]` entry points (upstream's defaults, see
`jsoniterScalaDefaults`, plus each entry point's settings): `JsoniterConfig.makeWithoutDiscriminator`,
`makeWithRequiredCollectionFields`, `makeWithRequiredCollectionFieldsAndNameAsDiscriminatorFieldName`,
`makeWithRequiredDefaultFields`, `makeWithSkipNestedOptionValues`, `makeCirceLike`, `makeCirceLikeSnakeCased`,
`makeOpenapiLike`, `makeOpenapiLike(discriminatorFieldName)`, `makeOpenapiLike(discriminatorFieldName, mapper)` (the
mapper receives simple class names) and `makeOpenapiLikeWithoutDiscriminator`:
`KindlingsJsonValueCodec.derived[A](JsoniterConfig.makeCirceLike)` is upstream's `JsonCodecMaker.makeCirceLike[A]`.

jsoniter-scala's checks of contradictory settings - `decodingOnly` with `encodingOnly`, `requireCollectionFields` with
`transientEmpty`, `requireDefaultFields` with `transientDefault`, `circeLikeObjectEncoding` with a discriminator,
`alwaysEmitDiscriminator` without one - are compile-time errors with a config known at compile time, and with a config
only known at runtime the codec throws `IllegalArgumentException` with the same message when it is created (or, for
`writeToString`/`readFromString`, when it is used). Duplicated JSON keys of a case class (also with the discriminator,
when the codec writes it: through the sealed parent or with `withAlwaysEmitDiscriminator`) and duplicated discriminator
values are compile-time errors with a config known at compile time.

## ADTs, enums and tuples

As in jsoniter-scala:

| Value | Without a discriminator (default) | With `withDiscriminator("type")` |
|-------|-----------------------------------|----------------------------------|
| case class leaf | `{"Leaf":{"a":1}}` | `{"type":"Leaf","a":1}` |
| case object / parameterless Scala 3 enum case | `"Leaf"` (`{"Leaf":{}}` with `withCirceLikeObjectEncoding`) | `{"type":"Leaf"}` (`"Leaf"` with `withEnumAsStrings` if all leaves are objects) |
| Java enum value | `"VALUE"` (`javaEnumValueNameMapper`) | `"VALUE"` |
| `scala.Enumeration` value | its `toString` (`Value("A")` is `"A"`), the id with `withUseScalaEnumValueId` | the same |
| tuple `(1, "a")` (also generic tuples and arity > 22 on Scala 3) | `[1,"a"]` | `[1,"a"]` |
| named tuple `(i = 1, s = "a")` | `{"i":1,"s":"a"}` | `{"i":1,"s":"a"}` |

Decoding also accepts `{"Leaf":{}}` for a case object without a discriminator (a Kindlings superset). The same names
are used for enum map keys. `@named("X")` on a leaf class, case object or Scala 3 enum case overrides its name (not
mapped).

As in jsoniter-scala, ADT leaves may also be plain (non-case) classes and objects, and a non-abstract sealed class is
one of its own leaves (`sealed class A; case class B(n: Int) extends A` encodes `new A()` as `{"type":"A"}`). With a
discriminator, a user-provided `JsonValueCodec` of a leaf encodes and decodes the whole leaf object (it has to write
the discriminator itself, if wanted).

An `Option` nested directly in an `Option` (without `withSkipNestedOptionValues`) is encoded as jsoniter-scala encodes
it - as the ADT `Option` with the leaves `Some` (field `value`) and `None` - so `Some(None)` survives a round-trip:
`Some(Some(1))` is `{"type":"Some","value":1}` with a discriminator and `{"Some":{"value":1}}` without one, `Some(None)`
is `{"type":"None"}` / `"None"` (`{"None":{}}` with `withCirceLikeObjectEncoding`). The outer `Option` is `null` /
absent as usual.

Unknown names fail with jsoniter-scala's errors: `illegal discriminator` (also for an unknown string of an ADT of case
objects), `illegal value of discriminator field "type"`, `illegal enum value "X"` (Java enums and `Enumeration`s);
unexpected keys with `unexpected field "x"` (with any config).

Literal types (`"VVV"`, `true`, `'1'`, `2`, `3L`, `4.0f`, `5.0`) are written as their value and checked when read
(`expected value: 2`), also as map keys (`expected key: "2"`). On Scala 2 a case class field of a literal type is not
supported yet (Hearth gap, see `docs/research/hearth-gap-scala2-literal-parameter-types.md`).

## Missing fields

Decoding treats a field that is absent from the JSON object the same way as jsoniter-scala's `JsonCodecMaker.make`,
whatever the `transient*` settings (those only control what is *written*):

| Field | Absent from the JSON object |
|-------|-----------------------------|
| has a default value | the default value (fails with `withRequireDefaultFields`) |
| `Option[A]` (no default) | `None` (never required) |
| collection or map (no default) | an empty one (fails with `withRequireCollectionFields`) |
| anything else | fails with `JsonReaderException`: `missing required field "<json name>"` |

An explicit `null` is not the same as an absent field. As in jsoniter-scala:

| Field | JSON `null` |
|-------|-------------|
| `Option[A]` | `None` (`Some(None)` for `Option[Option[A]]` with `withSkipNestedOptionValues`) |
| collection or map (no default) | an empty one (also with `withRequireCollectionFields`) |
| reference type with a default value (`String`, `BigInt`, collection, case class, `Option` other than `Option[Option[_]]`, ...) | the default value |
| Scala 3 nullable type `T \| Null` | `null` (also when absent: such fields are never required) |
| primitive or value class (`Int`, ...), or reference type without a default | fails with `JsonReaderException` |

As in jsoniter-scala, a codec's `nullValue` is `None` for `Option`s, an empty collection/map/array/bit set, zero for
primitives and their boxes, and `null` otherwise; a top-level JSON `null` decodes as the `default` argument of
`decodeValue` (readers pass `nullValue`) when it is not `null`, so a top-level `null` is an empty collection/map for
collection and map codecs and an error for case classes.

A field with a default value is decoded as jsoniter-scala does, with that default as the `default` argument of its
decoder: an empty JSON array/object (`[]`/`{}`) for a collection or map field with a default value is that default
(`"l":[]` decodes `l: List[Int] = List(0)` as `List(0)`, also with `withRequireDefaultFields`, which only rejects absent
fields), and a user-provided `JsonValueCodec` of a field receives the field's default value (its `nullValue` for a field
without one) as `default` and decides itself what a JSON `null` is. Defaults that depend on preceding parameter lists
are not passed (the codec's `nullValue` is). One difference: with `checkFieldDuplication = false`, jsoniter-scala passes
the value read for an earlier duplicate of the key instead of the default.

A case class without fields (or with only `@transientField` fields) skips unknown keys like any other case class (and
rejects them with `withSkipUnexpectedFields(false)`). Case objects skip the content of their JSON object.

## Map keys

Map keys are written as JSON object keys for `String`, the primitives and their boxed versions, `BigInt`, `BigDecimal`,
`UUID`, the `java.time` types (`Instant`, `LocalDate`, `Duration`, ...), value classes and opaque types over those,
enumerations of case objects, and any type with an implicit `JsonKeyCodec`. Other key types need `withMapAsArray`
(`[[key, value], ...]`); with a config known at compile time, using one without `withMapAsArray` is a compile-time error.

## Compile-time and runtime configs

The config is evaluated at compile time when possible, and then the codec has no runtime config checks. That is the
case for `JsoniterConfig.default`, `JsoniterConfig.jsoniterScalaDefaults`, a config passed explicitly
(`KindlingsJsonValueCodec.derived[A](JsoniterConfig.default.withX)` on Scala 2.13, `derived[A](using ...)` on Scala 3)
and, on Scala 3, a local `implicit val`/`given` or a `val` of an `object` (or a `final val`) initialized with such an
expression. Configs read from a `var`, from an overridable (non-final) class/trait `val`, or computed from runtime
values are only known at runtime: the codec then checks the config when it runs, with the same behaviour. So that the
result never depends on the incremental-compilation state, members of classes/objects of the module being compiled are
not evaluated by reflection: on Scala 3 their `val`s are followed to their right-hand side, on Scala 2 such configs are
always treated as runtime configs, whether their classes are compiled in the same run or by an earlier incremental run
(see `docs/research/hearth-gap-semieval-local-vals-and-vars.md`).

## Other notes

- `Unit` is written as `{}` and read from `{}` or `null` (jsoniter-scala has no `Unit` codec).
- As in jsoniter-scala, a class is encoded as the JSON object of the parameters of its primary constructor: case
  classes and non-abstract plain classes (`class C(val id: Int, var name: String)`), with all parameter lists
  (`case class C(i: Int)(val l: Long)` is `{"i":1,"l":2}`). Defaults of later parameter lists may depend on preceding
  parameters (they are computed from the decoded values, and compared with the encoded instance's values for
  `withTransientDefault`).
- Constructor parameters without a public accessor (`class C(a: Int)`, `case class S(private val token: String)`,
  `case class C(i: Int)(l: Long)`) are a compile-time error with jsoniter-scala's message
  (`'a' parameter of 'C' should be defined as 'val' or 'var' in the primary constructor.`): the codec could not write
  what it requires when reading. Make them public `val`s/`var`s or mark them `@transientField` (with a default value).
- With `withTransientEmpty`, `Iterator` fields are checked with `hasNext` (not consumed).
- A user-provided implicit `JsonKeyCodec` takes precedence over the built-in key types, also for `String` keys.

## Annotations

| Annotation | Description |
|-----------|-------------|
| `@fieldName("json_name")` | Override JSON field name (`@named("json_name")` on a field does the same, as in jsoniter-scala) |
| `@transientField` | Exclude field from codec (must have default) |
| `@stringified` | Encode the numbers and booleans of this field as strings (also inside `Option`s, collections, map values and tuples; no effect on strings) |
| `@named("X")` | Override the name of an ADT leaf (case class, case object, Scala 3 enum case); on a field, its JSON name |

## Usage examples

??? example "Sealed trait with discriminator"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val config: JsoniterConfig = JsoniterConfig.default
      .withDiscriminator("type")

    implicit val codec: JsonValueCodec[Shape] = KindlingsJsonValueCodec.derived[Shape]

    println(writeToString[Shape](Circle(5.0)))
    // expected output:
    // {"type":"Circle","radius":5.0}

    val decoded: Shape = readFromString[Shape]("""{"type":"Rectangle","width":3,"height":4}""")
    println(FastShowPretty.render(decoded, RenderConfig.Default))
    // expected output:
    // (Rectangle(
    //     width = 3.0d,
    //     height = 4.0d
    //   )): Shape
    ```

??? example "Transient fields and defaults"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    //> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:{{ libraries.jsoniterScala }}

    import hearth.kindlings.jsoniterderivation._
    import hearth.kindlings.fastshowpretty._
    import com.github.plokhotnyuk.jsoniter_scala.core._

    implicit val config: JsoniterConfig = JsoniterConfig.default
      .withTransientDefault
      .withTransientNone

    case class Settings(
      host: String,
      port: Int = 8080,
      debug: Option[Boolean] = None
    )

    implicit val codec: JsonValueCodec[Settings] = KindlingsJsonValueCodec.derived[Settings]

    // Default and None fields are omitted
    println(writeToString(Settings("localhost")))
    // expected output:
    // {"host":"localhost"}

    // Missing fields use defaults
    println(FastShowPretty.render(readFromString[Settings]("""{"host":"example.com"}"""), RenderConfig.Default))
    // expected output:
    // Settings(
    //   host = "example.com",
    //   port = 8080,
    //   debug = None
    // )
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.jsoniterderivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:jsoniterDerivation.logDerivation=true"
```

## Comparison with jsoniter-scala macros

### Feature differences

| Feature | jsoniter-scala macros | Kindlings |
|---------|----------------------|-----------|
| Same API on Scala 2.13 and 3 | Partial (different compile deps) | Yes |
| Sanely-automatic derivation | No | Yes |
| Inline encoding/decoding | No | Yes (`writeToString`, `readFromString`) |
| Recursive types | Needs workarounds | Just works |
| Named tuples | Yes | Yes |
| Opaque types | Yes | Yes |
| Scala 3 enums | Yes | Yes |
| Java enums | Yes | Yes |
| `make[A]` convenience factory | Yes | `KindlingsJsonValueCodec.derived[A]`; the `makeXxx` variants as `JsoniterConfig.makeXxx` presets |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

!!! note
    Kindlings matches jsoniter-scala's own macros: reads land at 0.96–1.04x and writes at 0.91–1.02x of the hand-tuned reference. The only consistent gap is SimpleADT write (0.91x on 2.13, 0.92x on 3).

#### Write

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 61.1M | 59.3M | 60.8M | **~tied** |
| SimpleCC | 3 | 63.6M | 63.9M | 63.8M | **~tied** |
| Person | 2.13 | 4.8M | 4.7M | 4.7M | **~tied** |
| Person | 3 | 5.5M | 5.4M | 5.4M | **~tied** |
| Event | 2.13 | 4.5M | 4.5M | 4.3M | **~tied** |
| Event | 3 | 4.8M | 4.8M | 4.9M | **~tied** |

#### Read

| Type | Scala | Kindlings semi | Kindlings auto | Original semi | vs original |
|------|-------|---------------|---------------|--------------|------------|
| SimpleCC | 2.13 | 36.4M | 35.8M | 35.5M | **~tied** |
| SimpleCC | 3 | 36.4M | 36.4M | 35.1M | **~tied** |
| Person | 2.13 | 3.6M | 3.6M | 3.8M | 0.96x |
| Person | 3 | 3.6M | 3.7M | 3.8M | 0.98x |
