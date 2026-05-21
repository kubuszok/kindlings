# Iron Integration

Automatic support for [Iron](https://github.com/Iltotore/iron) constrained types in all Kindlings derivation modules. Add the dependency and `A :| C` fields are handled transparently in Circe, Jsoniter, Avro, Cats, and every other module — no imports, no configuration.

**Scala 3 only** — Iron is a Scala 3-only library.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-iron-integration" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-iron-integration" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-iron-integration:{{ kindlings_version() }}
    ```

!!! note
    You also need `iron` as a dependency:

    ```scala
    libraryDependencies += "io.github.iltotore" %%% "iron" % "{{ libraries.iron }}"
    ```

## How it works

Iron types (`IronType[A, C]`, written as `A :| C`) are opaque types with an `<: A` upper bound. The integration registers an `IsValueType` provider that teaches all derivation modules how to unwrap and validate Iron-constrained types.

- **Encoding**: `A :| C` is unwrapped to `A` and encoded using `A`'s encoder.
- **Decoding**: The raw `A` value is decoded first, then validated using `RuntimeConstraint[A, C]`. If validation fails, decoding fails with an error.

`RuntimeConstraint` is Iron's non-inline validation mechanism, which makes it compatible with macro-generated code (where inline refinement methods cannot be called).

Like all Kindlings integration modules, the jar's presence on the classpath is sufficient — the macro extension system discovers the provider at compile time via SPI.

## Supported types

Any `A :| C` (i.e. `IronType[A, C]`) where:

- `A` is a type that the derivation module already knows how to handle (e.g. `Int`, `String`, `Double`)
- `C` is a constraint with a `RuntimeConstraint[A, C]` instance in scope (provided by the `iron` library for all standard constraints)

This includes all standard Iron constraints: `Positive`, `StrictlyNegative`, `Not[Empty]`, `MinLength[N]`, `Match[R]`, and user-defined constraints.

## Example

??? example "Case class with Iron-constrained fields (Circe)"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-iron-integration:{{ kindlings_version() }}
    //> using dep io.github.iltotore::iron:{{ libraries.iron }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._
    import io.github.iltotore.iron._
    import io.github.iltotore.iron.constraint.all._

    case class Product(
      name: String :| Not[Empty],
      price: Double :| Positive,
      quantity: Int :| Positive
    )

    val product = Product("widget".refine, 9.99.refine, 5.refine)
    println(KindlingsEncoder.encode(product).noSpaces)
    // expected output:
    // {"name":"widget","price":9.99,"quantity":5}

    val decoded = io.circe.parser.parse("""{"name":"","price":9.99,"quantity":5}""")
      .flatMap(KindlingsDecoder.decode[Product](_))
    println(decoded.isLeft)
    // expected output:
    // true
    ```

??? example "Works with any derivation module"

    The same Iron types work transparently with Jsoniter, Avro, YAML, and every other module:

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-iron-integration:{{ kindlings_version() }}
    //> using dep io.github.iltotore::iron:{{ libraries.iron }}

    import hearth.kindlings.jsoniterderivation._
    import com.github.plokhotnyuk.jsoniter_scala.core._
    import io.github.iltotore.iron._
    import io.github.iltotore.iron.constraint.all._

    case class Measurement(
      value: Double :| Positive,
      unit: String
    )

    val codec = KindlingsJsonValueCodec.derive[Measurement]
    val json = writeToString(Measurement(42.0.refine, "kg"))(codec)
    println(json)
    // expected output:
    // {"value":42.0,"unit":"kg"}
    ```

## Comparison with Refined

Both Refined and Iron provide compile-time-validated wrapper types. The key differences for Kindlings users:

| | Refined | Iron |
|---|---------|------|
| Scala versions | 2.13 and 3 | 3 only |
| Kindlings module | `kindlings-refined-integration` | `kindlings-iron-integration` |
| Validation mechanism | `refineV[P]` | `RuntimeConstraint[A, C]` |
| Representation (Scala 3) | Opaque type | Opaque type |

Both integrations work identically from the user's perspective: add the dependency, and constrained types are handled automatically in all derivation modules.
