# Refined Integration

Automatic support for [Refined](https://github.com/fthomas/refined) types in all Kindlings derivation modules. Add the dependency and `Refined[A, P]` fields are handled transparently in Circe, Jsoniter, Avro, Cats, and every other module — no imports, no configuration.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-refined-integration" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-refined-integration" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-refined-integration:{{ kindlings_version() }}
    ```

!!! note
    You also need `refined` as a dependency:

    ```scala
    libraryDependencies += "eu.timepit" %%% "refined" % "{{ libraries.refined }}"
    ```

## How it works

Kindlings' macro extension system automatically discovers the refined integration at compile time via SPI (Service Provider Interface). When the `kindlings-refined-integration` jar is on the classpath, an `IsValueType` provider for `Refined[A, P]` is registered, teaching all derivation modules how to unwrap and validate refined types.

- **Encoding**: `Refined[A, P]` is unwrapped to `A` and encoded using `A`'s encoder.
- **Decoding**: The raw `A` value is decoded first, then validated with `refineV[P]`. If validation fails, decoding fails with an error message from the predicate.

No wildcard import, no implicit, no configuration object — the jar's presence is sufficient.

## Platform behavior

| Platform | Scala 2.13 | Scala 3 |
|----------|-----------|---------|
| Representation | `AnyVal` wrapper | Opaque type |
| Provider | `IsValueTypeProviderForRefined` | Hearth's built-in opaque type provider |
| Validation on decode | `refineV[P]` | `refineV[P]` |

On Scala 3, `eu.timepit.refined.api.Refined` is an opaque type alias, so Hearth's built-in `IsValueTypeProviderForOpaque` matches it before the custom provider. The custom provider is primarily needed for Scala 2.13, where `Refined` is an `AnyVal` that requires validated wrapping via `refineV`.

The end result is identical on both Scala versions: encoding strips the refinement, decoding validates it.

## Supported types

Any `Refined[A, P]` where:

- `A` is a type that the derivation module already knows how to handle (e.g. `Int`, `String`, `Double`)
- `P` is a refinement predicate with a `Validate[A, P]` instance in scope (provided by the `refined` library)

This includes all standard refined predicates: `Positive`, `NonEmpty`, `Size[...]`, `MatchesRegex[...]`, `Interval.Closed[...]`, and user-defined predicates.

## Example

??? example "Case class with refined fields (Circe)"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-refined-integration:{{ kindlings_version() }}
    //> using dep eu.timepit::refined:{{ libraries.refined }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import io.circe._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.numeric.Positive
    import eu.timepit.refined.auto._

    case class Order(
      quantity: Int Refined Positive,
      item: String
    )

    val order = Order(3, "widget")
    println(KindlingsEncoder.encode(order).noSpaces)
    // expected output:
    // {"quantity":3,"item":"widget"}

    val decoded = io.circe.parser.parse("""{"quantity":0,"item":"widget"}""")
      .flatMap(KindlingsDecoder.decode[Order](_))
    println(decoded.isLeft)
    // expected output:
    // true
    ```

??? example "Works with any derivation module"

    The same refined types work transparently with Jsoniter, Avro, YAML, and every other module. No per-module setup is needed:

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-refined-integration:{{ kindlings_version() }}
    //> using dep eu.timepit::refined:{{ libraries.refined }}

    import hearth.kindlings.jsoniterderivation._
    import com.github.plokhotnyuk.jsoniter_scala.core._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.collection.NonEmpty
    import eu.timepit.refined.auto._

    case class User(
      name: String Refined NonEmpty,
      age: Int
    )

    implicit val codec = KindlingsJsonValueCodec.derived[User]
    val json = writeToString(User("Alice", 30))
    println(json)
    // expected output:
    // {"name":"Alice","age":30}
    ```
