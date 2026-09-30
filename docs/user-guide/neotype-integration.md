# Neotype Integration

Automatic support for [neotype](https://github.com/kitlangton/neotype) `Newtype`s and `Subtype`s in all Kindlings derivation modules. Add the dependency and neotype fields are handled transparently in Circe, Jsoniter, Avro, Cats, and every other module — no imports, no configuration.

**Scala 3 only** — neotype is a Scala 3-only library. Available for JVM and Scala.js (neotype is not published for Scala Native).

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-neotype-integration" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-neotype-integration" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-neotype-integration:{{ kindlings_version() }}
    ```

!!! note
    You also need `neotype` as a dependency:

    ```scala
    libraryDependencies += "io.github.kitlangton" %%% "neotype" % "{{ libraries.neotype }}"
    ```

## How it works

A neotype is defined by a companion object extending `Newtype[A]` (or `Subtype[A]`), whose opaque `Type` member is the actual wrapper type:

```scala
type Email = Email.Type
object Email extends Newtype[String] {
  override inline def validate(input: String): Boolean | String = input.contains("@")
}
```

The integration registers an `IsValueType` provider that recognizes any `Foo.Type` declared by `neotype.Newtype` or `neotype.Subtype`, and teaches all derivation modules how to unwrap and validate it:

- **Encoding**: `Email` is unwrapped to `String` and encoded using `String`'s encoder.
- **Decoding**: The raw `String` is decoded first, then passed to `Email.make`, which runs `validate` at runtime. If validation fails, decoding fails with an error (the `String` returned by `validate`, or `"Validation Failed"` when it returned `false`).

neotype's own `Newtype.WithType` witness is a `transparent inline given`, which a derivation macro cannot see through implicit search — the provider matches the type structurally instead, so no per-type instances are needed.

Like all Kindlings integration modules, the jar's presence on the classpath is sufficient — the macro extension system discovers the provider at compile time via SPI.

## Supported types

Any `Foo.Type` where:

- `object Foo extends neotype.Newtype[A]` or `object Foo extends neotype.Subtype[A]`
- `A` is a type that the derivation module already knows how to handle (e.g. `Int`, `String`, a case class, or another neotype)

A neotype wrapping another neotype is unwrapped one level at a time, so the validation of both is preserved.

## Example

??? example "Case class with neotype fields (Circe)"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-neotype-integration:{{ kindlings_version() }}
    //> using dep io.github.kitlangton::neotype:{{ libraries.neotype }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation._
    import neotype.*

    type Email = Email.Type
    object Email extends Newtype[String] {
      override inline def validate(input: String): Boolean | String = input.contains("@")
    }

    type Age = Age.Type
    object Age extends Subtype[Int] {
      override inline def validate(input: Int): Boolean | String = input >= 0
    }

    case class User(email: Email, age: Age)

    println(KindlingsEncoder.encode(User(Email("alice@example.com"), Age(30))).noSpaces)
    // expected output:
    // {"email":"alice@example.com","age":30}

    val decoded = io.circe.parser.parse("""{"email":"alice@example.com","age":-1}""")
      .flatMap(KindlingsDecoder.decode[User](_))
    println(decoded.isLeft)
    // expected output:
    // true
    ```

## Comparison with scala-newtype

| | scala-newtype | neotype |
|---|---------|------|
| Scala versions | 2.13 and 3 (via scala-newtype-compat) | 3 only |
| Kindlings module | `kindlings-newtype-integration` | `kindlings-neotype-integration` |
| Validation on decode | None (zero-cost cast) | `Companion.make` (runs `validate`) |
| Platforms | JVM | JVM, Scala.js |

See [scala-newtype Integration](newtype-integration.md).
