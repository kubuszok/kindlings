# Cats Collections Integration

Automatic support for [Cats](https://typelevel.org/cats/) data types in all Kindlings derivation modules. Add the dependency and `NonEmptyList`, `NonEmptyMap`, `Chain`, `Validated`, `Const`, and other Cats types are handled transparently in Circe, Jsoniter, Avro, YAML, and every other module — no imports, no configuration.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-cats-integration" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-cats-integration" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}
    ```

!!! note
    You also need `cats-core` as a dependency:

    ```scala
    libraryDependencies += "org.typelevel" %%% "cats-core" % "{{ libraries.cats }}"
    ```

## How it works

The integration registers `IsCollection`, `IsMap`, `IsEither`, and `IsValueType` providers for Cats data types. These providers are discovered at compile time via SPI (Service Provider Interface) — the macro extension system picks them up automatically when the jar is on the classpath.

Each provider teaches all derivation modules how to encode and decode the corresponding Cats type:

- **Collections** (`NonEmptyList`, `NonEmptyVector`, `NonEmptyChain`, `Chain`): encoded as JSON arrays, decoded with non-emptiness validation where applicable.
- **Maps** (`NonEmptyMap`): encoded as JSON objects (like `Map`), decoded with non-emptiness validation.
- **Sets** (`NonEmptySet`): encoded as JSON arrays, decoded with non-emptiness and ordering validation.
- **Either-like** (`Validated`): encoded/decoded like `Either`, mapping `Invalid` to left and `Valid` to right.
- **Value types** (`Const`): unwrapped to the underlying value.

## Supported types

### Collection types

| Type | Encoded as | Decoding validation |
|------|-----------|-------------------|
| `NonEmptyList[A]` | Array | Must be non-empty |
| `NonEmptyVector[A]` | Array | Must be non-empty |
| `NonEmptyChain[A]` | Array | Must be non-empty |
| `Chain[A]` | Array | None |

### Map types

| Type | Encoded as | Decoding validation |
|------|-----------|-------------------|
| `NonEmptyMap[K, V]` | Object / array of pairs | Must be non-empty; requires `Order[K]` |

### Set types

| Type | Encoded as | Decoding validation |
|------|-----------|-------------------|
| `NonEmptySet[A]` | Array | Must be non-empty; requires `Order[A]` |

### Either-like types

| Type | Encoded as | Decoding validation |
|------|-----------|-------------------|
| `Validated[E, A]` | Like `Either[E, A]` | None (both `Invalid` and `Valid` are valid) |
| `ValidatedNel[E, A]` | Like `Either[NonEmptyList[E], A]` | Handled automatically (it is `Validated[NonEmptyList[E], A]`) |

### Value types

| Type | Behavior |
|------|---------|
| `Const[A, B]` | Unwrapped to `A` (phantom `B` is ignored) |

## Examples

??? example "Non-empty collections with Circe"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}
    //> using dep org.typelevel::cats-core:{{ libraries.cats }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation.*
    import io.circe.*
    import io.circe.syntax.*
    import cats.data.NonEmptyList

    case class Team(
      name: String,
      members: NonEmptyList[String]
    )

    val team = Team("Backend", NonEmptyList.of("Alice", "Bob", "Carol"))
    println(team.asJson.noSpaces)
    // {"name":"Backend","members":["Alice","Bob","Carol"]}

    // Decoding an empty array fails — NonEmptyList requires at least one element
    val bad = io.circe.parser.decode[Team]("""{"name":"Empty","members":[]}""")
    println(bad)
    // Left(DecodingFailure(...))

    val good = io.circe.parser.decode[Team]("""{"name":"Solo","members":["Dave"]}""")
    println(good)
    // Right(Team(Solo,NonEmptyList(Dave)))
    ```

??? example "NonEmptyMap with Jsoniter"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::kindlings-jsoniter-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}
    //> using dep org.typelevel::cats-core:{{ libraries.cats }}

    import hearth.kindlings.jsoniterderivation.*
    import com.github.plokhotnyuk.jsoniter_scala.core.*
    import cats.data.NonEmptyMap
    import cats.implicits.*

    case class Config(
      settings: NonEmptyMap[String, String]
    )

    val codec = KindlingsJsonValueCodec.derive[Config]
    val config = Config(NonEmptyMap.of("host" -> "localhost", "port" -> "8080"))
    println(writeToString(config)(codec))
    // {"settings":{"host":"localhost","port":"8080"}}
    ```

??? example "Validated fields"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}
    //> using dep org.typelevel::cats-core:{{ libraries.cats }}
    //> using dep io.circe::circe-parser:{{ libraries.circe }}

    import hearth.kindlings.circederivation.*
    import io.circe.*
    import io.circe.syntax.*
    import cats.data.Validated

    case class FormResult(
      username: Validated[String, String]
    )

    val valid = FormResult(Validated.valid("alice"))
    println(valid.asJson.noSpaces)
    // {"username":{"Right":"alice"}}

    val invalid = FormResult(Validated.invalid("username is required"))
    println(invalid.asJson.noSpaces)
    // {"username":{"Left":"username is required"}}
    ```

## Combining with other integrations

The Cats integration composes naturally with other integration modules. For example, you can have `NonEmptyList[Int Refined Positive]` fields — the Cats integration handles `NonEmptyList` and the Refined integration handles `Refined`:

```scala
libraryDependencies ++= Seq(
  "com.kubuszok" %%% "kindlings-circe-derivation"    % "{{ kindlings_version() }}",
  "com.kubuszok" %%% "kindlings-cats-integration"    % "{{ kindlings_version() }}",
  "com.kubuszok" %%% "kindlings-refined-integration" % "{{ kindlings_version() }}"
)
```

Each integration module independently registers its providers, and the derivation engine composes them automatically.
