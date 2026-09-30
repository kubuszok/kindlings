# scala-newtype Integration

Automatic support for [scala-newtype](https://github.com/estatico/scala-newtype) `@newtype`s and `@newsubtype`s in all Kindlings derivation modules. Add the dependency and newtype fields are handled transparently in Circe, Jsoniter, Avro, Cats, and every other module — no imports, no configuration.

Works on Scala 2.13 (with the original `@newtype` macro annotation) and on Scala 3 (with the [scala-newtype-compat](https://github.com/kubuszok/scala-newtype-compat) compiler plugin, which performs the same expansion).

**JVM only** — scala-newtype-compat is published for the JVM only.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-newtype-integration" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-newtype-integration:{{ kindlings_version() }}
    ```

!!! note
    The integration brings `com.kubuszok::newtype-compat` (and with it `io.estatico:newtype`) onto the classpath. `@newtype` itself still has to be expanded by your build:

    ```scala
    // Scala 2.13: enable macro annotations
    scalacOptions ++= {
      if (scalaBinaryVersion.value == "2.13") Seq("-Ymacro-annotations")
      else Nil
    }

    // Scala 3: scala-newtype-compat's compiler plugin rewrites @newtype annotations
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "3")
        Seq(compilerPlugin("com.kubuszok" %% "newtype-plugin" % "{{ libraries.newtypeCompat }}" cross CrossVersion.full))
      else Nil
    }
    ```

## How it works

`@newtype case class UserId(value: Int)` expands into a type alias and a companion:

```scala
type UserId = UserId.Type
object UserId {
  type Repr = Int
  type Base = Any { type UserId$newtype }
  trait Tag extends Any
  type Type <: Base with Tag
  // apply, Coercible instances, ...
}
```

`UserId.Type` is an abstract type member — neither an `AnyVal` nor an opaque type — so none of the built-in value-type support can see it. The integration registers an `IsValueType` provider that recognizes this shape (`Type` next to `Repr`, `Base` and `Tag` in the companion) and teaches all derivation modules to use `Repr`:

- **Encoding**: `UserId` is unwrapped to `Int` and encoded using `Int`'s encoder.
- **Decoding**: The raw `Int` is decoded and wrapped. Newtypes carry no validation, so wrapping never fails — both directions are the same zero-cost casts that scala-newtype's `Coercible` performs.

Like all Kindlings integration modules, the jar's presence on the classpath is sufficient — the macro extension system discovers the provider at compile time via SPI.

## Supported types

- `@newtype case class Foo(value: A)` and `@newsubtype case class Foo(value: A)`
- type-parameterized newtypes, e.g. `@newtype case class Tags[A](values: List[A])` — `Tags[String]` is handled as `List[String]`

where the wrapped type is one that the derivation module already knows how to handle.

## Example

??? example "Case class with newtype fields (Circe)"

    ```scala
    //> using dep com.kubuszok::kindlings-circe-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-newtype-integration:{{ kindlings_version() }}
    //> using options -Ymacro-annotations

    import hearth.kindlings.circederivation._
    import io.estatico.newtype.macros.newtype

    object types {
      @newtype case class UserId(value: Int)
      @newtype case class Tags[A](values: List[A])
    }
    import types._

    case class User(id: UserId, tags: Tags[String])

    println(KindlingsEncoder.encode(User(UserId(1), Tags(List("admin")))).noSpaces)
    // expected output:
    // {"id":1,"tags":["admin"]}
    ```

## Comparison with neotype

See [Neotype Integration](neotype-integration.md#comparison-with-scala-newtype).
