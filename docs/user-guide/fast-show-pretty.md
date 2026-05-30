# FastShowPretty

Original module -- derives `FastShowPretty` instances for pretty-printing case classes, sealed traits, Scala 3 enums, and more with configurable indentation. Outputs Scala-literal-style text with named fields, nested indentation, and proper escaping.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-fast-show-pretty" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-fast-show-pretty" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}
    ```

## Quick start

??? example "Pretty-printing a nested case class"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    case class Address(street: String, city: String)
    case class Person(name: String, age: Int, address: Address)

    val person = Person("Alice", 30, Address("123 Main St", "New York"))
    println(FastShowPretty.render(person, RenderConfig.Default))
    // expected output:
    // Person(
    //   name = "Alice",
    //   age = 30,
    //   address = Address(
    //     street = "123 Main St",
    //     city = "New York"
    //   )
    // )
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `FastShowPretty.render[A](value, config)` | `String` | Inline render (no instance allocation) |
| `FastShowPretty.derived[A]` | `FastShowPretty[A]` | Sanely-automatic (given/implicit) |

`FastShowPretty.render` is the primary entry point -- it derives and renders in a single call with no intermediate instance. The `derived` method is available for cases where you need to pass an instance around.

### Type class interface

`FastShowPretty[A]` provides one method:

| Member | Signature | Description |
|--------|-----------|-------------|
| `render` | `(sb: StringBuilder, config: RenderConfig, level: Int)(value: A): StringBuilder` | Render a value into a `StringBuilder` at the given indentation level |

## Configuration

`RenderConfig` controls indentation. Unlike the builder pattern used by other modules, `RenderConfig` is a simple case class with predefined constants:

```scala
import hearth.kindlings.fastshowpretty._

// Use a predefined config
val output = FastShowPretty.render(myValue, RenderConfig.Default)

// Or construct a custom one
val custom = RenderConfig(indentString = "...", startLevel = 0)
val output2 = FastShowPretty.render(myValue, custom)
```

### Predefined configs

| Config | Indent string | Description |
|--------|--------------|-------------|
| `RenderConfig.Default` | 2 spaces | Standard pretty-printing |
| `RenderConfig.Compact` | empty string | No indentation (all fields on separate lines but not indented) |
| `RenderConfig.Tabs` | tab character | Tab-based indentation |
| `RenderConfig.FourSpaces` | 4 spaces | Wider indentation |

### Config fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `indentString` | `String` | `"  "` (2 spaces) | String prepended per indentation level |
| `startLevel` | `Int` | `0` | Initial indentation level |

## Usage examples

??? example "Different indentation styles"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    case class Point(x: Int, y: Int)

    val p = Point(10, 20)

    // 2-space indent (default)
    println(FastShowPretty.render(p, RenderConfig.Default))
    // expected output:
    // Point(
    //   x = 10,
    //   y = 20
    // )

    // Tab indent
    println(FastShowPretty.render(p, RenderConfig.Tabs))
    // expected output:
    // Point(
    // 	x = 10,
    // 	y = 20
    // )

    // 4-space indent
    println(FastShowPretty.render(p, RenderConfig.FourSpaces))
    // expected output:
    // Point(
    //     x = 10,
    //     y = 20
    // )

    // Compact (no indent)
    println(FastShowPretty.render(p, RenderConfig.Compact))
    // expected output:
    // Point(
    // x = 10,
    // y = 20
    // )
    ```

??? example "Collections and maps"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    case class Team(name: String, members: List[String])

    val team = Team("Engineering", List("Alice", "Bob", "Charlie"))
    println(FastShowPretty.render(team, RenderConfig.Default))
    // expected output:
    // Team(
    //   name = "Engineering",
    //   members = List(
    //     "Alice",
    //     "Bob",
    //     "Charlie"
    //   )
    // )
    ```

??? example "Sealed traits"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    val shape: Shape = Circle(5.0)
    println(FastShowPretty.render(shape, RenderConfig.Default))
    // expected output:
    // (Circle(
    //     radius = 5.0d
    //   )): Shape
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    case class Tree(value: Int, children: List[Tree])

    val tree = Tree(1, List(
      Tree(2, Nil),
      Tree(3, List(Tree(4, Nil)))
    ))
    println(FastShowPretty.render(tree, RenderConfig.Default))
    // expected output:
    // Tree(
    //   value = 1,
    //   children = List(
    //     Tree(
    //       value = 2,
    //       children = List()
    //     ),
    //     Tree(
    //       value = 3,
    //       children = List(
    //         Tree(
    //           value = 4,
    //           children = List()
    //         )
    //       )
    //     )
    //   )
    // )
    ```

??? example "Using the derived instance directly"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._

    case class Person(name: String, age: Int)

    // Sanely-automatic: resolved by the compiler via FastShowPretty.derived
    val instance = implicitly[FastShowPretty[Person]]
    val sb = instance.render(new StringBuilder, RenderConfig.Default, 0)(Person("Alice", 30))
    println(sb.toString)
    // expected output:
    // Person(
    //   name = "Alice",
    //   age = 30
    // )
    ```

## Annotations

| Annotation                 | Target        | Description                                           |
|----------------------------|---------------|-------------------------------------------------------|
| `@sensitiveData`           | Field or type | Replaces the rendered value with `[redacted]`         |
| `@sensitiveData("reason")` | Field or type | Replaces the rendered value with `[redacted: reason]` |

Import annotations from `hearth.kindlings.fastshowpretty.annotations`.

??? example "Redacting sensitive fields"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._
    import hearth.kindlings.fastshowpretty.annotations.sensitiveData

    case class User(name: String, @sensitiveData password: String)

    println(FastShowPretty.render(User("Alice", "s3cret"), RenderConfig.Default))
    // expected output:
    // User(
    //   name = "Alice",
    //   password = [redacted]
    // )
    ```

??? example "Redacting with a reason"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._
    import hearth.kindlings.fastshowpretty.annotations.sensitiveData

    case class User(name: String, @sensitiveData("PII") email: String, age: Int)

    println(FastShowPretty.render(User("Alice", "alice@example.com", 30), RenderConfig.Default))
    // expected output:
    // User(
    //   name = "Alice",
    //   email = [redacted: PII],
    //   age = 30
    // )
    ```

??? example "Redacting an entire type"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.fastshowpretty._
    import hearth.kindlings.fastshowpretty.annotations.sensitiveData

    @sensitiveData("financial data") case class CreditCard(number: String, cvv: String)
    case class Checkout(item: String, card: CreditCard)

    println(FastShowPretty.render(CreditCard("4111-1111-1111-1111", "123"), RenderConfig.Default))
    // expected output:
    // [redacted: financial data]

    println(FastShowPretty.render(Checkout("Widget", CreditCard("4111", "123")), RenderConfig.Default))
    // expected output:
    // Checkout(
    //   item = "Widget",
    //   card = [redacted: financial data]
    // )
    ```

## Primitive rendering

FastShowPretty renders primitives with type-disambiguating suffixes, matching Scala literal syntax:

| Type | Example value | Rendered as |
|------|--------------|-------------|
| `Boolean` | `true` | `true` |
| `Byte` | `42` | `42.toByte` |
| `Short` | `42` | `42.toShort` |
| `Int` | `42` | `42` |
| `Long` | `42L` | `42L` |
| `Float` | `3.14f` | `3.14f` |
| `Double` | `3.14` | `3.14d` |
| `Char` | `'a'` | `'a'` |
| `String` | `"hello"` | `"hello"` |

Strings are properly escaped (quotes, newlines, etc.).

## Comparison with kittens Show

FastShowPretty serves a similar purpose to `cats.Show` derived via kittens, but produces indented, multi-line output suitable for debugging complex nested structures. It also renders primitives with type-disambiguating suffixes (e.g., `42L` for `Long`, `3.14f` for `Float`).

For runtime performance comparison with kittens' `Show`, see the [Cats derivation benchmarks](cats-derivation.md#benchmarks). FastShowPretty uses the same macro-generated approach, so performance characteristics are similar to Kindlings' `Show` derivation.

## Debugging

Import the debug package to log the generated code during compilation:

```scala
import hearth.kindlings.fastshowpretty.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:fastShowPretty.logDerivation=true"
```
