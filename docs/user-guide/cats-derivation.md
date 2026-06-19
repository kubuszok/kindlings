# Cats Derivation

Drop-in replacement for [kittens](https://github.com/typelevel/kittens) — derives 35 type classes from Cats and Alleycats for case classes, sealed traits, Scala 3 enums, and more.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-cats-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-cats-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Deriving Show and Eq"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import cats._
    import cats.syntax.all._

    case class Person(name: String, age: Int)

    implicit val showPerson: Show[Person] = Show.derived[Person]
    implicit val eqPerson: Eq[Person] = Eq.derived[Person]

    println(Person("Alice", 30).show)
    // expected output:
    // Person(name = Alice, age = 30)

    println(Person("Alice", 30) === Person("Alice", 30))
    // expected output:
    // true

    val ord: Order[Person] = Order.derived[Person]
    println(ord.compare(Person("Alice", 30), Person("Bob", 25)) < 0)
    // expected output:
    // true
    ```

## Supported type classes

### Monomorphic (kind `*`)

Derived for case classes and sealed traits.

| Type class | Package |
|-----------|---------|
| `Show` | `cats` |
| `ShowPretty` | `hearth.kindlings.catsderivation` |
| `Eq` | `cats.kernel` |
| `Order` | `cats.kernel` |
| `PartialOrder` | `cats.kernel` |
| `Hash` | `cats.kernel` |
| `Semigroup` | `cats.kernel` |
| `Monoid` | `cats.kernel` |
| `CommutativeSemigroup` | `cats.kernel` |
| `CommutativeMonoid` | `cats.kernel` |
| `Band` | `cats.kernel` |
| `Semilattice` | `cats.kernel` |
| `BoundedSemilattice` | `cats.kernel` |
| `Group` | `cats.kernel` |
| `CommutativeGroup` | `cats.kernel` |
| `Empty` | `alleycats` |

### Polymorphic (kind `* → *`)

Derived for type constructors (case classes with one type parameter).

| Type class | Package |
|-----------|---------|
| `Functor` | `cats` |
| `Contravariant` | `cats` |
| `Invariant` | `cats` |
| `Apply` | `cats` |
| `Applicative` | `cats` |
| `Foldable` | `cats` |
| `Traverse` | `cats` |
| `Reducible` | `cats` |
| `NonEmptyTraverse` | `cats` |
| `SemigroupK` | `cats` |
| `MonoidK` | `cats` |
| `NonEmptyAlternative` | `cats` |
| `Alternative` | `cats` |
| `Pure` | `alleycats` |
| `EmptyK` | `alleycats` |
| `ConsK` | `alleycats` |

### Bi-variant (kind `(*, *) → *`)

Derived for type constructors with two type parameters (case classes only).

| Type class | Package |
|-----------|---------|
| `Bifunctor` | `cats` |
| `Bifoldable` | `cats` |
| `Bitraverse` | `cats` |

## API

Cats derivation works via extension methods on the Cats companion objects themselves. The derivation is triggered by calling `.derived` on the type class companion:

```scala
// Semi-automatic — call .derived explicitly
val showPerson: Show[Person] = Show.derived[Person]
val eqPerson: Eq[Person] = Eq.derived[Person]
val functorBox: Functor[Box] = Functor.derived[Box]

// Sanely-automatic — just use the type class
import hearth.kindlings.catsderivation._
Person("Alice", 30).show  // Show.derived[Person] resolved automatically
```

## Usage examples

??? example "Sealed trait derivation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import cats._
    import cats.syntax.all._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val showShape: Show[Shape] = Show.derived[Shape]
    implicit val eqShape: Eq[Shape] = Eq.derived[Shape]

    println(showShape.show(Circle(5.0)))
    // expected output:
    // Circle(radius = 5.0)

    println(eqShape.eqv(Circle(3.0), Circle(3.0)))
    // expected output:
    // true

    println(eqShape.eqv(Circle(3.0), Rectangle(3.0, 4.0)))
    // expected output:
    // false
    ```

??? example "Functor derivation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import hearth.kindlings.fastshowpretty._
    import cats._
    import cats.syntax.all._

    case class Box[A](value: A)

    implicit val functorBox: Functor[Box] = Functor.derived[Box]

    val box: Box[Int] = Box(42)
    println(FastShowPretty.render(Functor[Box].map(box)(_ * 2), RenderConfig.Default))
    // expected output:
    // Box(
    //   value = 84
    // )
    ```

??? example "Semigroup and Monoid derivation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}
    //> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import hearth.kindlings.fastshowpretty._
    import cats.kernel.{Monoid, Semigroup}
    import cats.syntax.all._

    case class Stats(count: Int, total: Double)

    implicit val monoidStats: Monoid[Stats] = Monoid.derived[Stats]

    val a = Stats(10, 100.0)
    val b = Stats(5, 50.0)
    println(FastShowPretty.render(a |+| b, RenderConfig.Default))
    // expected output:
    // Stats(
    //   count = 15,
    //   total = 150.0d
    // )

    println(FastShowPretty.render(Monoid[Stats].empty, RenderConfig.Default))
    // expected output:
    // Stats(
    //   count = 0,
    //   total = 0.0d
    // )
    ```

## Annotations

The `Show` and `ShowPretty` type classes support the `@sensitiveData` annotation to redact sensitive values from rendered output.

| Annotation                 | Target        | Description                                           |
|----------------------------|---------------|-------------------------------------------------------|
| `@sensitiveData`           | Field or type | Replaces the rendered value with `[redacted]`         |
| `@sensitiveData("reason")` | Field or type | Replaces the rendered value with `[redacted: reason]` |

Import annotations from `hearth.kindlings.catsderivation.annotations`.

??? example "Redacting sensitive fields in Show"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import hearth.kindlings.catsderivation.annotations.sensitiveData
    import cats._
    import cats.syntax.all._

    case class User(name: String, @sensitiveData password: String, @sensitiveData("PII") email: String)

    implicit val showUser: Show[User] = Show.derived[User]

    println(User("Alice", "s3cret", "alice@example.com").show)
    // expected output:
    // User(name = Alice, password = [redacted], email = [redacted: PII])
    ```

??? example "Redacting an entire type in ShowPretty"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import hearth.kindlings.catsderivation.annotations.sensitiveData

    @sensitiveData("classified") case class SecretData(code: String, key: Int)

    implicit val showPrettySecret: ShowPretty[SecretData] = ShowPretty.derived[SecretData]

    println(showPrettySecret.show(SecretData("alpha", 42)))
    // expected output:
    // [redacted: classified]
    ```

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.catsderivation.debug._
```

This enables `LogDerivation` implicits for all derived type classes, printing the derivation steps to the compiler output.

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:catsDerivation.logDerivation=true"
```

## Comparison with kittens

### Feature differences

| Feature | kittens (Scala 2) | kittens (Scala 3) | Kindlings |
|---------|-------------------|-------------------|-----------|
| Show, ShowPretty | Yes | Yes | Yes |
| Eq, Order, PartialOrder, Hash | Yes | Yes | Yes |
| Semigroup, Monoid | Yes | Yes | Yes |
| CommutativeSemigroup, CommutativeMonoid | Yes | Yes | Yes |
| Band, Semilattice, BoundedSemilattice | No | Yes | Yes |
| Group, CommutativeGroup | No | Yes | Yes |
| Empty | Yes | Yes | Yes |
| Functor, Contravariant, Invariant | Yes | Yes | Yes |
| Apply, Applicative | Yes | Yes | Yes |
| Foldable, Traverse | Yes | Yes | Yes |
| Reducible, NonEmptyTraverse | Yes | Yes | Yes |
| SemigroupK, MonoidK | Yes | Yes | Yes |
| Pure, EmptyK | Yes | Yes | Yes |
| Bifunctor, Bifoldable, Bitraverse | No | Yes | Yes |
| NonEmptyAlternative, Alternative | No | Yes | Yes |
| ConsK | Yes | No | Yes |
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17, 2 forks / 5 warmup / 10 measurement iterations.

#### Show

| Type | Scala | Kindlings | kittens semi | kittens auto | vs best kittens |
|------|-------|-----------|-------------|-------------|-----------------|
| SimpleCC | 2.13 | 38.2M | 7.5M | 7.0M | **5.1x faster** |
| SimpleCC | 3 | 27.2M | 19.6M | 19.9M | **1.4x faster** |
| SimpleADT | 2.13 | 86.0M | 16.2M | 10.4M | **5.3x faster** |
| SimpleADT | 3 | 72.8M | 51.9M | 52.8M | **1.4x faster** |
| Person | 2.13 | 2.0M | — | 804K | **2.5x faster** |
| Person | 3 | 1.6M | — | 1.4M | **1.1x faster** |
| Event | 2.13 | 1.9M | 602K | 620K | **3.1x faster** |
| Event | 3 | 1.5M | 535K | 1.2M | **1.3x faster** |

#### Eq

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC (eq) | 2.13 | 100.2M | 46.1M | **2.2x faster** |
| SimpleCC (eq) | 3 | 102.3M | 92.2M | **1.1x faster** |

#### Hash

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 824.9M | 27.4M | **30.1x faster** |
| SimpleCC | 3 | 828.3M | 110.0M | **7.5x faster** |

#### Order

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 424.8M | 389.7M | **1.1x faster** |
| SimpleCC | 3 | 429.7M | 347.6M | **1.2x faster** |

#### Semigroup

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair | 2.13 | 194.1M | 54.3M | **3.6x faster** |
| IntPair | 3 | 193.6M | 146.3M | **1.3x faster** |

#### Monoid

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair (combine) | 2.13 | 192.8M | 49.0M | **3.9x faster** |
| IntPair (combine) | 3 | 193.9M | 119.5M | **1.6x faster** |
| IntPair (empty) | 2.13 | 3.6B | 1.7B | **2.1x faster** |
| IntPair (empty) | 3 | 3.7B | 1.0B | **3.7x faster** |

#### Functor

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCCBox (map) | 2.13 | 277.3M | 5.7M | **49x faster** |
| SimpleCCBox (map) | 3 | 275.9M | 65.2M | **4.2x faster** |

#### Foldable / Traverse (Scala 3 benchmark only)

| Type class | Kindlings | kittens semi | vs kittens |
|-----------|-----------|-------------|-----------|
| Foldable (foldLeft) | 1.6B | 109.6M | **14.6x faster** |
| Traverse (traverse) | 164.2M | 18.7M | **8.8x faster** |

#### Show vs ShowPretty vs FastShowPretty

Pretty printing introduces indentation and multi-line output. The table below compares all approaches (Scala 3):

| Approach | SimpleCC | Person | Notes |
|----------|----------|--------|-------|
| Kindlings Show | 27.0M | 1.7M | Plain single-line output |
| Kindlings ShowPretty | 34.3M | 1.8M | Multi-line indented, ~0% overhead vs Show |
| kittens ShowPretty | 5.4M | 557.0K | List[String] line accumulation |
| Kindlings FastShowPretty | 18.0M | 1.3M | StringBuilder + escaped strings |

Kindlings ShowPretty is **6.4x faster** than kittens ShowPretty for SimpleCC and **3.2x** for Person. The overhead vs plain Show is negligible because indentation is handled at runtime by a simple `indentSubsequentLines` helper on the result string.

FastShowPretty uses `StringBuilder` instead of string concatenation and produces a richer format (quoted strings with escape handling, type suffixes on numeric literals). The StringBuilder allocation + `.toString()` copy adds fixed overhead that dominates for small types but amortizes for larger ones (Person: only 1.4x behind ShowPretty).
