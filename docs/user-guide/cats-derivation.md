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
    // Person(name = Alice, age = 30)

    println(Person("Alice", 30) === Person("Alice", 30))
    // true

    val ord: Order[Person] = Order.derived[Person]
    println(ord.compare(Person("Alice", 30), Person("Bob", 25)))
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
    // Circle(radius = 5.0)

    println(eqShape.eqv(Circle(3.0), Circle(3.0)))
    // true

    println(eqShape.eqv(Circle(3.0), Rectangle(3.0, 4.0)))
    // false
    ```

??? example "Functor derivation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import cats._
    import cats.syntax.all._

    case class Box[A](value: A)

    implicit val functorBox: Functor[Box] = Functor.derived[Box]

    val box: Box[Int] = Box(42)
    println(Functor[Box].map(box)(_ * 2))
    // Box(84)
    ```

??? example "Semigroup and Monoid derivation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-cats-derivation:{{ kindlings_version() }}

    import hearth.kindlings.catsderivation._
    import hearth.kindlings.catsderivation.extensions._
    import cats.kernel.{Monoid, Semigroup}
    import cats.syntax.all._

    case class Stats(count: Int, total: Double)

    implicit val monoidStats: Monoid[Stats] = Monoid.derived[Stats]

    val a = Stats(10, 100.0)
    val b = Stats(5, 50.0)
    println(a |+| b)
    // Stats(15,150.0)

    println(Monoid[Stats].empty)
    // Stats(0,0.0)
    ```

## Comparison with kittens

### Feature differences

| Feature | kittens (Scala 2) | kittens (Scala 3) | Kindlings |
|---------|-------------------|-------------------|-----------|
| Show, ShowPretty | Yes | Yes | Yes |
| Eq, Order, PartialOrder, Hash | Yes | Yes | Yes |
| Semigroup, Monoid | Yes | Yes | Yes |
| CommutativeSemigroup, CommutativeMonoid | Yes | Yes | Yes |
| Band, Semilattice, BoundedSemilattice | Yes | Yes | Yes |
| Group, CommutativeGroup | Yes | Yes | Yes |
| Empty | Yes | Yes | Yes |
| Functor, Contravariant, Invariant | Yes | Yes | Yes |
| Apply, Applicative | No | Yes | Yes |
| Foldable, Traverse | No | Yes | Yes |
| Reducible, NonEmptyTraverse | No | Yes | Yes |
| SemigroupK, MonoidK | No | Yes | Yes |
| Pure, EmptyK | No | Yes | Yes |
| Bifunctor, Bifoldable, Bitraverse | No | Yes | Yes |
| NonEmptyAlternative, Alternative | No | Yes | Yes |
| ConsK | Yes | No | Yes |
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM GraalVM CE 25, 2 forks / 5 warmup / 10 measurement iterations.

#### Show

| Type | Scala | Kindlings | kittens semi | kittens auto | vs best kittens |
|------|-------|-----------|-------------|-------------|-----------------|
| SimpleCC | 2.13 | 37.0M | 7.2M | 7.2M | **5.1x faster** |
| SimpleCC | 3 | 25.7M | 18.6M | 19.2M | **1.3x faster** |
| SimpleADT | 2.13 | 81.6M | 16.1M | 9.0M | **5.1x faster** |
| SimpleADT | 3 | 77.5M | 48.8M | 50.9M | **1.5x faster** |
| Person | 2.13 | 1.9M | — | 784K | **2.4x faster** |
| Person | 3 | 1.6M | — | 1.3M | **1.2x faster** |
| Event | 2.13 | 1.8M | 642K | 547K | **2.8x faster** |
| Event | 3 | 1.5M | 491K | 1.2M | **1.2x faster** |

#### Eq

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC (eq) | 2.13 | 97.0M | 43.4M | **2.2x faster** |
| SimpleCC (eq) | 3 | 100.0M | 95.3M | **~tied** |

#### Hash

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 793M | 26.5M | **30x faster** |
| SimpleCC | 3 | 812M | 96.6M | **8.4x faster** |

#### Order

| Type | Scala | Kindlings | kittens best | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCC | 2.13 | 418M | 384M | **1.1x faster** |
| SimpleCC | 3 | 386M | 340M | **1.1x faster** |

#### Semigroup

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair | 2.13 | 189M | 50.2M | **3.8x faster** |
| IntPair | 3 | 192M | 115M | **1.7x faster** |

#### Monoid

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| IntPair (combine) | 2.13 | 188M | 47.4M | **4.0x faster** |
| IntPair (combine) | 3 | 188M | 122M | **1.5x faster** |
| IntPair (empty) | 2.13 | 3630M | 1635M | **2.2x faster** |
| IntPair (empty) | 3 | 3645M | 972M | **3.8x faster** |

#### Functor

| Type | Scala | Kindlings | kittens semi | vs kittens |
|------|-------|-----------|-------------|-----------|
| SimpleCCBox (map) | 2.13 | 267M | 5.8M | **46x faster** |
| SimpleCCBox (map) | 3 | 266M | 63.0M | **4.2x faster** |

#### Foldable / Traverse (Scala 3 only — kittens Scala 2 doesn't support these)

| Type class | Kindlings | kittens semi | vs kittens |
|-----------|-----------|-------------|-----------|
| Foldable (foldLeft) | 1535M | 107M | **14x faster** |
| Traverse (traverse) | 68.2M | 18.1M | **3.8x faster** |

#### Show vs ShowPretty vs FastShowPretty

Pretty printing introduces indentation and multi-line output. The table below compares all approaches (Scala 3):

| Approach | SimpleCC | Person | Notes |
|----------|----------|--------|-------|
| Kindlings Show | 26.3M | 1.6M | Plain single-line output |
| Kindlings ShowPretty | 33.3M | 1.7M | Multi-line indented, ~0% overhead vs Show |
| kittens ShowPretty | 5.1M | 521K | List[String] line accumulation |
| Kindlings FastShowPretty | 16.8M | 1.3M | StringBuilder + escaped strings |

Kindlings ShowPretty is **6.6x faster** than kittens ShowPretty for SimpleCC and **3.3x** for Person. The overhead vs plain Show is negligible because indentation is handled at runtime by a simple `indentSubsequentLines` helper on the result string.

FastShowPretty uses `StringBuilder` instead of string concatenation and produces a richer format (quoted strings with escape handling, type suffixes on numeric literals). The StringBuilder allocation + `.toString()` copy adds fixed overhead that dominates for small types but amortizes for larger ones (Person: only 1.3x behind ShowPretty).
