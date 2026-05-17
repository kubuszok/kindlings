# Cats Derivation

Drop-in replacement for [kittens](https://github.com/typelevel/kittens) — derives 26 type classes from Cats and Alleycats for case classes, sealed traits, Scala 3 enums, and more.

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
| `Eq` | `cats.kernel` |
| `Order` | `cats.kernel` |
| `PartialOrder` | `cats.kernel` |
| `Hash` | `cats.kernel` |
| `Semigroup` | `cats.kernel` |
| `Monoid` | `cats.kernel` |
| `CommutativeSemigroup` | `cats.kernel` |
| `CommutativeMonoid` | `cats.kernel` |
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
| Show | Yes | Yes | Yes |
| Eq, Order, Hash | Yes | Yes | Yes |
| Semigroup, Monoid | Yes | Yes | Yes |
| CommutativeSemigroup, CommutativeMonoid | Yes | Yes | Yes |
| Empty | Yes | Yes | Yes |
| Functor | Yes | Yes | Yes |
| Contravariant | No | Yes | Yes |
| Invariant | No | Yes | Yes |
| Foldable, Traverse | No | Yes | Yes |
| Reducible, NonEmptyTraverse | No | Yes | Yes |
| Apply, Applicative | No | Yes | Yes |
| SemigroupK, MonoidK | No | Yes | Yes |
| NonEmptyAlternative, Alternative | No | No | Yes |
| Pure, EmptyK | No | Yes | Yes |
| ConsK | No | No | Yes |
| Same API on Scala 2.13 and 3 | No | No | Yes |
| Sanely-automatic derivation | No | No | Yes |

### Benchmarks

All values in ops/s (higher is better). Measured on macOS, JVM temurin 17.

#### Show

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC | 2.13 | 39.0M | 7.3M | 7.5M | **5.2x faster** |
| SimpleCC | 3 | 26.8M | 19.6M | 19.7M | **1.4x faster** |
| SimpleADT | 2.13 | 87.9M | 16.8M | 11.1M | **5.2x faster** |
| SimpleADT | 3 | 79.8M | 46.1M | 52.5M | **1.5x faster** |
| Person | 2.13 | 2.0M | — | 829.7K | **2.4x faster** |
| Person | 3 | 1.6M | — | 1.4M | **1.1x faster** |
| Event | 2.13 | 1.8M | 643.8K | 576.5K | **2.8x faster** |
| Event | 3 | 1.5M | 1.2M | 1.2M | **1.3x faster** |

#### Eq

| Type | Scala | Kindlings | Original semi | Original auto | vs best original |
|------|-------|-----------|--------------|--------------|-----------------|
| SimpleCC (eq) | 2.13 | 101.4M | 45.9M | 46.0M | **2.2x faster** |
| SimpleCC (eq) | 3 | 102.2M | 91.8M | 112.1M | 0.91x |

#### Hash

| Type | Scala | Kindlings | Original auto | vs original |
|------|-------|-----------|--------------|------------|
| SimpleCC | 2.13 | 836.9M | 27.7M | **30.0x faster** |
| SimpleCC | 3 | 839.1M | 122.5M | **6.9x faster** |
