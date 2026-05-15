# ScalaCheck Derivation

Derives `Arbitrary`, `Cogen`, and `Shrink` instances for case classes, sealed traits, Scala 3 enums, Java enums, and more. Replaces manual instance definitions and scalacheck-shapeless.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-scalacheck-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-scalacheck-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    ```

!!! note
    You also need `scalacheck` as a runtime dependency:

    ```scala
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "{{ libraries.scalacheck }}"
    ```

## Quick start

??? example "Generating random case class instances"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep org.scalacheck::scalacheck:{{ libraries.scalacheck }}

    import hearth.kindlings.scalacheckderivation.*
    import org.scalacheck.Arbitrary

    case class Person(name: String, age: Int)

    // Extension method on Arbitrary companion
    implicit val arbPerson: Arbitrary[Person] = Arbitrary.derived[Person]

    // Generate random samples
    println(Arbitrary.arbitrary[Person].sample)
    // Some(Person(abc,42))

    println(Arbitrary.arbitrary[Person].sample)
    // Some(Person(xyz,-7))
    ```

## API

### Derivation methods

ScalaCheck derivation works via extension methods on the ScalaCheck companion objects and via standalone `Derive*` objects:

| Method | Returns | Description |
|--------|---------|-------------|
| `Arbitrary.derived[A]` | `Arbitrary[A]` | Derive via extension method on companion |
| `DeriveArbitrary.derived[A]` | `Arbitrary[A]` | Derive via standalone object |
| `Cogen.derived[A]` | `Cogen[A]` | Derive via extension method on companion |
| `DeriveCogen.derived[A]` | `Cogen[A]` | Derive via standalone object |
| `Shrink.derived[A]` | `Shrink[A]` | Derive via extension method on companion |
| `DeriveShrink.derived[A]` | `Shrink[A]` | Derive via standalone object |

No configuration class is needed -- derivation is fully automatic based on the type structure.

### Supported types

- **Case classes** -- generates random values for each field
- **Sealed traits / enums** -- randomly selects a subtype
- **Scala 3 enums** -- randomly selects an enum value
- **Java enums** -- randomly selects an enum constant
- **Recursive types** -- handled automatically, no `Lazy` wrappers needed
- **Named tuples** -- supported on Scala 3
- **Opaque types** -- supported when the underlying type has an instance
- **Collections and Option** -- standard library instances are used

## Usage examples

??? example "Sealed trait with property-based testing"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep org.scalacheck::scalacheck:{{ libraries.scalacheck }}

    import hearth.kindlings.scalacheckderivation.*
    import org.scalacheck.{Arbitrary, Prop}

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val arbShape: Arbitrary[Shape] = Arbitrary.derived[Shape]

    val prop = Prop.forAll { (shape: Shape) =>
      shape match {
        case Circle(_)       => true
        case Rectangle(_, _) => true
      }
    }

    prop.check()
    // + OK, passed 100 tests.
    ```

??? example "Cogen for function generation"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep org.scalacheck::scalacheck:{{ libraries.scalacheck }}

    import hearth.kindlings.scalacheckderivation.*
    import org.scalacheck.{Arbitrary, Cogen, Gen}

    case class Point(x: Int, y: Int)

    // Cogen lets ScalaCheck generate functions *from* your type
    implicit val cogenPoint: Cogen[Point] = Cogen.derived[Point]

    // Now ScalaCheck can generate Point => String functions
    val genFn: Gen[Point => String] = Arbitrary.arbitrary[Point => String]
    val fn = genFn.sample.get
    println(fn(Point(1, 2)))
    ```

??? example "Shrink for minimal failing cases"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep org.scalacheck::scalacheck:{{ libraries.scalacheck }}

    import hearth.kindlings.scalacheckderivation.*
    import org.scalacheck.Shrink

    case class Config(retries: Int, timeout: Long, label: String)

    implicit val shrinkConfig: Shrink[Config] = Shrink.derived[Config]

    // Shrink produces smaller variants of a failing input
    val original = Config(100, 5000L, "production")
    val shrunk = Shrink.shrink(original).take(5).toList
    shrunk.foreach(println)
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-scalacheck-derivation:{{ kindlings_version() }}
    //> using dep org.scalacheck::scalacheck:{{ libraries.scalacheck }}

    import hearth.kindlings.scalacheckderivation.*
    import org.scalacheck.Arbitrary

    case class Tree(value: Int, children: List[Tree])

    // No Lazy wrapper needed -- recursion is handled automatically
    implicit val arbTree: Arbitrary[Tree] = Arbitrary.derived[Tree]

    val sample = Arbitrary.arbitrary[Tree].sample.get
    println(sample)
    ```

## Debugging

Enable debug logging to see the derivation process:

```scala
import hearth.kindlings.scalacheckderivation.debug.*
```

Or via scalac option:

```
-Xmacro-settings:scalacheckDerivation.logDerivation=true
```

## Comparison with scalacheck-shapeless

### Feature differences

| Feature | scalacheck-shapeless | Kindlings |
|---------|---------------------|-----------|
| Same API on Scala 2.13 and 3 | No (Scala 2 only) | Yes |
| Arbitrary derivation | Yes | Yes |
| Cogen derivation | Yes | Yes |
| Shrink derivation | No | Yes |
| Recursive types | Needs `Lazy` wrappers | Just works |
| Scala 3 enums | No | Yes |
| Java enums | No | Yes |
| Named tuples | No | Yes |
| Opaque types | No | Yes |
| No shapeless dependency | No | Yes |
