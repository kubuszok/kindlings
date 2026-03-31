package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary
import hearth.kindlings.scalacheckderivation.DeriveArbitrary
import hearth.kindlings.scalacheckderivation.extensions.*

/** Compilable examples that mirror the Scaladoc in Arbitrary.scala.
  *
  * These examples are compiled as part of the test suite to ensure documentation stays current with the API.
  */
@scala.annotation.nowarn
object examples {

  // ==Arbitrary.derived Syntax==
  // Recommended approach: use Arbitrary.derived directly
  object ArbitraryDerivedSyntax {
    case class Person(name: String, age: Int)
    object Person {
      implicit val arbPerson: Arbitrary[Person] = Arbitrary.derived[Person]
    }

    // For Scala 3, you can use the derives clause:
    // case class Book(title: String, author: String) derives Arbitrary
  }

  // ==Basic Usage==
  // From Arbitrary.scala lines 15-30
  object BasicUsage {
    case class Person(name: String, age: Int)
    object Person {
      implicit val arbPerson: Arbitrary[Person] = DeriveArbitrary.derived[Person]
    }

    // Example property using forAll (usage in test framework context)
    // property("age is always non-negative after normalization") {
    //   forAll { (p: Person) =>
    //     val normalized = p.copy(age = p.age.abs)
    //     normalized.age >= 0
    //   }
    // }
  }

  // ==Nested Types==
  // From Arbitrary.scala lines 32-39
  object NestedTypes {
    case class Address(city: String, country: String)
    case class Person(name: String, address: Address)

    object Address {
      implicit val arbAddress: Arbitrary[Address] = DeriveArbitrary.derived
    }
    object Person {
      implicit val arbPerson: Arbitrary[Person] = DeriveArbitrary.derived
    }
  }

  // ==Sealed Traits==
  // From Arbitrary.scala lines 41-50
  object SealedTraits {
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    object Circle {
      implicit val arbCircle: Arbitrary[Circle] = DeriveArbitrary.derived
    }
    object Rectangle {
      implicit val arbRectangle: Arbitrary[Rectangle] = DeriveArbitrary.derived
    }
    object Shape {
      implicit val arbShape: Arbitrary[Shape] = DeriveArbitrary.derived
    }
  }

  // ==Recursive Types==
  // From Arbitrary.scala lines 52-58
  object RecursiveTypes {
    case class Tree(value: Int, children: List[Tree])

    object Tree {
      implicit val arbTree: Arbitrary[Tree] = DeriveArbitrary.derived
      // No Lazy wrapper needed - recursion is handled automatically
    }
  }

  // ==Additional Examples==
  // Edge cases and common patterns not explicitly shown in Scaladoc
  object AdditionalExamples {

    // Zero-parameter case class
    case class Empty()
    object Empty {
      implicit val arbEmpty: Arbitrary[Empty] = DeriveArbitrary.derived
    }

    // Case class with Option
    case class PersonWithNickname(name: String, nickname: Option[String])
    object PersonWithNickname {
      implicit val arbPersonWithNickname: Arbitrary[PersonWithNickname] = DeriveArbitrary.derived
    }

    // Case class with collections
    case class Box(items: List[Int])
    object Box {
      implicit val arbBox: Arbitrary[Box] = DeriveArbitrary.derived
    }

    // Enum with only case objects
    sealed trait Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
    object Color {
      implicit val arbColor: Arbitrary[Color] = DeriveArbitrary.derived
    }

    // Mixed sealed trait (case objects + case classes)
    sealed trait Event
    case object Started extends Event
    case object Stopped extends Event
    case class Error(message: String) extends Event
    object Event {
      implicit val arbEvent: Arbitrary[Event] = DeriveArbitrary.derived
    }

    // Case class with multiple collection types
    case class MultiCollection(
        list: List[String],
        vector: Vector[Int],
        set: Set[Double]
    )
    object MultiCollection {
      implicit val arbMultiCollection: Arbitrary[MultiCollection] = DeriveArbitrary.derived
    }

    // Deeply nested structures
    case class Inner(value: Int)
    case class Middle(inner: Inner)
    case class Outer(middle: Middle)
    object Inner {
      implicit val arbInner: Arbitrary[Inner] = DeriveArbitrary.derived
    }
    object Middle {
      implicit val arbMiddle: Arbitrary[Middle] = DeriveArbitrary.derived
    }
    object Outer {
      implicit val arbOuter: Arbitrary[Outer] = DeriveArbitrary.derived
    }

    // Case class with Option of sealed trait
    sealed trait Status
    case object Active extends Status
    case object Inactive extends Status
    case class Pending(reason: String) extends Status
    case class Account(name: String, status: Option[Status])
    object Status {
      implicit val arbStatus: Arbitrary[Status] = DeriveArbitrary.derived
    }
    object Account {
      implicit val arbAccount: Arbitrary[Account] = DeriveArbitrary.derived
    }
  }
}
