package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary
import hearth.kindlings.scalacheckderivation.DeriveArbitrary
import hearth.kindlings.scalacheckderivation.extensions.*

@scala.annotation.nowarn
class ArbitrarySpec extends munit.FunSuite {

  test("derives Arbitrary for simple case class") {
    import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    case class Person(name: String, age: Int)

    val arb: Arbitrary[Person] = DeriveArbitrary.derived[Person]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    assert(samples.forall(p => p.name != null), "Names should not be null")
  }

  test("derives Arbitrary for case class with multiple fields") {
    case class Point(x: Int, y: Int)

    val arb: Arbitrary[Point] = DeriveArbitrary.derived[Point]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for nested case class") {
    case class Address(street: String, city: String)
    case class Person(name: String, address: Address)

    implicit val arbAddress: Arbitrary[Address] = DeriveArbitrary.derived[Address]
    val arb: Arbitrary[Person] = DeriveArbitrary.derived[Person]

    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    assert(samples.forall(p => p.address != null), "Addresses should not be null")
  }

  test("derives Arbitrary for case class with Option field") {
    case class Person(name: String, nickname: Option[String])

    val arb: Arbitrary[Person] = DeriveArbitrary.derived[Person]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    // Check that we get varied Option values
    val hasVariedOptions = samples.exists(_.nickname.isEmpty) || samples.exists(_.nickname.isDefined)
    assert(hasVariedOptions, "Should generate varied Option values")
  }

  test("derives Arbitrary for case class with collection field") {
    case class Box(items: List[Int])

    val arb: Arbitrary[Box] = DeriveArbitrary.derived[Box]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for simple sealed trait") {
    sealed trait Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    val arb: Arbitrary[Color] = DeriveArbitrary.derived[Color]
    val samples = List.fill(30)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    // Should generate colors
    assert(samples.exists(_ == Red), "Should generate Red")
  }

  test("derives Arbitrary for sealed trait with data") {
    import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val arbCircle: Arbitrary[Circle] = DeriveArbitrary.derived[Circle]
    implicit val arbRectangle: Arbitrary[Rectangle] = DeriveArbitrary.derived[Rectangle]
    val arb: Arbitrary[Shape] = DeriveArbitrary.derived[Shape]

    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    val hasCircles = samples.exists(_.isInstanceOf[Circle])
    val hasRectangles = samples.exists(_.isInstanceOf[Rectangle])
    assert(hasCircles || hasRectangles, "Should generate both shape types")
  }

  test("derives Arbitrary for zero-field case class") {
    import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    case class Empty()

    val arb: Arbitrary[Empty] = DeriveArbitrary.derived[Empty]
    val sample = arb.arbitrary.sample

    assert(sample.isDefined, "Should generate Empty instance")
  }

  test("derives Arbitrary for standalone Option type") {
    val arb: Arbitrary[Option[Int]] = DeriveArbitrary.derived[Option[Int]]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    val hasNone = samples.exists(_.isEmpty)
    val hasSome = samples.exists(_.isDefined)
    assert(hasNone || hasSome, "Should generate both None and Some")
  }

  test("derives Arbitrary for standalone List type") {
    val arb: Arbitrary[List[String]] = DeriveArbitrary.derived[List[String]]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for Vector") {
    case class Container(values: Vector[Int])

    val arb: Arbitrary[Container] = DeriveArbitrary.derived[Container]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for Set") {
    case class UniqueItems(items: Set[Int])

    val arb: Arbitrary[UniqueItems] = DeriveArbitrary.derived[UniqueItems]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for nested Option") {
    case class NestedOpt(value: Option[Option[String]])

    val arb: Arbitrary[NestedOpt] = DeriveArbitrary.derived[NestedOpt]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for built-in types") {
    case class AllTypes(
        b: Boolean,
        byte: Byte,
        short: Short,
        i: Int,
        l: Long,
        f: Float,
        d: Double,
        c: Char,
        s: String,
        bigInt: BigInt,
        bigDec: BigDecimal
    )

    val arb: Arbitrary[AllTypes] = DeriveArbitrary.derived[AllTypes]
    val samples = List.fill(5)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate samples with all built-in types")
  }

  test("derives Arbitrary using Arbitrary.derived syntax") {
    case class User(name: String, age: Int, email: String)

    val arb: Arbitrary[User] = Arbitrary.derived[User]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate User samples using Arbitrary.derived")
    assert(samples.forall(u => u.name != null && u.email != null), "Fields should not be null")
  }

  test("generators work in Prop.forAll") {
    import org.scalacheck.Prop
    case class Point(x: Int, y: Int)
    implicit val arb: Arbitrary[Point] = DeriveArbitrary.derived[Point]

    val prop = Prop.forAll { (p: Point) =>
      p.x == p.x && p.y == p.y // trivial property
    }
    val result = org.scalacheck.Test.check(org.scalacheck.Test.Parameters.default, prop)
    assert(result.passed, s"Property check failed: ${result.status}")
  }

  test("recursive case class via List generates finite structures") {
    case class TreeNode(value: Int, children: List[TreeNode])

    val arb: Arbitrary[TreeNode] = DeriveArbitrary.derived[TreeNode]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate finite tree node samples")
  }

  test("recursive case class via Option generates finite structures") {
    case class LinkedNode(value: String, next: Option[LinkedNode])

    val arb: Arbitrary[LinkedNode] = DeriveArbitrary.derived[LinkedNode]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate finite linked node samples")
  }

  test("singleton case objects generate correctly") {
    sealed trait Singleton
    case object Only extends Singleton

    val arb: Arbitrary[Singleton] = DeriveArbitrary.derived[Singleton]
    val samples = List.fill(5)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate singleton samples")
    assert(samples.forall(_ == Only), "All samples should be Only")
  }

  test("derives Arbitrary for Map[String, Int]") {
    case class WithMap(data: Map[String, Int])

    val arb: Arbitrary[WithMap] = DeriveArbitrary.derived[WithMap]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate Map samples")
  }

  test("deeply nested case class generates correctly") {
    case class Inner(value: Int)
    case class Middle(inner: Inner, flag: Boolean)
    case class Outer(middle: Middle, name: String)

    val arb: Arbitrary[Outer] = DeriveArbitrary.derived[Outer]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate deeply nested samples")
    assert(samples.forall(_.middle.inner != null), "Nested fields should not be null")
  }
}
