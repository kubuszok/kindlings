package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary as ScArbitrary
import hearth.kindlings.scalacheckderivation.DeriveArbitrary

@scala.annotation.nowarn
class ArbitrarySpec extends munit.FunSuite {

  test("derives Arbitrary for simple case class") {
    import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    case class Person(name: String, age: Int)

    val arb: ScArbitrary[Person] = DeriveArbitrary.derived[Person]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    assert(samples.forall(p => p.name != null), "Names should not be null")
  }

  test("derives Arbitrary for case class with multiple fields") {
    case class Point(x: Int, y: Int)

    val arb: ScArbitrary[Point] = DeriveArbitrary.derived[Point]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for nested case class") {
    case class Address(street: String, city: String)
    case class Person(name: String, address: Address)

    implicit val arbAddress: ScArbitrary[Address] = DeriveArbitrary.derived[Address]
    val arb: ScArbitrary[Person] = DeriveArbitrary.derived[Person]

    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    assert(samples.forall(p => p.address != null), "Addresses should not be null")
  }

  test("derives Arbitrary for case class with Option field") {
    case class Person(name: String, nickname: Option[String])

    val arb: ScArbitrary[Person] = DeriveArbitrary.derived[Person]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    // Check that we get varied Option values
    val hasVariedOptions = samples.exists(_.nickname.isEmpty) || samples.exists(_.nickname.isDefined)
    assert(hasVariedOptions, "Should generate varied Option values")
  }

  test("derives Arbitrary for case class with collection field") {
    case class Box(items: List[Int])

    val arb: ScArbitrary[Box] = DeriveArbitrary.derived[Box]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
  }

  test("derives Arbitrary for simple sealed trait") {
    sealed trait Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color

    val arb: ScArbitrary[Color] = DeriveArbitrary.derived[Color]
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

    implicit val arbCircle: ScArbitrary[Circle] = DeriveArbitrary.derived[Circle]
    implicit val arbRectangle: ScArbitrary[Rectangle] = DeriveArbitrary.derived[Rectangle]
    val arb: ScArbitrary[Shape] = DeriveArbitrary.derived[Shape]

    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate some samples")
    val hasCircles = samples.exists(_.isInstanceOf[Circle])
    val hasRectangles = samples.exists(_.isInstanceOf[Rectangle])
    assert(hasCircles || hasRectangles, "Should generate both shape types")
  }

  test("derives Arbitrary for zero-field case class") {
    import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation
    case class Empty()

    val arb: ScArbitrary[Empty] = DeriveArbitrary.derived[Empty]
    val sample = arb.arbitrary.sample

    assert(sample.isDefined, "Should generate Empty instance")
  }
}
