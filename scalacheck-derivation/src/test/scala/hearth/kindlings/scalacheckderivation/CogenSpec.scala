package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Cogen, Gen}
import org.scalacheck.rng.Seed
import hearth.kindlings.scalacheckderivation.extensions.*

@scala.annotation.nowarn
class CogenSpec extends munit.FunSuite {

  private val baseSeed = Seed(12345L)

  test("derives Cogen for simple case class") {
    case class Point(x: Int, y: Int)
    val cogen: Cogen[Point] = DeriveCogen.derived[Point]
    val s1 = cogen.perturb(baseSeed, Point(1, 2))
    val s2 = cogen.perturb(baseSeed, Point(3, 4))
    val s3 = cogen.perturb(baseSeed, Point(1, 2))
    // Same input → same seed; different input → different seed
    assert(s1 == s3, "Same values should produce same seed")
    assert(s1 != s2, "Different values should produce different seeds")
  }

  test("derives Cogen for zero-field case class") {
    case class Empty()
    val cogen: Cogen[Empty] = DeriveCogen.derived[Empty]
    val s1 = cogen.perturb(baseSeed, Empty())
    val s2 = cogen.perturb(baseSeed, Empty())
    assert(s1 == s2, "Empty case class should always produce same seed")
  }

  test("derives Cogen for sealed trait") {
    sealed trait Color
    case object Red extends Color
    case object Blue extends Color

    val cogen: Cogen[Color] = DeriveCogen.derived[Color]
    val s1 = cogen.perturb(baseSeed, Red: Color)
    val s2 = cogen.perturb(baseSeed, Blue: Color)
    assert(s1 != s2, "Different variants should produce different seeds")
  }

  test("derives Cogen for Option") {
    val cogen: Cogen[Option[Int]] = DeriveCogen.derived[Option[Int]]
    val s1 = cogen.perturb(baseSeed, Some(42))
    val s2 = cogen.perturb(baseSeed, None)
    val s3 = cogen.perturb(baseSeed, Some(42))
    assert(s1 != s2, "Some and None should produce different seeds")
    assert(s1 == s3, "Same Some value should produce same seed")
  }

  test("derives Cogen for collection") {
    case class WithList(items: List[Int])
    val cogen: Cogen[WithList] = DeriveCogen.derived[WithList]
    val s1 = cogen.perturb(baseSeed, WithList(List(1, 2, 3)))
    val s2 = cogen.perturb(baseSeed, WithList(List(4, 5, 6)))
    val s3 = cogen.perturb(baseSeed, WithList(List(1, 2, 3)))
    assert(s1 == s3, "Same list should produce same seed")
    assert(s1 != s2, "Different lists should produce different seeds")
  }

  test("Cogen.derived extension syntax works") {
    case class Person(name: String, age: Int)
    val cogen: Cogen[Person] = Cogen.derived[Person]
    val s1 = cogen.perturb(baseSeed, Person("Alice", 30))
    val s2 = cogen.perturb(baseSeed, Person("Bob", 25))
    assert(s1 != s2, "Different persons should produce different seeds")
  }

  test("Cogen enables function generation via Gen.function1") {
    case class Point(x: Int, y: Int)
    implicit val cogenPoint: Cogen[Point] = DeriveCogen.derived[Point]
    implicit val arbPoint: org.scalacheck.Arbitrary[Point] = DeriveArbitrary.derived[Point]

    // Cogen enables Gen.function1 which generates random functions
    val genFn: Gen[Point => Int] = Gen.function1[Point, Int](Gen.choose(0, 100))
    val fn = genFn.sample.get
    // The function should be deterministic for same inputs
    val p = Point(1, 2)
    assert(fn(p) == fn(p), "Generated function should be deterministic")
  }

  test("built-in Int Cogen works") {
    val cogen: Cogen[Int] = DeriveCogen.derived[Int]
    val s1 = cogen.perturb(baseSeed, 42)
    val s2 = cogen.perturb(baseSeed, 43)
    assert(s1 != s2, "Different ints should produce different seeds")
  }
}
