package hearth.kindlings.scalacheckderivation

import org.scalacheck.Arbitrary
import hearth.kindlings.scalacheckderivation.extensions.*

/** Tests for Scala 3 derives clause syntax */
class DerivesArbitrarySpec extends munit.FunSuite {

  test("derives Arbitrary using derives clause") {
    case class Book(title: String, author: String, pages: Int) derives Arbitrary

    val arb = summon[Arbitrary[Book]]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate Book samples using derives clause")
    assert(samples.forall(b => b.title != null && b.author != null), "Fields should not be null")
  }

  test("derives Arbitrary for sealed trait using derives clause") {
    sealed trait Status derives Arbitrary
    case object Active extends Status
    case object Inactive extends Status
    case class Pending(reason: String) extends Status derives Arbitrary

    val arb = summon[Arbitrary[Status]]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate Status samples")
  }

  test("derives Arbitrary for nested types with derives clause") {
    case class Address(street: String, city: String) derives Arbitrary
    case class Company(name: String, address: Address) derives Arbitrary

    val arb = summon[Arbitrary[Company]]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten

    assert(samples.nonEmpty, "Should generate Company samples")
    assert(samples.forall(c => c.address != null), "Nested Address should not be null")
  }
}
