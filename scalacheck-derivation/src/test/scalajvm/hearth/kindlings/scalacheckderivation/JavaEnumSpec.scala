package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Shrink}

@scala.annotation.nowarn
class JavaEnumSpec extends munit.FunSuite {

  test("derives Arbitrary for Java enum (java.time.Month)") {
    val arb: Arbitrary[java.time.Month] = DeriveArbitrary.derived[java.time.Month]
    val samples = List.fill(20)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate Java enum samples")
    assert(samples.forall(java.time.Month.values().contains(_)), "All samples should be valid Month values")
    assert(samples.distinct.size > 1, "Should generate diverse Month values")
  }

  test("derives Arbitrary for case class with Java enum field") {
    case class Schedule(month: java.time.Month, day: Int)

    implicit val arbMonth: Arbitrary[java.time.Month] = DeriveArbitrary.derived[java.time.Month]
    val arb: Arbitrary[Schedule] = DeriveArbitrary.derived[Schedule]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty, "Should generate Schedule samples with Java enum field")
  }

  test("derives Shrink for Java enum — produces empty stream (singletons)") {
    val shrink: Shrink[java.time.Month] = DeriveShrink.derived[java.time.Month]
    val result = shrink.shrink(java.time.Month.MARCH).take(5).toList
    // Each enum constant is a singleton — cannot shrink further
    assert(result.isEmpty, "Java enum constants should produce empty shrink stream")
  }

  test("derives Cogen for Java enum") {
    val cogen: Cogen[java.time.Month] = DeriveCogen.derived[java.time.Month]
    val seed = org.scalacheck.rng.Seed(12345L)
    val s1 = cogen.perturb(seed, java.time.Month.JANUARY)
    val s2 = cogen.perturb(seed, java.time.Month.DECEMBER)
    assert(s1 != s2, "Different Java enum values should perturb differently")
  }
}
