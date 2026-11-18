package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Gen, Shrink}

/** A field type whose `Arbitrary`/`Shrink` are `def`s counting how often they are created. */
final case class CountedValue(value: Int)
@scala.annotation.nowarn("cat=deprecation")
object CountedValue {
  @volatile var instancesCreated: Int = 0
  implicit def arbitraryCounted: Arbitrary[CountedValue] = {
    instancesCreated += 1
    Arbitrary(Gen.choose(0, 100).map(CountedValue(_)))
  }
  implicit def shrinkCounted: Shrink[CountedValue] = {
    instancesCreated += 1
    Shrink(c => if (c.value > 0) Stream(CountedValue(c.value / 2)) else Stream.empty)
  }
}
final case class WithCountedValue(counted: CountedValue, other: Int)

/** The field instances of a derived `Arbitrary`/`Shrink` are not evaluated again for every generated or shrunk value.
  */
@scala.annotation.nowarn
final class PerInstanceSpec extends munit.FunSuite {

  test("Arbitrary: generating samples does not re-create the field generators") {
    val arb: Arbitrary[WithCountedValue] = DeriveArbitrary.derived[WithCountedValue]
    val gen = arb.arbitrary
    gen.sample: Unit
    val before = CountedValue.instancesCreated
    val samples = List.fill(20)(gen.sample).flatten
    assert(samples.nonEmpty)
    assertEquals(CountedValue.instancesCreated - before, 0)
  }

  test("Shrink: shrinking does not re-create the field shrinks") {
    val shrink: Shrink[WithCountedValue] = DeriveShrink.derived[WithCountedValue]
    val value = WithCountedValue(CountedValue(64), 10)
    shrink.shrink(value).take(1).toList: Unit
    val before = CountedValue.instancesCreated
    val shrunk = shrink.shrink(value).take(20).toList
    assert(shrunk.contains(WithCountedValue(CountedValue(32), 10)), shrunk.toString)
    assertEquals(CountedValue.instancesCreated - before, 0)
  }

  test("Arbitrary for a wide case class constructs every field") {
    final case class Wide10(a: Int, b: Int, c: Int, d: Int, e: Int, f: String, g: Boolean, h: Long, i: Int, j: Int)
    val arb: Arbitrary[Wide10] = DeriveArbitrary.derived[Wide10]
    val samples = List.fill(10)(arb.arbitrary.sample).flatten
    assert(samples.nonEmpty)
  }
}
