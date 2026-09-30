package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed
import hearth.kindlings.scalacheckderivation.extensions.*

import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.Try

object ScalaCheckCoverageSpec {
  final case class Inner(value: Int)

  // Every leaf type ScalaCheck 1.20 has an `Arbitrary` for, available on all platforms.
  final case class ArbLeaves(
      bool: Boolean,
      byte: Byte,
      short: Short,
      int: Int,
      long: Long,
      float: Float,
      double: Double,
      char: Char,
      string: String,
      unit: Unit,
      bigInt: BigInt,
      bigDecimal: BigDecimal,
      symbol: Symbol,
      uuid: java.util.UUID,
      bitSet: scala.collection.BitSet,
      duration: Duration,
      finiteDuration: FiniteDuration,
      throwable: Throwable,
      exception: Exception,
      error: Error
  )
  // ScalaCheck combinators without a structural counterpart - still ScalaCheck's instances.
  final case class ArbCombinators(
      f0: () => Int,
      f1: Int => Int,
      f2: (Int, String) => Int,
      pf: PartialFunction[Int, Int],
      future: Future[Int],
      gen: Gen[Int]
  )
  // ScalaCheck combinators replaced by structural rules - the nested case class is derived, not required implicitly.
  final case class Structural(
      list: List[Inner],
      vector: Vector[Inner],
      set: Set[Inner],
      map: Map[String, Inner],
      option: Option[Inner],
      either: Either[Inner, Int],
      tryValue: Try[Inner],
      tuple: (Inner, Int)
  )

  // Every leaf type ScalaCheck 1.20 has a `Cogen` for, available on all platforms.
  final case class CogenLeaves(
      bool: Boolean,
      byte: Byte,
      short: Short,
      int: Int,
      long: Long,
      float: Float,
      double: Double,
      char: Char,
      string: String,
      unit: Unit,
      bigInt: BigInt,
      bigDecimal: BigDecimal,
      symbol: Symbol,
      uuid: java.util.UUID,
      bitSet: scala.collection.immutable.BitSet,
      duration: Duration,
      finiteDuration: FiniteDuration,
      throwable: Throwable,
      exception: Exception,
      setOfInts: Set[Int],
      mapOfInts: Map[Int, Int]
  )
  final case class CogenCombinators(f0: () => Int, f1: Int => Int, pf: PartialFunction[Int, Int])

  // Every leaf type ScalaCheck 1.20 has a `Shrink` for (other types fall back to its no-op `shrinkAny`).
  final case class ShrinkLeaves(
      int: Int,
      long: Long,
      double: Double,
      string: String,
      bigInt: BigInt,
      duration: Duration,
      finiteDuration: FiniteDuration,
      uuid: java.util.UUID
  )

  final case class Box[A](value: A)
  final case class WithBox(box: Box[Int])
}

/** Verifies that derivation supports every type ScalaCheck supports out of the box, within a single macro expansion:
  * ScalaCheck's leaf instances are used as built-ins, while the combinators Kindlings replaces (collections, Option,
  * Either, Try, tuples) recurse through derivation, so nested types need no implicit in scope.
  */
@scala.annotation.nowarn
final class ScalaCheckCoverageSpec extends munit.FunSuite {
  import ScalaCheckCoverageSpec.*

  private val params = Gen.Parameters.default.withSize(10)
  private def samples[A](gen: Gen[A], n: Int = 50): List[A] =
    (0 until n).toList.flatMap(i => gen.apply(params, Seed(i.toLong)))

  test("Arbitrary: every ScalaCheck leaf type") {
    assertEquals(samples(Arbitrary.derived[ArbLeaves].arbitrary).size, 50)
  }

  test("Arbitrary: ScalaCheck combinators without a structural counterpart") {
    // ScalaCheck's own generators for some of these occasionally yield no value, so not every sample succeeds
    val values = samples(Arbitrary.derived[ArbCombinators].arbitrary)
    assert(values.nonEmpty)
    values.foreach { v =>
      v.f0(); v.f1(1); v.f2(1, "a"); v.pf.isDefinedAt(1)
      v.gen.apply(params, Seed(0L))
    }
  }

  test("Arbitrary: structurally replaced combinators derive the nested type") {
    val values = samples(Arbitrary.derived[Structural].arbitrary)
    assertEquals(values.size, 50)
    assert(values.exists(_.list.nonEmpty))
    assert(values.exists(_.option.isDefined))
    assert(values.exists(_.tryValue.isSuccess))
  }

  test("Arbitrary: self-type served by ScalaCheck's instance") {
    assertEquals(samples(Arbitrary.derived[String].arbitrary).size, 50)
    assertEquals(samples(Arbitrary.derived[java.util.UUID].arbitrary).size, 50)
    assertEquals(samples(Arbitrary.derived[Duration].arbitrary).size, 50)
  }

  test("Arbitrary: a user-provided generic instance built on ScalaCheck's leaf instances wins") {
    implicit def arbBox[A](implicit a: Arbitrary[A]): Arbitrary[Box[A]] =
      Arbitrary(a.arbitrary.map(_ => Box(42.asInstanceOf[A])))
    samples(Arbitrary.derived[WithBox].arbitrary).foreach(v => assertEquals(v.box, Box(42)))
  }

  test("Cogen: every ScalaCheck leaf type") {
    val cogen = Cogen.derived[CogenLeaves]
    val value = CogenLeaves(true, 1, 2, 3, 4L, 5f, 6d, 'c', "s", (), BigInt(7), BigDecimal(8), Symbol("sym"),
      new java.util.UUID(1L, 2L), scala.collection.immutable.BitSet(1, 2), 1.second, 2.seconds, new RuntimeException("t"), new Exception("e"),
      Set(1, 2, 3), Map(1 -> 2, 3 -> 4))
    val seed = Seed(0L)
    assertEquals(cogen.perturb(seed, value), cogen.perturb(seed, value))
    assertNotEquals(cogen.perturb(seed, value), cogen.perturb(seed, value.copy(int = 4)))
    // cogenSet/cogenMap are kept: equal sets built in a different order perturb identically
    assertEquals(
      cogen.perturb(seed, value.copy(setOfInts = Set(3, 2, 1), mapOfInts = Map(3 -> 4, 1 -> 2))),
      cogen.perturb(seed, value)
    )
  }

  test("Cogen: ScalaCheck combinators without a structural counterpart") {
    val cogen = Cogen.derived[CogenCombinators]
    val value = CogenCombinators(() => 1, _ + 1, { case 1 => 2 })
    cogen.perturb(Seed(0L), value)
  }

  test("Cogen: structurally replaced combinators derive the nested type") {
    val cogen = Cogen.derived[Structural]
    val value = Structural(List(Inner(1)), Vector(Inner(2)), Set(Inner(3)), Map("a" -> Inner(4)), Some(Inner(5)),
      Left(Inner(6)), scala.util.Success(Inner(7)), (Inner(8), 9))
    val seed = Seed(0L)
    assertEquals(cogen.perturb(seed, value), cogen.perturb(seed, value))
    assertNotEquals(cogen.perturb(seed, value), cogen.perturb(seed, value.copy(option = Some(Inner(50)))))
    assertNotEquals(cogen.perturb(seed, value), cogen.perturb(seed, value.copy(tryValue = scala.util.Success(Inner(0)))))
  }

  test("Shrink: every ScalaCheck leaf type") {
    val value = ShrinkLeaves(100, 100L, 1.5, "abc", BigInt(100), 10.seconds, 20.seconds, new java.util.UUID(1L, 2L))
    val result = Shrink.derived[ShrinkLeaves].shrink(value).take(1000).toList
    assert(result.exists(_.int != 100))
    assert(result.exists(_.long != 100L))
    assert(result.exists(_.double != 1.5))
    assert(result.exists(_.string != "abc"))
    assert(result.exists(_.bigInt != BigInt(100)))
    assert(result.exists(_.duration != 10.seconds))
    assert(result.exists(_.finiteDuration != 20.seconds))
    assert(result.forall(_.uuid == value.uuid), "no Shrink for UUID in ScalaCheck - falls back to shrinkAny")
  }

  test("Arbitrary/Shrink: a builder exception is a rejected candidate, not a runtime exception") {
    // Neither ScalaCheck nor the structural rule knows immutable.BitSet accepts only non-negative elements
    final case class WithBitSet(bits: scala.collection.immutable.BitSet)
    samples(Arbitrary.derived[WithBitSet].arbitrary).foreach(v => assert(v.bits.forall(_ >= 0)))
    Shrink.derived[WithBitSet].shrink(WithBitSet(scala.collection.immutable.BitSet(1, 2, 3))).toList
  }

  test("Shrink: self-type served by ScalaCheck's instance") {
    assert(Shrink.derived[String].shrink("abc").nonEmpty)
    assert(Shrink.derived[Duration].shrink(10.seconds).nonEmpty)
  }
}
