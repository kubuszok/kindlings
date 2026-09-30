package hearth.kindlings.scalacheckderivation

import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed
import hearth.kindlings.scalacheckderivation.extensions.*

import scala.collection.immutable.{SortedMap, SortedSet}

final case class WithNEL(items: NonEmptyList[Int])
final case class WithNEV(items: NonEmptyVector[Int])
final case class WithNEC(items: NonEmptyChain[Int])
final case class WithChain(items: Chain[Int])
final case class WithNES(items: NonEmptySet[Int])
final case class WithNEM(items: NonEmptyMap[String, Int])

/** Issue kubuszok/kindlings#218: containers whose `CtorResult` is not the container itself, that are not Scala
  * `Iterable`s, and whose smart constructor rejects some inputs.
  */
@scala.annotation.nowarn
final class CatsContainersSpec extends munit.FunSuite {

  private def samples[A](gen: Gen[A], n: Int = 200): List[A] =
    (0 until n).toList.flatMap(i => gen.apply(Gen.Parameters.default.withSize(20), Seed(i.toLong)))

  test("Arbitrary generates real cats containers") {
    val nels = samples(Arbitrary.derived[WithNEL].arbitrary)
    assert(nels.nonEmpty)
    nels.foreach(v => assert(v.items.isInstanceOf[NonEmptyList[?]] && v.items.toList.nonEmpty))

    samples(Arbitrary.derived[WithNEV].arbitrary).foreach(v => assert(v.items.toVector.nonEmpty))
    samples(Arbitrary.derived[WithNEC].arbitrary).foreach(v => assert(v.items.toChain.nonEmpty))
    samples(Arbitrary.derived[WithChain].arbitrary).foreach(v => assert(v.items.isInstanceOf[Chain[?]]))
    samples(Arbitrary.derived[WithNES].arbitrary).foreach(v => assert(v.items.toSortedSet.nonEmpty))
    val nems = samples(Arbitrary.derived[WithNEM].arbitrary)
    assert(nems.nonEmpty)
    nems.foreach(v => assert(v.items.toSortedMap.isInstanceOf[SortedMap[?, ?]] && v.items.toSortedMap.nonEmpty))
  }

  test("Arbitrary for a standalone NonEmptyList never yields an empty or foreign value") {
    val gen = Arbitrary.derived[NonEmptyList[Int]].arbitrary
    val values = samples(gen)
    assert(values.size == 200, "every sample should succeed")
    values.foreach(nel => assert(nel.toList.nonEmpty))
  }

  test("Shrink produces smaller non-empty cats containers") {
    val nel = NonEmptyList.of(1, 2, 3)
    val shrunkNels = Shrink.derived[NonEmptyList[Int]].shrink(nel).take(50).toList
    assert(shrunkNels.nonEmpty)
    assert(shrunkNels.exists(_.size < 3))
    shrunkNels.foreach(s => assert(s.toList.nonEmpty))

    val single = NonEmptyList.one(0)
    assert(Shrink.derived[NonEmptyList[Int]].shrink(single).isEmpty, "cannot shrink below one element")

    val nemShrunk =
      Shrink.derived[WithNEM].shrink(WithNEM(NonEmptyMap.of("a" -> 1, "b" -> 2, "c" -> 3))).take(50).toList
    assert(nemShrunk.exists(_.items.length < 3))
    nemShrunk.foreach(v => assert(v.items.toSortedMap.nonEmpty))

    val chainShrunk = Shrink.derived[WithChain].shrink(WithChain(Chain(1))).take(50).toList
    assert(chainShrunk.exists(_.items.isEmpty), "Chain (unlike the NonEmpty* containers) can shrink down to empty")

    val nesShrunk = Shrink.derived[WithNES].shrink(WithNES(NonEmptySet.of(1, 2, 3))).take(50).toList
    assert(nesShrunk.exists(_.items.length < 3))
  }

  test("Cogen perturbs the seed from the elements of cats containers") {
    val cogen = Cogen.derived[NonEmptyList[Int]]
    val seed = Seed(0L)
    assertEquals(cogen.perturb(seed, NonEmptyList.of(1, 2, 3)), cogen.perturb(seed, NonEmptyList.of(1, 2, 3)))
    assertNotEquals(cogen.perturb(seed, NonEmptyList.of(1, 2, 3)), cogen.perturb(seed, NonEmptyList.of(1, 2, 4)))

    val nemCogen = Cogen.derived[WithNEM]
    assertNotEquals(
      nemCogen.perturb(seed, WithNEM(NonEmptyMap.of("a" -> 1))),
      nemCogen.perturb(seed, WithNEM(NonEmptyMap.of("a" -> 2)))
    )
    val nesCogen = Cogen.derived[WithNES]
    assertNotEquals(
      nesCogen.perturb(seed, WithNES(NonEmptySet.of(1))),
      nesCogen.perturb(seed, WithNES(NonEmptySet.of(2)))
    )
    val chainCogen = Cogen.derived[WithChain]
    assertNotEquals(chainCogen.perturb(seed, WithChain(Chain(1))), chainCogen.perturb(seed, WithChain(Chain(2))))
  }
}
