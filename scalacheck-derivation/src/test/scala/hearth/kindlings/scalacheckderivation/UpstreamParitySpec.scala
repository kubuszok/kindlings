package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Cogen, Gen, Shrink}
import org.scalacheck.rng.Seed

/** Behaviour aligned with ScalaCheck's own instances and with scalacheck-shapeless / scalacheck-derived (the upstream
  * oracle in `upstream-oracle-tests` compares the derivations directly).
  */
object UpstreamParitySpec {
  sealed trait UpShape
  final case class UpCircle(radius: Int) extends UpShape
  final case class UpRect(width: Int, height: Int) extends UpShape
  case object UpOrigin extends UpShape

  sealed trait UpTree
  final case class UpLeaf(value: Int) extends UpTree
  final case class UpNode(left: UpTree, right: UpTree) extends UpTree

  final case class UpPerson(name: String, tags: List[Int], nick: Option[String])
  final case class UpNested(groups: List[List[Int]])

  // scalacheck-derived's test classes (core/src/test/scala/io/github/martinhh/test_classes.scala): subtypes in the same
  // order by name and by declaration, so that the expected generators are the same on Scala 2 and 3.
  sealed trait UpSimpleADT
  final case class UpSimpleCaseClass(x: Int, y: String, z: Double) extends UpSimpleADT
  case object UpSimpleCaseObject extends UpSimpleADT
  final case class UpCaseClassWithContainers(
      set: Set[Int],
      list: List[Boolean],
      option: Option[String],
      either: Either[String, Double]
  )
  // scalacheck-derived's `SealedDiamond`: `Bar` is reachable through two sealed subtraits
  sealed trait UpDiamond
  sealed trait UpDiamondA extends UpDiamond
  sealed trait UpDiamondB extends UpDiamond
  case object UpDiamondBar extends UpDiamondA with UpDiamondB
  case object UpDiamondFoo extends UpDiamond

  // scalacheck-shapeless' `WeekDay` (TestsDefinitions.scala)
  object UpWeekDay extends Enumeration {
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  final case class UpWithLiterals(i: Int, s: "lit", n: 42)
  final case class UpCaseClassWithListOfCaseClass(list: List[UpSimpleCaseClass])
  final case class UpMoreContainers(
      vector: Vector[UpSimpleCaseClass],
      map: Map[Int, String],
      nested: Option[List[Long]],
      tried: scala.util.Try[Int]
  )
}

@scala.annotation.nowarn("msg=deprecated")
final class UpstreamParitySpec extends munit.FunSuite {
  import UpstreamParitySpec.*

  private val seed = Seed(42L)

  private def depth(t: UpTree): Int = t match {
    case UpLeaf(_)    => 1
    case UpNode(l, r) => 1 + math.max(depth(l), depth(r))
  }

  test("sealed trait with case-class children derived inline (no implicit instances for the children)") {
    // Used to emit every cached child definition twice; the unused duplicates failed Scala 2 builds with -Werror.
    val arb = DeriveArbitrary.derived[UpShape]
    val shrink = DeriveShrink.derived[UpShape]
    val cogen = DeriveCogen.derived[UpShape]
    val samples = (0 until 200).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(10), Seed(i.toLong)))
    // By pattern match: `getSimpleName` of an object differs per platform (`UpOrigin$` on the JVM, `UpOrigin` on Native).
    val subtypes = samples.map {
      case _: UpCircle => "UpCircle"
      case _: UpRect   => "UpRect"
      case UpOrigin    => "UpOrigin"
    }.toSet
    assertEquals(subtypes, Set("UpCircle", "UpRect", "UpOrigin"))
    assertEquals(shrink.shrink(UpCircle(2)).toList, List(UpCircle(1), UpCircle(-1), UpCircle(0)))
    assertNotEquals(cogen.perturb(seed, UpCircle(1)), cogen.perturb(seed, UpOrigin))
  }

  /** scalacheck-derived's `ArbitrarySuite.equalArbitraryValues`: the same values as `expected` for 100 seeds. */
  private def equalArbitraryValues[A](expected: Gen[A], params: Gen.Parameters = Gen.Parameters.default)(implicit
      derived: org.scalacheck.Arbitrary[A]
  ): Unit =
    (0 until 100).foldLeft(Seed(1234L)) { (seed, _) =>
      assertEquals(derived.arbitrary(params, seed), expected(params, seed), s"Differing values for seed $seed")
      seed.next
    }: Unit

  private val simpleCaseClassGen: Gen[UpSimpleCaseClass] = for {
    x <- org.scalacheck.Arbitrary.arbitrary[Int]
    y <- org.scalacheck.Arbitrary.arbitrary[String]
    z <- org.scalacheck.Arbitrary.arbitrary[Double]
  } yield UpSimpleCaseClass(x, y, z)

  // scalacheck-derived ArbitraryDerivingSuite: "Generates same values as non-derived Gen (for simple case class)"
  test("Arbitrary for a case class generates what ScalaCheck's own instances generate for the same seed") {
    equalArbitraryValues(simpleCaseClassGen)(DeriveArbitrary.derived[UpSimpleCaseClass])
  }

  // scalacheck-derived ArbitraryDerivingSuite: "Generates same values as non-derived Gen (for case class with
  // containers)" and "... does not take precedence over existing given factories (e.g. for Lists)"
  test("Arbitrary for collections, Option, Either, Map, Try generates what ScalaCheck's own instances generate") {
    import org.scalacheck.Arbitrary.arbitrary
    val containersGen = for {
      set <- arbitrary[Set[Int]]
      list <- arbitrary[List[Boolean]]
      option <- arbitrary[Option[String]]
      either <- arbitrary[Either[String, Double]]
    } yield UpCaseClassWithContainers(set, list, option, either)
    equalArbitraryValues(containersGen)(DeriveArbitrary.derived[UpCaseClassWithContainers])
    List(0, 1, 7, 50).foreach { size =>
      equalArbitraryValues(containersGen, Gen.Parameters.default.withSize(size))(
        DeriveArbitrary.derived[UpCaseClassWithContainers]
      )
    }

    val listGen = Gen.listOf(simpleCaseClassGen).map(UpCaseClassWithListOfCaseClass(_))
    equalArbitraryValues(listGen)(DeriveArbitrary.derived[UpCaseClassWithListOfCaseClass])

    val moreGen = for {
      vector <- Gen.containerOf[Vector, UpSimpleCaseClass](simpleCaseClassGen)
      map <- arbitrary[Map[Int, String]]
      nested <- arbitrary[Option[List[Long]]]
      tried <- arbitrary[scala.util.Try[Int]]
    } yield UpMoreContainers(vector, map, nested, tried)
    // `Try` holds a `Throwable`, which has no structural equality: compare what is visible
    def visible(m: UpMoreContainers) = (m.vector, m.map, m.nested, m.tried.toEither.left.map(_.getMessage))
    val derived = DeriveArbitrary.derived[UpMoreContainers]
    (0 until 100).foldLeft(Seed(99L)) { (seed, _) =>
      val params = Gen.Parameters.default
      assertEquals(derived.arbitrary(params, seed).map(visible), moreGen(params, seed).map(visible), s"seed $seed")
      seed.next
    }: Unit
  }

  // scalacheck-derived ArbitraryDerivingSuite: "Generates same values as non-derived Gen (for simple ADT)" - the size
  // goes down by one at every sealed trait (at size 0 Kindlings still generates, see feature-parity.md).
  test("Arbitrary for a sealed trait generates what scalacheck-derived's generator generates for the same seed") {
    val expected: Gen[UpSimpleADT] = Gen.sized { size =>
      Gen.resize(size - 1, Gen.oneOf(simpleCaseClassGen, Gen.const(UpSimpleCaseObject)))
    }
    equalArbitraryValues(expected)(DeriveArbitrary.derived[UpSimpleADT])
  }

  // scalacheck-derived ArbitraryDerivingSuite: "even distribution in sealed traits with diamond inheritance"
  test("a subtype reachable through several sealed subtraits is picked once") {
    val expected: Gen[UpDiamond] = Gen.sized { size =>
      Gen.resize(size - 1, Gen.oneOf(Gen.const(UpDiamondBar), Gen.const(UpDiamondFoo)))
    }
    equalArbitraryValues(expected)(DeriveArbitrary.derived[UpDiamond])
  }

  // scalacheck-shapeless ArbitraryTests: "enumeration"
  test("Enumeration values are picked like Gen.oneOf(values)") {
    equalArbitraryValues[UpWeekDay.Value](Gen.oneOf(UpWeekDay.values.toSeq))(DeriveArbitrary.derived[UpWeekDay.Value])
    val cogen = DeriveCogen.derived[UpWeekDay.Value]
    assertNotEquals(cogen.perturb(seed, UpWeekDay.Mon), cogen.perturb(seed, UpWeekDay.Tue))
    assertEquals(DeriveShrink.derived[UpWeekDay.Value].shrink(UpWeekDay.Wed).toList, Nil)
  }

  // scalacheck-shapeless ArbitraryTests "singleton" (`Witness.\`2\`.T`), scalacheck-derived-extras ExtrasSuite (literal
  // types): a literal type always generates its value, and all its values perturb a seed alike.
  test("literal types generate their only value") {
    val expected = for {
      i <- org.scalacheck.Arbitrary.arbitrary[Int]
      s <- Gen.const["lit"]("lit")
      n <- Gen.const[42](42)
    } yield UpWithLiterals(i, s, n)
    equalArbitraryValues(expected)(DeriveArbitrary.derived[UpWithLiterals])
    val cogen = DeriveCogen.derived[UpWithLiterals]
    val value = UpWithLiterals(3, "lit", 42)
    assertEquals(
      cogen.perturb(seed, value),
      Cogen.perturb(Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, 3), 0L), 0L), ())
    )
    assertEquals(
      DeriveShrink.derived[UpWithLiterals].shrink(value).toList,
      Shrink.shrink(3).toList.map(i => value.copy(i = i))
    )
  }

  test("recursive sealed traits terminate at every size without a Recursive marker") {
    val arb = DeriveArbitrary.derived[UpTree]
    List(0, 1, 5, 30, 100).foreach { size =>
      val samples = (0 until 300).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(size), Seed(i.toLong)))
      assertEquals(samples.size, 300, s"size $size")
      assert(samples.forall(depth(_) < 1000), s"size $size")
    }
    // At size 0 only the non-recursive case is generated.
    val atZero = (0 until 50).flatMap(i => arb.arbitrary(Gen.Parameters.default.withSize(0), Seed(i.toLong)))
    assert(atZero.forall(_.isInstanceOf[UpLeaf]))
  }

  test("collections shrink with ScalaCheck's own algorithm") {
    val shrink = DeriveShrink.derived[UpPerson]
    val value = UpPerson("ab", List(1, 2, 3), Some("x"))
    val expectedTags = Shrink.shrink(List(1, 2, 3)).toList.map(tags => value.copy(tags = tags))
    val tagShrinks = shrink.shrink(value).toList.filter(p => p.name == value.name && p.nick == value.nick)
    assertEquals(tagShrinks, expectedTags)
  }

  test("Cogen perturbs like ScalaCheck's instances and the upstream derivations") {
    val person = DeriveCogen.derived[UpPerson]
    val value = UpPerson("ab", List(1, 2), Some("x"))
    // Case classes: every field in order, then `Cogen[Unit]` (scalacheck-shapeless and scalacheck-derived).
    val expected = Cogen.perturb(
      Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, value.name), value.tags), value.nick),
      ()
    )
    assertEquals(person.perturb(seed, value), expected)
    assertEquals(
      person.perturb(seed, value.copy(nick = None)),
      Cogen.perturb(Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, "ab"), List(1, 2)), Option.empty[String]), ())
    )

    // Collections end with `next`, so different groupings of the same elements perturb differently.
    val nested = DeriveCogen.derived[UpNested]
    assertNotEquals(
      nested.perturb(seed, UpNested(List(List(1), Nil))),
      nested.perturb(seed, UpNested(List(Nil, List(1))))
    )

    // Sealed traits: the case's Cogen, then the case's index (scalacheck-derived) - in declaration order on Scala 3,
    // by name on Scala 2 (UpCircle, UpOrigin, UpRect: the same in every build, see feature-parity.md).
    val isScala3 = ScalaVersionSpecific.isScala3
    val shape = DeriveCogen.derived[UpShape]
    assertEquals(shape.perturb(seed, UpCircle(3)), Cogen.perturb(Cogen.perturb(Cogen.perturb(seed, 3), ()), 0))
    assertEquals(shape.perturb(seed, UpOrigin), Cogen.perturb(Cogen.perturb(seed, ()), if (isScala3) 2 else 1))
  }
}
