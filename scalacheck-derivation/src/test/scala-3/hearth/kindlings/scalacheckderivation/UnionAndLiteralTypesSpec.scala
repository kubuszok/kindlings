package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

/** Union and literal types (Scala 3 only), mirroring scalacheck-derived-extras' `ExtrasSuite`
  * (extras/src/test/scala/io/github/martinhh/ExtrasSuite.scala): the same values, seeds and shrinks as its expected
  * instances.
  */
@scala.annotation.nowarn("msg=deprecated")
final class UnionAndLiteralTypesSpec extends munit.FunSuite {
  import UnionAndLiteralTypesSpec.*

  private def equalArbitraryValues[A](expected: Gen[A])(using derived: Arbitrary[A]): Unit =
    (0 until 100).foldLeft(Seed(4242L)) { (seed, _) =>
      val params = Gen.Parameters.default
      assertEquals(derived.arbitrary(params, seed), expected(params, seed), s"Differing values for seed $seed")
      seed.next
    }: Unit

  private def equalCogenValues[A](expected: Cogen[A], values: List[A])(using derived: Cogen[A]): Unit =
    values.foreach { value =>
      assertEquals(derived.perturb(Seed(7L), value), expected.perturb(Seed(7L), value), s"value $value")
    }

  private def equalShrinkValues[A](expected: Shrink[A], values: List[A])(using derived: Shrink[A]): Unit =
    values.foreach { value =>
      assertEquals(derived.shrink(value).take(30).toList, expected.shrink(value).take(30).toList, s"value $value")
    }

  test("Arbitrary for union of two types") {
    type TheUnion = String | Int
    equalArbitraryValues(Gen.oneOf[TheUnion](arbitrary[String], arbitrary[Int]))(using
      DeriveArbitrary.derived[TheUnion]
    )
  }

  test("Arbitrary for union of three types") {
    type TheUnion = Boolean | String | Int
    equalArbitraryValues(Gen.oneOf[TheUnion](arbitrary[Boolean], arbitrary[String], arbitrary[Int]))(using
      DeriveArbitrary.derived[TheUnion]
    )
  }

  test("Arbitrary for union of more complex types") {
    type TheUnion = Set[Double] | Option[String] | List[Int]
    equalArbitraryValues(
      Gen.oneOf[TheUnion](arbitrary[Set[Double]], arbitrary[Option[String]], arbitrary[List[Int]])
    )(using DeriveArbitrary.derived[TheUnion])
  }

  test("Arbitrary for literal types and unions of literal types") {
    equalArbitraryValues(Gen.const["Foo"]("Foo"))(using DeriveArbitrary.derived["Foo"])
    type TheUnion = "Foo" | "Bar" | "Baz"
    equalArbitraryValues(
      Gen.oneOf[TheUnion](Gen.const[TheUnion]("Foo"), Gen.const[TheUnion]("Bar"), Gen.const[TheUnion]("Baz"))
    )(using DeriveArbitrary.derived[TheUnion])
    type Numbers = 1 | 2 | 3
    equalArbitraryValues(Gen.oneOf[Numbers](Gen.const[Numbers](1), Gen.const[Numbers](2), Gen.const[Numbers](3)))(using
      DeriveArbitrary.derived[Numbers]
    )
  }

  test("Arbitrary for union of two case classes") {
    type TheUnion = UA | UB
    equalArbitraryValues(Gen.oneOf[TheUnion](Gen.const[TheUnion](UA()), Gen.const[TheUnion](UB())))(using
      DeriveArbitrary.derived[TheUnion]
    )
  }

  test("Arbitrary for a case class with a literal-typed field and a union-typed field") {
    val expected = for {
      i <- arbitrary[Int]
      l <- Gen.const["lit"]("lit")
      u <- Gen.oneOf[Int | String](arbitrary[Int], arbitrary[String])
    } yield WithLiteralAndUnion(i, l, u)
    equalArbitraryValues(expected)(using DeriveArbitrary.derived[WithLiteralAndUnion])
  }

  test("Cogen for union of literal types") {
    type TheUnion = 1 | 2
    val expected: Cogen[TheUnion] = Cogen { (seed, value) =>
      value match {
        case 1 => Cogen.perturb(Cogen.perturb(seed, 0L), 0)
        case 2 => Cogen.perturb(Cogen.perturb(seed, 0L), 1)
      }
    }
    equalCogenValues[TheUnion](expected, List(1, 2))(using DeriveCogen.derived[TheUnion])
  }

  test("Cogen for union of three types") {
    type TheUnion = String | Int | Boolean
    val expected: Cogen[TheUnion] = Cogen { (seed, value) =>
      value match {
        case s: String  => Cogen.perturb(Cogen.perturb(seed, s), 0)
        case i: Int     => Cogen.perturb(Cogen.perturb(seed, i), 1)
        case b: Boolean => Cogen.perturb(Cogen.perturb(seed, b), 2)
      }
    }
    equalCogenValues[TheUnion](expected, List("", "abc", 0, -5, 42, true, false))(using DeriveCogen.derived[TheUnion])
  }

  test("Shrink for union of three types") {
    type TheUnion = String | Int | Boolean
    val expected: Shrink[TheUnion] = Shrink { (a: TheUnion) =>
      a match {
        case s: String  => Shrink.shrinkString.shrink(s)
        case i: Int     => Shrink.shrinkIntegral[Int].shrink(i)
        case b: Boolean => Shrink.shrinkAny[Boolean].shrink(b)
      }
    }
    equalShrinkValues[TheUnion](expected, List("", "abc", 0, -5, 42, true, false))(using
      DeriveShrink.derived[TheUnion]
    )
  }
}

object UnionAndLiteralTypesSpec {
  final case class UA()
  final case class UB()
  final case class WithLiteralAndUnion(i: Int, l: "lit", u: Int | String)
}
