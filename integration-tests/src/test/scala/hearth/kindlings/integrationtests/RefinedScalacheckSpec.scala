package hearth.kindlings.integrationtests

import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive
import hearth.MacroSuite
import hearth.kindlings.scalacheckderivation.{DeriveArbitrary, DeriveShrink}
import org.scalacheck.{Arbitrary, Gen, Shrink}

/** Refined types are value types whose smart constructor returns `Either[String, A]`: the derived generator must unwrap
  * the `Right` (and regenerate rejected candidates), the derived `Shrink` must drop rejected candidates.
  */
final class RefinedScalacheckSpec extends MacroSuite {

  private val params = Gen.Parameters.default.withSize(20)

  private def samples[A](arb: Arbitrary[A], n: Int = 50): List[A] =
    (1 to n).toList.flatMap(i => arb.arbitrary(params, org.scalacheck.rng.Seed(i.toLong)))

  group("Refined + scalacheck-derivation") {

    test("Arbitrary for a case class with refined fields generates valid values") {
      val arb: Arbitrary[RefinedPerson] = DeriveArbitrary.derived[RefinedPerson]
      val generated = samples(arb)
      assert(generated.nonEmpty, "expected some generated values")
      generated.foreach { person =>
        assert(person.name.value.nonEmpty, s"empty name in $person")
        assert(person.age.value > 0, s"non-positive age in $person")
      }
    }

    test("Arbitrary for refined values in an Option and a List generates valid values") {
      val arbOpt: Arbitrary[WithRefinedOption] = DeriveArbitrary.derived[WithRefinedOption]
      samples(arbOpt).flatMap(_.value).foreach(v => assert(v.value > 0, s"non-positive $v"))
      val arbList: Arbitrary[WithRefinedList] = DeriveArbitrary.derived[WithRefinedList]
      samples(arbList).flatMap(_.values).foreach(v => assert(v.value.nonEmpty, s"empty $v"))
    }

    test("Shrink for a case class with refined fields only yields valid values") {
      val shrink: Shrink[RefinedPerson] = DeriveShrink.derived[RefinedPerson]
      val person = RefinedPerson(refineV[NonEmpty]("Alice").toOption.get, refineV[Positive](30).toOption.get)
      val shrunk = shrink.shrink(person).take(100).toList
      assert(shrunk.nonEmpty, "expected some shrunk values")
      shrunk.foreach { p =>
        assert(p.name.value.nonEmpty, s"empty name in $p")
        assert(p.age.value > 0, s"non-positive age in $p")
      }
    }
  }
}
