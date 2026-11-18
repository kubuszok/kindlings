package hearth.kindlings.integrationtests

import hearth.MacroSuite
import hearth.kindlings.scalacheckderivation.{DeriveArbitrary, DeriveShrink}
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*
import org.scalacheck.{Arbitrary, Gen, Shrink}

/** Iron types are value types whose smart constructor returns `Either[String, A]` (see `RefinedScalacheckSpec`). */
final class IronScalacheckSpec extends MacroSuite {

  private val params = Gen.Parameters.default.withSize(20)

  group("Iron + scalacheck-derivation") {

    test("Arbitrary for a case class with Iron fields generates valid values") {
      val arb: Arbitrary[IronPerson] = DeriveArbitrary.derived[IronPerson]
      val generated = (1 to 50).toList.flatMap(i => arb.arbitrary(params, org.scalacheck.rng.Seed(i.toLong)))
      assert(generated.nonEmpty, "expected some generated values")
      generated.foreach { person =>
        assert(!person.name.isBlank, s"blank name in $person")
        assert(person.age > 0, s"non-positive age in $person")
      }
    }

    test("Shrink for a case class with Iron fields only yields valid values") {
      val shrink: Shrink[IronPerson] = DeriveShrink.derived[IronPerson]
      val person = IronPerson("Alice".refineUnsafe[Not[Blank]], 30.refineUnsafe[Positive])
      val shrunk = shrink.shrink(person).take(100).toList
      assert(shrunk.nonEmpty, "expected some shrunk values")
      shrunk.foreach { p =>
        assert(!p.name.isBlank, s"blank name in $p")
        assert(p.age > 0, s"non-positive age in $p")
      }
    }
  }
}
