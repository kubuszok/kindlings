package hearth.kindlings.scalacheckderivation

import org.scalacheck.{Cogen, Shrink}

final case class ScPrivateFields(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
class PrivateFieldsSpec extends munit.FunSuite {

  test("Cogen perturbs with private val constructor fields") {
    val cogen: Cogen[ScPrivateFields] = DeriveCogen.derived[ScPrivateFields]
    val seed = org.scalacheck.rng.Seed(12345L)
    assert(
      cogen.perturb(seed, ScPrivateFields(1, "two", c = true)) != cogen.perturb(
        seed,
        ScPrivateFields(1, "three", c = true)
      )
    )
    assert(
      cogen.perturb(seed, ScPrivateFields(1, "two", c = true)) != cogen.perturb(
        seed,
        ScPrivateFields(1, "two", c = false)
      )
    )
  }

  test("Shrink keeps private val constructor fields in their places") {
    val shrink: Shrink[ScPrivateFields] = DeriveShrink.derived[ScPrivateFields]
    val shrunk = shrink.shrink(ScPrivateFields(0, "two", c = false)).toList
    assert(shrunk.nonEmpty)
    // only `b` can shrink (`a` = 0 and `c` = false are minimal): every candidate keeps them, and shrinks `b`
    assert(shrunk.forall(s => s.a == 0 && !s.c && s.bValue != "two" && s.bValue.length <= 3), shrunk.toString)
  }
}
