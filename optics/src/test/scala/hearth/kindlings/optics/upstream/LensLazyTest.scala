package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/LensLazyTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class LensLazyTest extends QuicklensSuite {
  it should "create reusable lens of the given type" in {
    val lens = modifyLens[A1](_.a2.a3.a4.a5.name)

    val lm = lens.using(duplicate)
    lm(a1) should be(a1dup)
  }

  it should "compose lens" in {
    val lens_a1_a3 = modifyLens[A1](_.a2.a3)
    val lens_a3_name = modifyLens[A3](_.a4.a5.name)

    val lm = (lens_a1_a3 andThenModify lens_a3_name).using(duplicate)
    lm(a1) should be(a1dup)
  }
}
