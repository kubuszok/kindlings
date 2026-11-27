package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyPimpTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class ModifyPimpTest extends QuicklensSuite {
  it should "modify a field once" in {
    a1.modify(_.a2.a3.a4.a5.name).using(duplicate) should be(a1dup)
  }

  it should "modify a deeply-nested case class field" in {
    a1.modify(_.a2.a3.a4.a5.name)
      .using(duplicate)
      .modify(_.a2.a3.a4.a5.name)
      .using(duplicate) should be(a1dupdup)
  }

  it should "modify several fields" in {
    b1.modifyAll(_.b2, _.b3.each).using(duplicate) should be(b1dupdup)
  }

  it should "modify polymorphic case class field" in {
    aPoly.modify(_.poly).using(duplicate) should be(aPolyDup)
  }
}
