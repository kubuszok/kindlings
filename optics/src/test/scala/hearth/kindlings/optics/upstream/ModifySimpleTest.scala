package hearth.kindlings.optics.upstream

import hearth.kindlings.optics._

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifySimpleTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData._

class ModifySimpleTest extends QuicklensSuite {
  it should "modify a single-nested case class field" in {
    modify(a5)(_.name).using(duplicate) should be(a5dup)
  }

  it should "modify a single-nested case class field using apply" in {
    modify(a5)(_.name)(duplicate) should be(a5dup)
  }

  it should "modify a deeply-nested case class field" in {
    modify(a1)(_.a2.a3.a4.a5.name).using(duplicate) should be(a1dup)
  }

  it should "modify several fields" in {
    modifyAll(b1)(_.b2, _.b3.each).using(duplicate) should be(b1dupdup)
  }

  it should "modify a case class field if the condition is true" in {
    modify(a5)(_.name).usingIf(true)(duplicate) should be(a5dup)
  }

  it should "leave a case class unchanged if the condition is false" in {
    modify(a5)(_.name).usingIf(false)(duplicate) should be(a5)
  }

  it should "modify polymorphic case class field" in {
    modify(aPoly)(_.poly).using(duplicate) should be(aPolyDup)
  }

  it should "modify polymorphic case class field using apply" in {
    modify(aPoly)(_.poly)(duplicate) should be(aPolyDup)
  }

  it should "modify polymorphic case class field if condition is true" in {
    modify(aPoly)(_.poly).usingIf(true)(duplicate) should be(aPolyDup)
  }
  it should "leave a polymorphic case class field if condition is false" in {
    modify(aPoly)(_.poly).usingIf(false)(duplicate) should be(aPoly)
  }
}
