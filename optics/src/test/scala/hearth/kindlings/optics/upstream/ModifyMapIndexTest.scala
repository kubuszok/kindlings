package hearth.kindlings.optics.upstream

import hearth.kindlings.optics._

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyMapIndexTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData._

class ModifyMapIndexTest extends QuicklensSuite {

  it should "modify a non-nested map with case class item" in {
    modify(m1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested sorted map with case class item" in {
    modify(ms1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested hash map with case class item" in {
    modify(mh1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a non-nested listed map with case class item" in {
    modify(ml1)(_.index("K1").a5.name).using(duplicate) should be(m1dup)
  }

  it should "modify a nested map using index" in {
    modify(m2)(_.m3.index("K1").a5.name).using(duplicate) should be(m2dup)
  }

  it should "not modify if there's no such element" in {
    modify(m1)(_.index("K0").a5.name).using(duplicate) should be(m1)
  }
}
