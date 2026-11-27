package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyArrayIndexTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class ModifyArrayIndexTest extends QuicklensSuite {

  it should "modify a non-nested array with case class item" in {
    modify(ar1)(_.index(2).a4.a5.name).using(duplicate) should be(l1at2dup)
    modify(ar1)(_.index(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) should be(l1at2dup)
  }

  it should "modify a nested array using index" in {
    modify(arar1)(_.index(2).index(1).name).using(duplicate) should be(ll1at2at1dup)
  }

  it should "modify a nested array using index and each" in {
    modify(arar1)(_.index(2).each.name).using(duplicate) should be(ll1at2eachdup)
    modify(arar1)(_.each.index(1).name).using(duplicate) should be(ll1eachat1dup)
  }

  it should "not modify if given index does not exist" in {
    modify(ar1)(_.index(10).a4.a5.name).using(duplicate) should be(l1)
  }
}
