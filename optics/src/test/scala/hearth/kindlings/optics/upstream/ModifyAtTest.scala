package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyAtTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import TestData.*

class ModifyAtTest extends QuicklensSuite {

  it should "modify a non-nested list with case class item" in {
    modify(l1)(_.at(2).a4.a5.name).using(duplicate) should be(l1at2dup)
    modify(l1)(_.at(2))
      .using(a3 => modify(a3)(_.a4.a5.name).using(duplicate)) should be(l1at2dup)
  }

  it should "modify a nested list using at" in {
    modify(ll1)(_.at(2).at(1).name).using(duplicate) should be(ll1at2at1dup)
  }

  it should "modify a nested list using at and each" in {
    modify(ll1)(_.at(2).each.name).using(duplicate) should be(ll1at2eachdup)
    modify(ll1)(_.each.at(1).name).using(duplicate) should be(ll1eachat1dup)
  }

  it should "modify both lists and options" in {
    modify(y1)(_.y2.y3.at(1).y4.each.name).using(duplicate) should be(y1at1dup)
  }

  it should "throw an exception if there's no element at the given index" in {
    an[IndexOutOfBoundsException] should be thrownBy
      modify(l1)(_.at(10).a4.a5.name).using(duplicate)
  }
}
