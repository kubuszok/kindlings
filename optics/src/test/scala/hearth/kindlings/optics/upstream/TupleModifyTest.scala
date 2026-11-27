package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/TupleModifyTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
class TupleModifyTest extends QuicklensSuite {
  it should "modify case classes using setTo" in {
    case class Pair(a: Int, b: Int)
    val p = Pair(0, 1)
    val modified = p.modify(_.b).setTo(2)
    modified shouldBe Pair(0, 2)
  }
  it should "modify tuples using setTo" in {
    val tuple = (0, 1)
    val modified = tuple.modify(_._2).setTo(2)
    modified shouldBe ((0, 2))
  }
  it should "modify tuples using using" in {
    val tuple4 = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1)
    modified shouldBe ((0, 1, 3, 3))
  }

  it should "modify tuples using multiple modify" in {
    val tuple4 = (0, 1, 2, 3)
    val modified = tuple4.modify(_._3).using(_ + 1).modify(_._4).using(_ + 1)
    modified shouldBe ((0, 1, 3, 4))
  }
}
