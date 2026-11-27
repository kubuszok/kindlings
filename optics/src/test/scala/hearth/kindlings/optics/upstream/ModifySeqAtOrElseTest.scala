package hearth.kindlings.optics.upstream

import hearth.kindlings.optics._

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifySeqAtOrElseTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
class ModifySeqAtOrElseTest extends QuicklensSuite {

  case class Item(name: String)

  it should "modify an existing element using atOrElse" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(1, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("B"))
    )
  }

  it should "use the default for a missing index instead of throwing" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(5, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }

  it should "append the modified default on an empty sequence" in {
    val items = List.empty[Item]
    modify(items)(_.atOrElse(0, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("DEFAULT"))
    )
  }

  it should "append the modified default for a negative index" in {
    val items = List(Item("a"), Item("b"))
    modify(items)(_.atOrElse(-1, Item("default")).name).using(_.toUpperCase) should be(
      List(Item("a"), Item("b"), Item("DEFAULT"))
    )
  }
}
