package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyOptionIndexTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
class ModifyOptionIndexTest extends QuicklensSuite {

  it should "modify a Option with case class item" in {
    modify(Option(1))(_.index).using(_ + 1) should be(Option(2))
  }

  it should "modify a Option in a case class hierarchy" in {
    case class Foo(a: Int)
    case class Bar(foo: Foo)
    case class BarOpt(maybeBar: Option[Bar])
    case class BazOpt(barOpt: BarOpt)
    modify(BazOpt(BarOpt(Some(Bar(Foo(4))))))(_.barOpt.maybeBar.index.foo.a).using(_ + 1) should be(
      BazOpt(BarOpt(Some(Bar(Foo(5)))))
    )
  }

  it should "not modify on missing key" in {
    modify(Option.empty[Int])(_.index).using(_ + 1) should be(None)
  }
}
