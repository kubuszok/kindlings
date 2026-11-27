package hearth.kindlings.optics.upstream

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/SecondParamListTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].

@scala.annotation.nowarn // upstream's tests discard values and keep unused definitions
class SecondParamListTest extends QuicklensSuite {
  it should "modify an object with second implicit param list" in {
    import hearth.kindlings.optics._

    case class State(inside: Boolean)(implicit d: Double)

    val d: Double = 1.0

    val state1 = State(true)(d)

    implicit val dd: Double = d
    val state2 = state1.modify(_.inside).setTo(true)

    state1 should be(state2)
  }

  it should "should give a meaningful error for an object with more than one non-implicit param list" in {

    case class State(inside: Boolean)(d: Double)

    val d: Double = 1.0

    val state1 = State(true)(d)

    implicit val dd: Double = d

    compileErrors("state1.modify(_.inside).setTo(true)").arePresent()
  }
}
