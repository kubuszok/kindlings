package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala-3/com/softwaremill/quicklens/test/CustomModifyProxyTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
class CustomModifyProxyTest extends QuicklensSuite {

  it should "correctly modify a class using a custom modify proxy method" in {
    case class State(foo: Int)

    inline def set[A](state: State, inline path: State => A, value: A): State =
      modify(state)(path).setTo(value)

    val state = State(100)
    val res = set(state, _.foo, 200)
    val expected = State(200)
    res.shouldBe(expected)
  }

}
