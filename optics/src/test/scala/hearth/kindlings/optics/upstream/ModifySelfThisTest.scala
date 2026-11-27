package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifySelfThisTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import ModifySelfThisTest.*

@scala.annotation.nowarn // upstream declares a side-effecting nullary method
object ModifySelfThisTest {

  case class State(x: Int) { self =>

    def mod: State = this.modify(_.x).setTo(1)
  }

  trait A {
    def a: Unit
  }

  case class State1(x: Int) extends A { self: A =>

    def mod: State1 = this.modify(_.x).setTo(1)

    def a: Unit = ()
  }
}

class ModifySelfThisTest extends QuicklensSuite {
  it should "modify an object even in presence of self alias" in {
    val s = State(0)
    val modified = s.mod

    modified.x shouldBe 1
  }

  it should "modify an object even in presence of self type" in {
    val s = State(0)
    val modified = s.mod

    modified.x shouldBe 1
  }
}
