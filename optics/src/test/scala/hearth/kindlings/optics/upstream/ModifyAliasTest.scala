package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/ModifyAliasTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import ModifyAliasTest.*

object ModifyAliasTest {

  case class State(x: Int)

  type S = State

  sealed trait Expr {
    def i: Int
  }
  case class ListInt(i: Int) extends Expr

  type E = Expr
}

class ModifyAliasTest extends QuicklensSuite {
  it should "modify an object declared using type alias" in {
    val s: S = State(0)
    val modified = s.modify(_.x).setTo(1)

    modified.x shouldBe 1
  }

  it should "modify a sealed hierarchy declared using type alias" in {
    val s: E = ListInt(0)
    val modified = s.modify(_.i).setTo(1)

    modified.i shouldBe 1
  }
}
