package hearth.kindlings.optics

import hearth.MacroSuite

final class ModifyAllSpec extends MacroSuite {

  import ModifyAllSpec.*
  import hearth.kindlings.optics.syntax.*

  group("modifyAll over sibling fields (same focus type)") {

    test("using transforms every focused field") {
      Names("ann", "bob", "cid").modifyAll(_.first, _.second, _.third).using(_.toUpperCase) ==>
        Names("ANN", "BOB", "CID")
    }

    test("setTo replaces every focused field") {
      Names("ann", "bob", "cid").modifyAll(_.first, _.third).setTo("x") ==>
        Names("x", "bob", "x")
    }

    test("a single path behaves like modify") {
      Names("ann", "bob", "cid").modifyAll(_.second).using(_ + "!") ==> Names("ann", "bob!", "cid")
    }
  }

  group("modifyAll with a shared field prefix") {

    test("modifyAll(_.a.x, _.a.y) updates both leaves under a shared `a`") {
      val o = Outer(Inner("p", "q"), "top")
      o.modifyAll(_.inner.left, _.inner.right).using(_.toUpperCase) ==>
        Outer(Inner("P", "Q"), "top")
    }

    test("shared prefix mixed with a sibling at the root") {
      val o = Outer(Inner("p", "q"), "top")
      o.modifyAll(_.inner.left, _.top).setTo("Z") ==>
        Outer(Inner("Z", "q"), "Z")
    }
  }

  group("modifyAll combined with `.each`") {

    test("a `.each` path and a plain path together") {
      val box = Box(List("ann", "bob"), "label")
      box.modifyAll(_.xs.each, _.name).using(_.toUpperCase) ==>
        Box(List("ANN", "BOB"), "LABEL")
    }
  }

  group("modifyAll terminal operations") {

    test("usingIf and setToIf honour the condition") {
      Names("a", "b", "c").modifyAll(_.first, _.second).usingIf(true)(_ + "!") ==> Names("a!", "b!", "c")
      Names("a", "b", "c").modifyAll(_.first, _.second).usingIf(false)(_ + "!") ==> Names("a", "b", "c")
      Names("a", "b", "c").modifyAll(_.first, _.second).setToIf(true)("z") ==> Names("z", "z", "c")
    }
  }
}

object ModifyAllSpec {

  final case class Names(first: String, second: String, third: String)

  final case class Inner(left: String, right: String)
  final case class Outer(inner: Inner, top: String)

  final case class Box(xs: List[String], name: String)
}
