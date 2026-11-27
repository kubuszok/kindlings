package hearth.kindlings.optics.upstream

import hearth.kindlings.optics._

// Ported from quicklens 1.9.15 (Apache License 2.0), scala-3/com/softwaremill/quicklens/test/ModifyAndTypeTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
import ModifyAndTypeTest._

object ModifyAndTypeTest {
  case class A(a: Int) extends B
  trait B {
    def a: Int
  }

  case class A1(a: Int)

  sealed trait T
  case class C(a: Int) extends T with B

  sealed trait T1
  case class C1(a: Int) extends T1
}

class ModifyAndTypeTest extends QuicklensSuite {
  it should "modify an & type object" in {
    val ab: A & B = A(0)

    val modified = ab.modify(_.a).setTo(1)

    modified.a shouldBe 1
  }

  it should "modify an & type object 1" in {
    val ab: B & A = A(0)

    val modified = ab.modify(_.a).setTo(1)

    modified.a shouldBe 1
  }

  it should "modify an & type object 2" in {
    val ab: B & A1 = new A1(0) with B

    val modified = ab.modify(_.a).setTo(1)

    modified.a shouldBe 1
  }

  it should "modify an & type object 3" in {
    val ab: A1 & B = new A1(0) with B

    val modified = ab.modify(_.a).setTo(1)

    modified.a shouldBe 1
  }

  // TODO this is an implemenation limitation for now, since anonymous classes crash on runtime
  // it should "modify an & type object with a sealed trait" in {
  //   val tb: T & B = C(0)

  //   val modified = tb.modify(_.a).setTo(1)

  //   modified.a shouldBe 1
  // }

  // it should "modify an & type object with a sealed trait 1" in {
  //   val tb: B & T = C(0)

  //   val modified = tb.modify(_.a).setTo(1)

  //   modified.a shouldBe 1
  // }

  // it should "modify an & type object with a sealed trait 2" in {
  //   val tb: B & T1 = new C1(0) with B

  //   val modified = tb.modify(_.a).setTo(1)

  //   modified.a shouldBe 1
  // }

  // it should "modify an & type object with a sealed trait 3" in {
  //   val tb: T1 & B = new C1(0) with B

  //   val modified = tb.modify(_.a).setTo(1)

  //   modified.a shouldBe 1
  // }
}
