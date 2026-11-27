package hearth.kindlings.optics.upstream

import hearth.kindlings.optics._

// Ported from quicklens 1.9.15 (Apache License 2.0), scala/com/softwaremill/quicklens/RepeatedModifyAllTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
class RepeatedModifyAllTest extends QuicklensSuite {
  import RepeatedModifyAllTest._

  it should "expand a very long repeated function" in {
    val c6 = C6(1)
    val c5 = C5(c6)
    val c4 = C4(c5)
    val c3 = C3(c4)
    val c2 = C2(c3)
    val c1 = C1(c2)

    val c6e = C6(2)
    val c5e = C5(c6e)
    val c4e = C4(c5e)
    val c3e = C3(c4e)
    val c2e = C2(c3e)
    val c1e = C1(c2e)

    val res = c1
      .modifyAll(
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a
      )
      .setTo(2)
    res should be(c1e)
  }

  it should "expand a very long repeated function correct number of times" in {
    val c6 = C6(1)
    val c5 = C5(c6)
    val c4 = C4(c5)
    val c3 = C3(c4)
    val c2 = C2(c3)
    val c1 = C1(c2)

    val c6e = C6(11)
    val c5e = C5(c6e)
    val c4e = C4(c5e)
    val c3e = C3(c4e)
    val c2e = C2(c3e)
    val c1e = C1(c2e)

    val res = c1
      .modifyAll(
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a,
        _.a.a.a.a.a.a
      )
      .using(_ + 1)
    res should be(c1e)
  }
}

object RepeatedModifyAllTest {
  case class C1(
      a: C2
  )

  case class C2(
      a: C3
  )

  case class C3(
      a: C4
  )

  case class C4(
      a: C5
  )

  case class C5(
      a: C6
  )

  case class C6(
      a: Int
  )
}
