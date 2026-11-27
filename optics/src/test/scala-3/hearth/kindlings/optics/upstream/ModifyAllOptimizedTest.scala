package hearth.kindlings.optics.upstream

import hearth.kindlings.optics.*

// Ported from quicklens 1.9.15 (Apache License 2.0), scala-3/com/softwaremill/quicklens/test/ModifyAllOptimizedTest.scala.
// Only the imports and the test framework differ, see [[QuicklensSuite]].
@scala.annotation.nowarn // upstream's tests discard values and keep unused definitions
class ModifyAllOptimizedTest extends QuicklensSuite {
  import ModifyAllOptimizedTest.*

  override def beforeEach(context: BeforeEach): Unit =
    Cons.copyCount = 0

  it should "Have a correct number of copy calls with single focus" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head
      )
      .using(_ + 1)

    Cons.copyCount should be(1)
  }

  it should "optimize number of copy calls 1" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head,
        _.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(2)
  }

  it should "optimize number of copy calls 2" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.head,
        _.tail.each.head,
        _.tail.each.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(3)
  }

  it should "optimize number of copy calls 3" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail.each.tail.each.head,
        _.tail.each.tail.each.tail.each.head
      )
      .using(_ + 1)

    Cons.copyCount should be(4)
  }

  it should "optimize number of copy calls 4" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail,
        _.tail,
        _.tail.each.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(3)
  }

  it should "optimize number of copy calls 5" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail,
        _.tail.each.tail,
        _.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(2)
  }

  it should "optimize number of copy calls 6" in {
    val lst: Cons = Cons.fromList(List(1, 2, 3, 4, 5, 6)).get

    lst
      .modifyAll(
        _.tail,
        _.tail.each.tail.each.tail,
        _.tail.each.tail,
        _.tail.each.tail.each.tail.each.tail,
        _.tail.each.tail,
        _.tail
      )
      .using {
        case None                   => None
        case Some(Cons(head, tail)) => Some(Cons(head + 1, tail))
      }

    Cons.copyCount should be(5)
  }

  // Not ported: "optimize number of each function delegates 1..3" count the calls of a user-defined
  // `QuicklensFunctor[Opt]`; Kindlings has no such type class (containers come from Hearth's providers). That paths
  // sharing an `.each` traverse the container once is what "optimize number of copy calls 2..6" check: an element
  // reached twice through the same `.each` is copied once.
}

object ModifyAllOptimizedTest {

  case class Cons(head: Int, tail: Option[Cons]) {
    def copy(head: Int = head, tail: Option[Cons] = tail): Cons = {
      Cons.copyCount = Cons.copyCount + 1
      Cons(head, tail)
    }
  }
  object Cons {
    var copyCount = 0
    def fromList(list: List[Int]): Option[Cons] = list match {
      case Nil          => None
      case head :: tail =>
        Some(Cons(head, fromList(tail)))
    }
  }

}
