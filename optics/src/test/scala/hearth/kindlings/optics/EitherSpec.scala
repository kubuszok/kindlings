package hearth.kindlings.optics

import hearth.MacroSuite

final class EitherSpec extends MacroSuite {

  import EitherSpec.*
  import hearth.kindlings.optics.syntax.*

  group("`.eachRight`") {

    test("Right: transforms the right branch") {
      val box = Box(Right(Person("ann", 1)))
      box.modify(_.e.eachRight.name).using(_.toUpperCase) ==> Box(Right(Person("ANN", 1)))
    }

    test("Left: leaves a Left untouched") {
      val box = Box(Left("err"))
      box.modify(_.e.eachRight.name).using(_.toUpperCase) ==> Box(Left("err"))
    }
  }

  group("`.eachLeft`") {

    test("Left: transforms the left branch") {
      val box = Box(Left("err"))
      box.modify(_.e.eachLeft).using(_.toUpperCase) ==> Box(Left("ERR"))
    }

    test("Right: leaves a Right untouched") {
      val box = Box(Right(Person("ann", 1)))
      box.modify(_.e.eachLeft).using(_.toUpperCase) ==> Box(Right(Person("ann", 1)))
    }
  }

  group("`.eachRight` on a primitive right branch") {

    test("transforms an Int right branch") {
      val box = IntBox(Right(5))
      box.modify(_.e.eachRight).using(_ + 100) ==> IntBox(Right(105))
    }
  }

  group("interleaving `.each` and Either") {

    test("`_.xs.each.e.eachRight.name`") {
      val box = ListBox(List(Box(Right(Person("ann", 1))), Box(Left("x"))))
      box.modify(_.xs.each.e.eachRight.name).using(_.toUpperCase) ==>
        ListBox(List(Box(Right(Person("ANN", 1))), Box(Left("x"))))
    }
  }
}

object EitherSpec {

  final case class Person(name: String, age: Int)

  final case class Box(e: Either[String, Person])
  final case class IntBox(e: Either[String, Int])
  final case class ListBox(xs: List[Box])
}
