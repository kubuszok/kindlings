package hearth.kindlings.optics

import hearth.MacroSuite

final class AtIndexSpec extends MacroSuite {

  import AtIndexSpec.*
  import hearth.kindlings.optics.syntax.*

  group("`.at(i)` over a Seq") {

    test("present: transforms the element at the index") {
      val box = ListBox(List(Person("ann", 1), Person("bob", 2)))
      box.modify(_.people.at(1).name).using(_.toUpperCase) ==>
        ListBox(List(Person("ann", 1), Person("BOB", 2)))
    }

    test("absent: throws") {
      val box = ListBox(List(Person("ann", 1)))
      val _ = intercept[NoSuchElementException] {
        box.modify(_.people.at(5).name).using(_.toUpperCase)
      }
    }
  }

  group("`.at(key)` over a Map") {

    test("present: transforms the value at the key") {
      val box = MapBox(Map("a" -> 1, "b" -> 2))
      box.modify(_.m.at("b")).using(_ + 100) ==> MapBox(Map("a" -> 1, "b" -> 102))
    }

    test("absent: throws") {
      val box = MapBox(Map("a" -> 1))
      val _ = intercept[NoSuchElementException] {
        box.modify(_.m.at("zzz")).using(_ + 1)
      }
    }
  }

  group("`.index(i)` over a Seq / Map") {

    test("Seq present: transforms") {
      val box = ListInts(List(1, 2, 3))
      box.modify(_.xs.index(0)).using(_ + 10) ==> ListInts(List(11, 2, 3))
    }

    test("Seq absent: no-op") {
      val box = ListInts(List(1, 2, 3))
      box.modify(_.xs.index(99)).using(_ + 10) ==> ListInts(List(1, 2, 3))
    }

    test("Map present: transforms") {
      val box = MapBox(Map("a" -> 1))
      box.modify(_.m.index("a")).using(_ + 5) ==> MapBox(Map("a" -> 6))
    }

    test("Map absent: no-op") {
      val box = MapBox(Map("a" -> 1))
      box.modify(_.m.index("zzz")).using(_ + 5) ==> MapBox(Map("a" -> 1))
    }
  }

  group("`.atOrElse(i, default)`") {

    test("Map present: transforms existing value") {
      val box = MapBox(Map("a" -> 1))
      box.modify(_.m.atOrElse("a", 0)).using(_ + 10) ==> MapBox(Map("a" -> 11))
    }

    test("Map absent: inserts default then transforms") {
      val box = MapBox(Map("a" -> 1))
      box.modify(_.m.atOrElse("b", 100)).using(_ + 1) ==> MapBox(Map("a" -> 1, "b" -> 101))
    }

    test("Seq absent: inserts default (grows the seq)") {
      val box = ListInts(List(1, 2))
      box.modify(_.xs.atOrElse(2, 99)).using(_ + 1).xs ==> List(1, 2, 100)
    }
  }

  group("`.at`/`.index`/`.atOrElse` over Option") {

    test("at Some: transforms the contained value") {
      val box = OptBox(Some(Person("ann", 1)))
      box.modify(_.maybe.at.name).using(_.toUpperCase) ==> OptBox(Some(Person("ANN", 1)))
    }

    test("at None: throws") {
      val box = OptBox(None)
      val _ = intercept[NoSuchElementException] {
        box.modify(_.maybe.at.name).using(_.toUpperCase)
      }
    }

    test("index None: no-op") {
      val box = OptInt(None)
      box.modify(_.x.index).using(_ + 1) ==> OptInt(None)
    }

    test("index Some: transforms") {
      val box = OptInt(Some(5))
      box.modify(_.x.index).using(_ + 1) ==> OptInt(Some(6))
    }

    test("atOrElse None: inserts default") {
      val box = OptInt(None)
      box.modify(_.x.atOrElse(7)).using(_ + 1) ==> OptInt(Some(8))
    }

    test("atOrElse Some: transforms existing") {
      val box = OptInt(Some(5))
      box.modify(_.x.atOrElse(7)).using(_ + 1) ==> OptInt(Some(6))
    }
  }

  group("interleaving `.each` and `.at`") {

    test("`_.xs.each` then field, plus a `.at` elsewhere") {
      val box = ListInts(List(1, 2, 3))
      box.modify(_.xs.at(1)).setTo(20) ==> ListInts(List(1, 20, 3))
    }
  }
}

object AtIndexSpec {

  final case class Person(name: String, age: Int)

  final case class ListBox(people: List[Person])
  final case class ListInts(xs: List[Int])
  final case class MapBox(m: Map[String, Int])
  final case class OptBox(maybe: Option[Person])
  final case class OptInt(x: Option[Int])
}
