package hearth.kindlings.optics

import hearth.MacroSuite

final class EachSpec extends MacroSuite {

  import EachSpec.*
  import hearth.kindlings.optics.syntax.*

  group("`.each` over List/Vector/Seq of case classes") {

    test("List: transforms every element, siblings untouched") {
      val team = Team("devs", List(Person("ann", 1), Person("bob", 2)))
      team.modify(_.people.each.name).using(_.toUpperCase) ==>
        Team("devs", List(Person("ANN", 1), Person("BOB", 2)))
    }

    test("Vector: setTo replaces every focused field") {
      val box = VectorBox(Vector(Person("ann", 1), Person("bob", 2)))
      box.modify(_.people.each.age).setTo(0) ==>
        VectorBox(Vector(Person("ann", 0), Person("bob", 0)))
    }

    test("Seq: using transforms every element") {
      val box = SeqBox(Seq(Person("ann", 1), Person("bob", 2)))
      box.modify(_.people.each.age).using(_ + 10) ==>
        SeqBox(Seq(Person("ann", 11), Person("bob", 12)))
    }
  }

  group("`.each` over Option") {

    test("Some: transforms the contained field") {
      val box = OptBox(Some(Person("ann", 1)))
      box.modify(_.maybe.each.name).setTo("zed") ==> OptBox(Some(Person("zed", 1)))
    }

    test("None: leaves the container unchanged") {
      val box = OptBox(None)
      box.modify(_.maybe.each.name).setTo("zed") ==> OptBox(None)
    }
  }

  group("interleaving fields and `.each`") {

    test("field after `.each` (`_.xs.each.field`)") {
      val nested = Nested(Inner(List(Person("ann", 1), Person("bob", 2))))
      nested.modify(_.inner.people.each.name).using(_ + "!") ==>
        Nested(Inner(List(Person("ann!", 1), Person("bob!", 2))))
    }

    test("leaf `.each` over a primitive element") {
      val box = IntsBox(List(1, 2, 3))
      box.modify(_.xs.each).using(_ + 1) ==> IntsBox(List(2, 3, 4))
    }
  }

  group("`.each` over Set and Array") {

    test("Set: transforms every element") {
      val box = SetBox(Set(1, 2, 3))
      box.modify(_.xs.each).using(_ * 10) ==> SetBox(Set(10, 20, 30))
    }

    test("Array: transforms every element (needs ClassTag)") {
      val box = ArrBox(Array(1, 2, 3))
      box.modify(_.xs.each).using(_ + 1).xs.toList ==> List(2, 3, 4)
    }
  }

  group("`.each` over Map values") {

    test("Map: transforms every value, keys untouched") {
      val box = MapBox(Map("a" -> 1, "b" -> 2))
      box.modify(_.m.each).using(_ + 100) ==> MapBox(Map("a" -> 101, "b" -> 102))
    }

    test("Map of case classes: transforms a field of every value") {
      val box = MapPeople(Map("a" -> Person("ann", 1), "b" -> Person("bob", 2)))
      box.modify(_.m.each.name).using(_.toUpperCase) ==>
        MapPeople(Map("a" -> Person("ANN", 1), "b" -> Person("BOB", 2)))
    }
  }

  group("`.eachWhere`") {

    test("transforms only matching elements") {
      val box = IntsBox(List(1, 2, 3, 4))
      box.modify(_.xs.eachWhere(_ % 2 == 0)).using(_ * 100) ==> IntsBox(List(1, 200, 3, 400))
    }
  }

  group("custom user functor") {

    test("a user-defined QuicklensFunctor makes `.each` work for a custom container") {
      import EachSpec.customFunctor
      val box = BoxOfBox(MyBox(Person("ann", 1)))
      box.modify(_.b.each.name).setTo("zed") ==> BoxOfBox(MyBox(Person("zed", 1)))
    }
  }
}

object EachSpec {

  final case class Person(name: String, age: Int)

  final case class Team(label: String, people: List[Person])
  final case class VectorBox(people: Vector[Person])
  final case class SeqBox(people: Seq[Person])
  final case class OptBox(maybe: Option[Person])

  final case class Inner(people: List[Person])
  final case class Nested(inner: Inner)

  final case class IntsBox(xs: List[Int])
  final case class SetBox(xs: Set[Int])
  final case class ArrBox(xs: Array[Int])

  final case class MapBox(m: Map[String, Int])
  final case class MapPeople(m: Map[String, Person])

  // A custom unary container with a user-provided QuicklensFunctor, to prove extensibility.
  final case class MyBox[A](value: A)
  final case class BoxOfBox(b: MyBox[Person])

  implicit val customFunctor: QuicklensFunctor[MyBox] = new QuicklensFunctor[MyBox] {
    def each[A, B](fa: MyBox[A])(f: A => B): MyBox[B] = MyBox(f(fa.value))
  }
}
