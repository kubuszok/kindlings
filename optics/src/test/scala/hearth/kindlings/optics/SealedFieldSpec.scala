package hearth.kindlings.optics

import hearth.MacroSuite

/** Direct field descent over a sealed hierarchy: a field declared on the trait (and present on every case) can be
  * modified with a plain `_.field` path — the macro generates the match over subtypes, no `.when[Sub]` needed.
  */
final class SealedFieldSpec extends MacroSuite {

  import SealedFieldSpec._
  import hearth.kindlings.optics._

  group("modify a common field across a sealed hierarchy") {

    test("a field on every subtype is modified directly (no `.when`)") {
      val dog: Animal = Dog("rex", 3)
      val cat: Animal = Cat("tom", indoor = true)
      dog.modify(_.name).using(_.toUpperCase) ==> Dog("REX", 3)
      cat.modify(_.name).using(_.toUpperCase) ==> Cat("TOM", indoor = true)
    }

    test("setTo on a sealed common field") {
      val a: Animal = Dog("rex", 3)
      a.modify(_.name).setTo("zed") ==> Dog("zed", 3)
    }

    test("descend further into a sealed common field whose type is a case class") {
      val s: Shape = Circle(Point(1, 2), 5)
      s.modify(_.center.x).using(_ + 10) ==> Circle(Point(11, 2), 5)
      val sq: Shape = Square(Point(4, 4), 2)
      sq.modify(_.center.y).setTo(0) ==> Square(Point(4, 0), 2)
    }

    test("a sealed field inside a case class") {
      val z = Zoo("z", Dog("rex", 3))
      z.modify(_.resident.name).using(_ + "!") ==> Zoo("z", Dog("rex!", 3))
    }

    test("a non-matching subtype keeps its own data") {
      // modifying `name` on a Cat must preserve the Cat-only `indoor` field
      val c: Animal = Cat("tom", indoor = true)
      c.modify(_.name).setTo("felix") ==> Cat("felix", indoor = true)
    }

    test("modifyAll over a sealed common field") {
      val a: Animal = Dog("rex", 3)
      a.modifyAll(_.name).using(_.toUpperCase) ==> Dog("REX", 3)
    }

    test("nested sealed hierarchy (trait of traits)") {
      val v: Vehicle = Car("ford", 4)
      v.modify(_.brand).using(_.toUpperCase) ==> Car("FORD", 4)
      val b: Vehicle = Bike("trek")
      b.modify(_.brand).setTo("giant") ==> Bike("giant")
    }
  }
}

object SealedFieldSpec {

  sealed trait Animal { def name: String }
  final case class Dog(name: String, age: Int) extends Animal
  final case class Cat(name: String, indoor: Boolean) extends Animal

  final case class Point(x: Int, y: Int)
  sealed trait Shape { def center: Point }
  final case class Circle(center: Point, radius: Int) extends Shape
  final case class Square(center: Point, side: Int) extends Shape

  final case class Zoo(label: String, resident: Animal)

  // A nested sealed hierarchy: Vehicle -> {Motorized -> Car, Bike}. `brand` is on every leaf.
  sealed trait Vehicle { def brand: String }
  sealed trait Motorized extends Vehicle
  final case class Car(brand: String, doors: Int) extends Motorized
  final case class Bike(brand: String) extends Vehicle
}
