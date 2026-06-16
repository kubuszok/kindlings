package hearth.kindlings.optics

import hearth.MacroSuite

final class WhenSpec extends MacroSuite {

  import WhenSpec.*
  import hearth.kindlings.optics.syntax.*

  group("`.when[Subtype]` prism") {

    test("focus IS the subtype: transforms its field") {
      val box: Box = Box(Dog("rex", 3))
      box.modify(_.animal.when[Dog].name).using(_.toUpperCase) ==> Box(Dog("REX", 3))
    }

    test("focus is a DIFFERENT subtype: left unchanged") {
      val box: Box = Box(Cat("tom", indoor = true))
      box.modify(_.animal.when[Dog].name).using(_.toUpperCase) ==> Box(Cat("tom", indoor = true))
    }

    test("root `.when` (focus is the sealed trait directly)") {
      val animal: Animal = Dog("rex", 3)
      animal.modify(_.when[Dog].age).using(_ + 1) ==> Dog("rex", 4)
    }

    test("root `.when` on a non-matching subtype: unchanged") {
      val animal: Animal = Cat("tom", indoor = true)
      animal.modify(_.when[Dog].age).using(_ + 1) ==> Cat("tom", indoor = true)
    }
  }

  group("interleaving `.when` with fields and `.each`") {

    test("`_.field.when[Sub].subfield`") {
      val kennel = Kennel("home", Dog("rex", 3))
      kennel.modify(_.resident.when[Dog].name).using(_ + "!") ==> Kennel("home", Dog("rex!", 3))
    }

    test("`_.xs.each.when[Sub].field`: only matching elements change") {
      val zoo = Zoo(List(Dog("rex", 3), Cat("tom", indoor = true), Dog("fido", 1)))
      zoo.modify(_.animals.each.when[Dog].name).using(_.toUpperCase) ==>
        Zoo(List(Dog("REX", 3), Cat("tom", indoor = true), Dog("FIDO", 1)))
    }
  }
}

object WhenSpec {

  sealed trait Animal
  final case class Dog(name: String, age: Int) extends Animal
  final case class Cat(name: String, indoor: Boolean) extends Animal

  final case class Box(animal: Animal)
  final case class Kennel(label: String, resident: Animal)
  final case class Zoo(animals: List[Animal])
}
