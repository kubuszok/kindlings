package hearth.kindlings.optics

import hearth.MacroSuite

/** Scala 3 `enum` direct field descent — a field common to every case (here via a shared parent trait that each case
  * implements) can be modified with a plain `_.field` path, just like a sealed trait. Lives in `src/test/scala-3`
  * because `enum` is Scala-3-only syntax.
  */
final class EnumFieldSpec extends MacroSuite {

  import EnumFieldSpec.*
  import hearth.kindlings.optics.*

  group("modify a common field across a Scala 3 enum") {

    test("a field on every enum case is modified directly (no `.when`)") {
      val dog: Pet = Pet.Dog("rex", 3)
      dog.modify(_.name).using(_.toUpperCase) ==> Pet.Dog("REX", 3)
      val cat: Pet = Pet.Cat("tom", indoor = true)
      cat.modify(_.name).setTo("felix") ==> Pet.Cat("felix", indoor = true)
    }

    test("descend further into an enum common field whose type is a case class") {
      val n: Node = Node.Branch(Meta("a"), 2)
      n.modify(_.meta.label).using(_.toUpperCase) ==> Node.Branch(Meta("A"), 2)
    }
  }
}

object EnumFieldSpec {

  sealed trait Named { def name: String }
  enum Pet extends Named {
    case Dog(name: String, age: Int)
    case Cat(name: String, indoor: Boolean)
  }

  final case class Meta(label: String)
  sealed trait HasMeta { def meta: Meta }
  enum Node extends HasMeta {
    case Leaf(meta: Meta)
    case Branch(meta: Meta, children: Int)
  }
}
