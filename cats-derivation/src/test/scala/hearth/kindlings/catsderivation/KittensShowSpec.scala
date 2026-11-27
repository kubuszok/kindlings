package hearth.kindlings.catsderivation

import cats.Show
import cats.laws.discipline.SerializableTests
import hearth.kindlings.catsderivation.extensions.*

/** kittens' `ShowSuite` (`core/src/test/scala-3/cats/derived/ShowSuite.scala`, kittens 3.5.0): the rendered strings of
  * its data types, on Kindlings-derived instances.
  */
final class KittensShowSpec extends KittensLawsSuite {
  import KittensAdts.*
  import KittensShowSpec.*

  checkAll("Show is Serializable", SerializableTests.serializable(Show.derived[Foo]))

  test("Show[Foo]")(assertEquals(Show.derived[Foo].show(Foo(42, Option("Hello"))), "Foo(i = 42, b = Some(Hello))"))
  test("Show[Outer]")(assertEquals(Show.derived[Outer].show(Outer(Inner(3))), "Outer(in = Inner(i = 3))"))
  test("Show[IntTree]") {
    val value: IntTree = IntNode(IntLeaf(1), IntNode(IntNode(IntLeaf(2), IntLeaf(3)), IntLeaf(4)))
    assertEquals(
      Show.derived[IntTree].show(value),
      "IntNode(l = IntLeaf(t = 1), r = IntNode(l = IntNode(l = IntLeaf(t = 2), r = IntLeaf(t = 3)), r = IntLeaf(t = 4)))"
    )
  }
  test("Show[GenericAdt[Int]]") {
    val value: GenericAdt[Int] = GenericAdtCase(Some(1))
    assertEquals(Show.derived[GenericAdt[Int]].show(value), "GenericAdtCase(value = Some(1))")
  }
  test("Show[People] (respects Show[Address] in scope)") {
    val value = People("Kai", ContactInfo("303-123-4567", Address("123 1st St", "New York", "NY")))
    assertEquals(
      Show.derived[People].show(value),
      "People(name = Kai, contactInfo = ContactInfo(phoneNumber = 303-123-4567, address = 123 1st St New York NY))"
    )
  }
  test("Show[ListField]") {
    assertEquals(
      Show.derived[ListField].show(ListField("a", List(ListFieldChild(1)))),
      "ListField(a = a, b = List(ListFieldChild(c = 1)))"
    )
  }
  test("Show[Interleaved[Int]]") {
    assertEquals(
      Show.derived[Interleaved[Int]].show(Interleaved(1, 2, 3L, Vector(4, 5, 6), "789")),
      "Interleaved(i = 1, t = 2, l = 3, tt = Vector(4, 5, 6), s = 789)"
    )
  }
  test("Show[Tree[Int]]") {
    val value: Tree[Int] = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
    assertEquals(
      Show.derived[Tree[Int]].show(value),
      "Node(left = Leaf(value = 1), right = Node(left = Node(left = Leaf(value = 2), right = Leaf(value = 3)), right = Leaf(value = 4)))"
    )
  }
  test("Show[Singletons[Int]]") {
    assertEquals(
      Show.derived[Singletons[Int]].show(Singletons(313)),
      "Singletons(value = 313, str = Scala, lng = 42, dbl = 3.14)"
    )
  }
  test("Show[EnumK0]") {
    val value: EnumK0 = EnumK0.LeafI(3)
    assertEquals(Show.derived[EnumK0].show(value), "LeafI(value = 3)")
  }
  test("Show respects existing instances")(assertEquals(Show.derived[Box[Bogus]].show(Box(Bogus(42))), "Box(content = Blah)"))
}

object KittensShowSpec {
  import KittensAdts.*

  implicit val showAddress: Show[Address] = Show.show(a => List(a.street, a.city, a.state).mkString(" "))

  final case class Bogus(value: Int)
  object Bogus {
    implicit val show: Show[Bogus] = Show.show(_ => "Blah")
  }
}
