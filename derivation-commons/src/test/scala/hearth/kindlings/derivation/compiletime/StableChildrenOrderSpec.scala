package hearth.kindlings.derivation.compiletime

final class StableChildrenOrderSpec extends hearth.MacroSuite {

  private def inOrderOf(items: String*)(reference: String*): List[String] =
    StableChildrenOrder.inOrderOf(items.toList, reference.toList)(_ == _)

  group("StableChildrenOrder.inOrderOf") {

    test("puts the items in the order of the reference") {
      inOrderOf("Red", "Green", "Blue")("Blue", "Green", "Red") ==> List("Blue", "Green", "Red")
    }

    test("ignores reference entries without an item (e.g. intermediate sealed traits around leaves)") {
      inOrderOf("Leaf2", "Leaf1", "Other")("Branch", "Leaf1", "Leaf2", "Other") ==> List("Leaf1", "Leaf2", "Other")
    }

    test("puts items missing from the reference last, in their original order") {
      inOrderOf("X", "B", "Y", "A")("A", "B") ==> List("A", "B", "X", "Y")
    }

    test("uses the first matching position") {
      StableChildrenOrder.inOrderOf(List("a1", "b1"), List("b", "a", "b"))((item, ref) => item.startsWith(ref)) ==>
        List("b1", "a1")
    }

    test("keeps an empty list empty") {
      inOrderOf()("A") ==> Nil
    }
  }
}
