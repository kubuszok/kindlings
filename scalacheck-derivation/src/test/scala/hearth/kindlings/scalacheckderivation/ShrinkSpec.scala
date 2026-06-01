package hearth.kindlings.scalacheckderivation

import org.scalacheck.Shrink
import hearth.kindlings.scalacheckderivation.extensions.*

@scala.annotation.nowarn
class ShrinkSpec extends munit.FunSuite {

  test("derives Shrink for simple case class") {
    case class Point(x: Int, y: Int)

    val shrink: Shrink[Point] = DeriveShrink.derived[Point]
    val result = shrink.shrink(Point(10, 20))
    // Should produce at least some shrunk values
    val shrunkValues = result.take(20).toList
    assert(shrunkValues.nonEmpty, "Should produce shrunk values for Point(10, 20)")
    // Shrunk values should differ from original
    assert(shrunkValues.exists(_ != Point(10, 20)), "At least some shrunk values should differ from original")
  }

  test("derives Shrink for case class — shrinks fields independently") {
    case class Pair(a: Int, b: Int)

    val shrink: Shrink[Pair] = DeriveShrink.derived[Pair]
    val result = shrink.shrink(Pair(100, 200)).take(20).toList
    // Should include values where only a is shrunk and values where only b is shrunk
    val aShrunk = result.exists(p => p.a != 100 && p.b == 200)
    val bShrunk = result.exists(p => p.a == 100 && p.b != 200)
    assert(aShrunk || bShrunk, "Should shrink fields independently")
  }

  test("derives Shrink for zero-field case class") {
    case class Empty()

    val shrink: Shrink[Empty] = DeriveShrink.derived[Empty]
    val result = shrink.shrink(Empty())
    assert(result.isEmpty, "Zero-field case class should produce empty shrink stream")
  }

  test("derives Shrink for sealed trait") {
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Square(side: Double) extends Shape

    implicit val shrinkCircle: Shrink[Circle] = DeriveShrink.derived[Circle]
    implicit val shrinkSquare: Shrink[Square] = DeriveShrink.derived[Square]
    val shrink: Shrink[Shape] = DeriveShrink.derived[Shape]

    val result = shrink.shrink(Circle(10.0): Shape).take(10).toList
    assert(result.nonEmpty, "Should shrink Circle variant")
  }

  test("derives Shrink for singleton case object — empty stream") {
    sealed trait Direction
    case object Up extends Direction
    case object Down extends Direction

    val shrink: Shrink[Direction] = DeriveShrink.derived[Direction]
    val result = shrink.shrink(Up: Direction).take(5).toList
    // Case objects cannot be shrunk further
    assert(result.isEmpty, "Singleton should produce empty shrink stream")
  }

  test("derives Shrink for Option — Some shrinks to None and smaller values") {
    val shrink: Shrink[Option[Int]] = DeriveShrink.derived[Option[Int]]
    val result = shrink.shrink(Some(10)).take(20).toList
    assert(result.contains(None), "Shrinking Some should include None")
    assert(result.exists(_.isDefined), "Shrinking Some should include smaller Some values")
  }

  test("derives Shrink for Option — None produces empty stream") {
    val shrink: Shrink[Option[Int]] = DeriveShrink.derived[Option[Int]]
    val result = shrink.shrink(None).toList
    assert(result.isEmpty, "Shrinking None should produce empty stream")
  }

  test("derives Shrink for collection field") {
    case class Box(items: List[Int])

    val shrink: Shrink[Box] = DeriveShrink.derived[Box]
    val result = shrink.shrink(Box(List(1, 2, 3))).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values")
    // Should produce smaller lists
    assert(result.exists(_.items.size < 3), "Should produce shorter lists")
  }

  test("derives Shrink for recursive sealed trait (TreeNode)") {
    sealed trait TreeNode
    case class Branch(value: Int, children: List[TreeNode]) extends TreeNode
    case class Leaf(value: Int) extends TreeNode

    implicit val shrinkLeaf: Shrink[Leaf] = DeriveShrink.derived[Leaf]
    implicit val shrinkBranch: Shrink[Branch] = DeriveShrink.derived[Branch]
    val shrink: Shrink[TreeNode] = DeriveShrink.derived[TreeNode]

    val branch: TreeNode = Branch(10, List(Leaf(20), Leaf(30)))
    val result = shrink.shrink(branch).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values for a Branch")
    // Shrunk values should be simpler (e.g., smaller value, shorter children list, or Leaf nodes)
    val hasSmallerChildren = result.exists {
      case Branch(_, cs) => cs.size < 2
      case _             => false
    }
    val hasSmallerValue = result.exists {
      case Branch(v, _) => v < 10
      case _            => false
    }
    assert(hasSmallerChildren || hasSmallerValue, "Shrunk values should be simpler than original")
  }

  test("Shrink.derived extension syntax works") {
    case class Person(name: String, age: Int)

    val shrink: Shrink[Person] = Shrink.derived[Person]
    val result = shrink.shrink(Person("Alice", 30)).take(10).toList
    assert(result.nonEmpty, "Extension syntax should work")
  }

  test("Int shrinks toward zero") {
    val shrink: Shrink[Int] = DeriveShrink.derived[Int]
    val result = shrink.shrink(100).take(20).toList
    assert(result.nonEmpty, "Built-in Int should shrink")
    assert(result.contains(0) || result.exists(x => x.abs < 100), "Should shrink toward zero")
  }

  test("nested case class shrinks correctly") {
    case class Inner(value: Int)
    case class Outer(inner: Inner, flag: Boolean)

    implicit val shrinkInner: Shrink[Inner] = DeriveShrink.derived[Inner]
    val shrink: Shrink[Outer] = DeriveShrink.derived[Outer]
    val result = shrink.shrink(Outer(Inner(50), true)).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk nested values")
  }

  test("derives Shrink for case class with Map field") {
    case class Config(settings: Map[String, Int])

    val shrink: Shrink[Config] = DeriveShrink.derived[Config]
    val result = shrink.shrink(Config(Map("a" -> 1, "b" -> 2, "c" -> 3))).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values for Map field")
    assert(result.exists(_.settings.size < 3), "Should produce smaller maps")
  }

  test("derives Shrink for case class with Set field") {
    case class Tags(values: Set[Int])

    val shrink: Shrink[Tags] = DeriveShrink.derived[Tags]
    val result = shrink.shrink(Tags(Set(1, 2, 3))).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values for Set field")
    assert(result.exists(_.values.size < 3), "Should produce smaller sets")
  }

  test("derives Shrink for sealed trait — case objects appear in shrink stream") {
    sealed trait Status
    case object Active extends Status
    case class Suspended(reason: String) extends Status

    implicit val shrinkSuspended: Shrink[Suspended] = DeriveShrink.derived[Suspended]
    val shrink: Shrink[Status] = DeriveShrink.derived[Status]
    // Shrink a case class variant — should produce shrunk values
    val result = shrink.shrink(Suspended("timeout"): Status).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values for Suspended")
    // Shrink a case object — should produce empty stream (cannot shrink further)
    val caseObjResult = shrink.shrink(Active: Status).take(5).toList
    assert(caseObjResult.isEmpty, "Case object should produce empty shrink stream")
  }

  test("derives Shrink for case class with value class field") {
    import examples.WrappedId
    case class WithWrapper(w: WrappedId, extra: Int)

    val shrink: Shrink[WithWrapper] = DeriveShrink.derived[WithWrapper]
    val result = shrink.shrink(WithWrapper(WrappedId(100), 50)).take(20).toList
    assert(result.nonEmpty, "Should produce shrunk values for value class fields")
    // Value class field should be shrunk
    assert(
      result.exists(ww => ww.w.value != 100 || ww.extra != 50),
      "At least some shrunk values should differ from original"
    )
  }

  test("derives Shrink for case class with empty collection") {
    case class Box(items: List[Int])

    val shrink: Shrink[Box] = DeriveShrink.derived[Box]
    val result = shrink.shrink(Box(Nil)).take(5).toList
    assert(result.isEmpty, "Empty collection should produce empty shrink stream")
  }

  test("derives Shrink for case class with empty Map") {
    case class Config(settings: Map[String, Int])

    val shrink: Shrink[Config] = DeriveShrink.derived[Config]
    val result = shrink.shrink(Config(Map.empty)).take(5).toList
    assert(result.isEmpty, "Empty map should produce empty shrink stream")
  }

  test("derives Shrink for case class with empty Set") {
    case class Tags(values: Set[Int])

    val shrink: Shrink[Tags] = DeriveShrink.derived[Tags]
    val result = shrink.shrink(Tags(Set.empty)).take(5).toList
    assert(result.isEmpty, "Empty set should produce empty shrink stream")
  }
}
