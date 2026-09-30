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

  test("nested case class shrinks without a Shrink for it in scope (issue #219)") {
    case class Inner(value: Int)
    case class Outer(inner: Inner, n: Int)

    val outer = Outer(Inner(50), 100)
    val result = DeriveShrink.derived[Outer].shrink(outer).take(200).toList
    assert(result.exists(_.inner != Inner(50)), "Should shrink the nested case class")
    assert(result.exists(_.n != 100), "Should shrink the Int field")
  }

  test(
    "nested case class inside a collection/Option/Either/tuple shrinks without a Shrink for it in scope (issue #219)"
  ) {
    case class Inner(value: Int)
    case class Outer(inners: Vector[Inner], opt: Option[Inner], either: Either[String, Inner], tuple: (Inner, Int))

    val outer = Outer(Vector(Inner(50)), Some(Inner(50)), Right(Inner(50)), (Inner(50), 1))
    val result = DeriveShrink.derived[Outer].shrink(outer).take(500).toList
    assert(result.exists(_.inners.exists(_ != Inner(50))), "Should shrink the nested case class inside a Vector")
    assert(result.exists(_.opt.exists(_ != Inner(50))), "Should shrink the nested case class inside an Option")
    assert(result.exists(_.either.exists(_ != Inner(50))), "Should shrink the nested case class inside an Either")
    assert(result.exists(_.tuple._1 != Inner(50)), "Should shrink the nested case class inside a tuple")
  }

  test("recursive types shrink structurally without any Shrink in scope (issue #219)") {
    sealed trait Tree
    case class Node(value: Int, children: List[Tree], next: Option[Tree]) extends Tree
    case class Tip(value: Int) extends Tree

    val tree: Tree = Node(10, List(Tip(20), Node(30, Nil, Some(Tip(40)))), None)
    val result = DeriveShrink.derived[Tree].shrink(tree).take(500).toList
    assert(result.nonEmpty)
    assert(
      result.exists {
        case Node(_, List(Tip(v), _), _) => v != 20
        case _                           => false
      },
      "Should shrink nodes nested inside the List"
    )
  }

  test("ScalaCheck's leaf instances are still used for nested fields (issue #219)") {
    import scala.concurrent.duration.*
    case class Timed(d: Duration, fd: FiniteDuration, s: String, n: Long, x: Double)

    val result = DeriveShrink.derived[Timed].shrink(Timed(10.seconds, 20.seconds, "abc", 100L, 1.5)).take(500).toList
    assert(result.exists(_.d != 10.seconds), "Duration should be shrunk by ScalaCheck's shrinkDuration")
    assert(result.exists(_.fd != 20.seconds), "FiniteDuration should be shrunk by ScalaCheck's shrinkFiniteDuration")
    assert(result.exists(_.s != "abc"), "String should be shrunk by ScalaCheck's shrinkString")
    assert(result.exists(_.n != 100L), "Long should be shrunk by ScalaCheck's shrinkIntegral")
    assert(result.exists(_.x != 1.5), "Double should be shrunk by ScalaCheck's shrinkFractional")
  }

  test("sealed trait: a value is shrunk only by its own case's Shrink") {
    // Previously every case's Shrink was tried, so `B(100)` also yielded same-arity `C`s built from its fields.
    val result = DeriveShrink.derived[ShrinkSpec.Letter].shrink(ShrinkSpec.B(100)).take(100).toList
    assert(result.nonEmpty)
    assert(result.forall(_.isInstanceOf[ShrinkSpec.B]), s"Only B values expected, got: $result")
  }

  test("an explicit Shrink for a nested type still takes precedence, even a no-op one (issue #219)") {
    case class Inner(value: Int)
    case class Outer(inner: Inner, n: Int)

    implicit val noShrinkInner: Shrink[Inner] = Shrink.shrinkAny[Inner]
    val result = DeriveShrink.derived[Outer].shrink(Outer(Inner(50), 100)).take(200).toList
    assert(result.nonEmpty)
    assert(result.forall(_.inner == Inner(50)), "The explicit no-op Shrink[Inner] must be used")
  }

  test("a nested type no rule can handle still falls back to ScalaCheck's shrinkAny (issue #219)") {
    final class Opaque(val value: Int)
    case class Outer(opaque: Opaque, n: Int)

    val opaque = new Opaque(1)
    val result = DeriveShrink.derived[Outer].shrink(Outer(opaque, 100)).take(200).toList
    assert(result.nonEmpty)
    assert(result.forall(_.opaque eq opaque))
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

object ShrinkSpec {
  sealed trait Letter
  case object A extends Letter
  final case class B(value: Int) extends Letter
  final case class C(value: Int) extends Letter
}
