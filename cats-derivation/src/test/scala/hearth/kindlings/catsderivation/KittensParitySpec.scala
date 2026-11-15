package hearth.kindlings.catsderivation

import cats.{Eval, Foldable, Functor, MonoidK, SemigroupK, Traverse}
import cats.kernel.PartialOrder
import alleycats.{Empty, EmptyK}
import hearth.MacroSuite

/** Behaviour aligned with kittens (the upstream oracle in `upstream-oracle-tests` compares the derivations directly).
  */
object KittensParitySpec {
  final case class KpPartial(set: Set[Int], n: Int)
  sealed trait KpShape
  final case class KpCircle(set: Set[Int]) extends KpShape
  final case class KpSquare(n: Int) extends KpShape
  case object KpOrigin extends KpShape

  sealed trait KpMaybe
  case object KpNothing extends KpMaybe
  final case class KpJust(id: java.util.UUID) extends KpMaybe

  final case class KpNested[A](items: List[Option[A]], head: A, more: Vector[List[A]])

  sealed trait KpList[A]
  final case class KpNil[A]() extends KpList[A]
  final case class KpCons[A](head: A, tail: KpList[A]) extends KpList[A]

  final case class KpLists[A](list: List[A], opt: Option[A])
}

final class KittensParitySpec extends MacroSuite {
  import KittensParitySpec.*
  import hearth.kindlings.catsderivation.extensions.*

  test("PartialOrder: fields with only a PartialOrder, NaN for different subtypes") {
    val partial: PartialOrder[KpPartial] = PartialOrder.derived[KpPartial]
    assertEquals(partial.partialCompare(KpPartial(Set(1), 1), KpPartial(Set(1, 2), 0)), -1.0)
    assert(partial.partialCompare(KpPartial(Set(1), 1), KpPartial(Set(2), 0)).isNaN)
    assertEquals(partial.partialCompare(KpPartial(Set(1), 1), KpPartial(Set(1), 2)), -1.0)

    val shape: PartialOrder[KpShape] = PartialOrder.derived[KpShape]
    assert(shape.partialCompare(KpCircle(Set(1)), KpSquare(1)).isNaN)
    assert(shape.partialCompare(KpSquare(1), KpOrigin).isNaN)
    assertEquals(shape.partialCompare(KpOrigin, KpOrigin), 0.0)
    assertEquals(shape.partialCompare(KpSquare(1), KpSquare(2)), -1.0)
  }

  test("Empty: the single subtype forming one (a case object here), without implicit instances") {
    val empty: Empty[KpMaybe] = Empty.derived[KpMaybe]
    assertEquals(empty.empty, KpNothing)
  }

  test("Functor, Foldable, Traverse: fields nesting several type constructors, lazy foldRight") {
    val value = KpNested(List(Some(1), None, Some(2)), 3, Vector(List(4, 5), Nil))
    val functor: Functor[KpNested] = Functor.derived[KpNested]
    assertEquals(functor.map(value)(_ * 10), KpNested(List(Some(10), None, Some(20)), 30, Vector(List(40, 50), Nil)))

    val foldable: Foldable[KpNested] = Foldable.derived[KpNested]
    assertEquals(foldable.toList(value), List(1, 2, 3, 4, 5))
    var visited = 0
    val found = foldable.foldRight(value, Eval.now(false)) { (a, rest) =>
      visited += 1
      if (a >= 2) Eval.now(true) else rest
    }
    assert(found.value)
    assertEquals(visited, 2)

    val traverse: Traverse[KpNested] = Traverse.derived[KpNested]
    assertEquals(traverse.traverse(value)(a => Option(a.toString)).map(_.head), Some("3"))
    assertEquals(traverse.traverse(value)(a => if (a == 5) None else Some(a)), None)
  }

  test("Functor, Foldable, Traverse: recursive sealed traits without an implicit instance in scope") {
    val list: KpList[Int] = KpCons(1, KpCons(2, KpNil()))
    assertEquals(Functor.derived[KpList].map(list)(_ + 1), KpCons(2, KpCons(3, KpNil[Int]())): KpList[Int])
    assertEquals(Foldable.derived[KpList].toList(list), List(1, 2))
    assertEquals(Traverse.derived[KpList].traverse(list)(Option(_)), Some(list))
  }

  test("SemigroupK, MonoidK, EmptyK: fields G[A] use SemigroupK[G] / MonoidK[G] / EmptyK[G]") {
    val x = KpLists(List(1), Option.empty[Int])
    val y = KpLists(List(2), Some(2))
    val z = KpLists(List.empty[Int], Some(3))
    val semigroupK: SemigroupK[KpLists] = SemigroupK.derived[KpLists]
    assertEquals(semigroupK.combineK(x, y), KpLists(List(1, 2), Some(2)))
    assertEquals(semigroupK.combineK(y, z), KpLists(List(2), Some(2))) // Option's orElse, not Semigroup[Option]
    val monoidK: MonoidK[KpLists] = MonoidK.derived[KpLists]
    assertEquals(monoidK.empty[Int], KpLists(List.empty[Int], Option.empty[Int]))
    assertEquals(monoidK.combineK(z, y), KpLists(List(2), Some(3)))
    import alleycats.std.all.*
    val emptyK: EmptyK[KpLists] = EmptyK.derived[KpLists]
    assertEquals(emptyK.empty[Int], KpLists(List.empty[Int], Option.empty[Int]))
  }
}
