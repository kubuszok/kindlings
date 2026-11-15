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

  final case class KpNel[A](head: A, tail: List[A])
  final case class KpRose[A](value: A, children: List[KpRose[A]])
  final case class KpDeep[A](items: List[Option[A]], head: A, label: String)
  final case class KpWrap[A](a: Option[A], b: Vector[A])
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

  test("Reducible and NonEmptyTraverse: fields wrapping the type parameter (NonEmptyList-like, rose tree, nested)") {
    import cats.{NonEmptyTraverse, Reducible}
    import cats.data.State
    val nel = KpNel(1, List(2, 3))
    val reducible: Reducible[KpNel] = Reducible.derived[KpNel]
    assertEquals(reducible.reduceLeft(nel)(_ * 10 + _), 123)
    assertEquals(reducible.reduceRight(nel)((a, eb) => eb.map(a * 10 + _)).value, 33) // 1 * 10 + (2 * 10 + 3)
    val net: NonEmptyTraverse[KpNel] = NonEmptyTraverse.derived[KpNel]
    type Log[X] = State[List[Int], X]
    val logged = net.nonEmptyTraverse[Log, Int, String](nel)(a => State((l: List[Int]) => (l :+ a, a.toString)))
    assertEquals(logged.run(Nil).value, (List(1, 2, 3), KpNel("1", List("2", "3"))))
    assertEquals(net.nonEmptyTraverse(nel)(a => if (a > 2) None else Some(a)), None)

    val rose = KpRose(1, List(KpRose(2, Nil), KpRose(3, List(KpRose(4, Nil)))))
    val roseReducible: Reducible[KpRose] = Reducible.derived[KpRose]
    val roseTraverse: NonEmptyTraverse[KpRose] = NonEmptyTraverse.derived[KpRose]
    assertEquals(roseReducible.toList(rose), List(1, 2, 3, 4))
    assertEquals(roseTraverse.traverse(rose)(a => Option(a * 2)).map(roseReducible.toList(_)), Some(List(2, 4, 6, 8)))

    val deep = KpDeep(List(Some(1), None, Some(2)), 3, "x")
    assertEquals(Reducible.derived[KpDeep].reduceLeft(deep)(_ * 10 + _), 123)
  }

  test("Apply, Applicative, Pure: fields wrapping the type parameter, constants combined with their instances") {
    import cats.{Applicative, Apply}
    import alleycats.Pure
    val apply: Apply[KpNel] = Apply.derived[KpNel]
    assertEquals(apply.map2(KpNel(1, List(1, 2)), KpNel(10, List(10, 20)))(_ + _), KpNel(11, List(11, 21, 12, 22)))
    val applicative: Applicative[KpDeep] = Applicative.derived[KpDeep]
    assertEquals(applicative.pure(5), KpDeep(List(Some(5)), 5, ""))
    assertEquals(
      applicative.map2(KpDeep(List(Some(1)), 1, "a"), KpDeep(List(Some(2), None), 2, "b"))(_ + _),
      KpDeep(List(Some(3), None), 3, "ab")
    )
    val pure: Pure[KpWrap] = Pure.derived[KpWrap]
    assertEquals(pure.pure("x"), KpWrap(Some("x"), Vector("x")))
    // Recursive types too (kittens' derived Apply overflows the stack for this one).
    val rose: Apply[KpRose] = Apply.derived[KpRose]
    assertEquals(
      rose.map2(KpRose(1, List(KpRose(2, Nil))), KpRose(10, Nil))(_ + _),
      KpRose(11, List.empty[KpRose[Int]])
    )
  }

  test("NonEmptyAlternative and Alternative: every field wraps the type parameter") {
    import cats.{Alternative, NonEmptyAlternative}
    val nea: NonEmptyAlternative[KpLists] = NonEmptyAlternative.derived[KpLists]
    assertEquals(nea.combineK(KpLists(List(1), None), KpLists(List(2), Some(2))), KpLists(List(1, 2), Some(2)))
    assertEquals(nea.pure(3), KpLists(List(3), Some(3)))
    val alternative: Alternative[KpLists] = Alternative.derived[KpLists]
    assertEquals(alternative.empty[Int], KpLists(List.empty[Int], Option.empty[Int]))
  }

  test("shapes kittens rejects are rejected with the reason") {
    compileErrors(
      """
      import hearth.kindlings.catsderivation.extensions.*
      cats.Reducible.derived[hearth.kindlings.catsderivation.KittensParitySpec.KpWrap]
      """
    ).check("Macro derivation failed", "no field guarantees an element")
    compileErrors(
      """
      import hearth.kindlings.catsderivation.extensions.*
      cats.NonEmptyAlternative.derived[hearth.kindlings.catsderivation.KittensParitySpec.KpNel]
      """
    ).check("Macro derivation failed", "has no NonEmptyAlternative")
  }
}
