package hearth.kindlings.oracle.catsderivation

import cats.{Bifoldable, Bifunctor, Bitraverse, Eval}
import cats.data.State
import cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, Group, Order, Semilattice}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

final case class CtSets(a: Set[Int], b: Set[String])
final case class CtGroup(a: Int, b: Long)
final case class CtBi[A, B](a: A, b: B, label: String)

// kittens' `ADTs.Result`: fields nesting the type parameters in other type constructors (`List[E]`, `Either[A, E]`).
sealed trait CtResult[+A, +E]
case object CtDone extends CtResult[Nothing, Nothing]
final case class CtMaybeR[+A, +E](either: Either[A, E]) extends CtResult[A, E]
final case class CtOk[+A](value: A, code: Int) extends CtResult[A, Nothing]
final case class CtErr[+E](err: E, errs: List[E]) extends CtResult[Nothing, E]
// kittens' `BifunctorSuite.Nested`: an alias nesting type constructors.
type CtNestedEither[A, B] = Either[Option[A], Either[A, B]]

// kittens' `ADTs.AtLeastOne`: recursion through another type constructor (`Option`).
sealed trait CtAtLeastOne[+A]
final case class CtSingle[+A](value: A) extends CtAtLeastOne[A]
final case class CtMore[+A](value: A, rest: Option[CtAtLeastOne[A]]) extends CtAtLeastOne[A]

/** Type classes kittens derives only on Scala 3: Order for sealed traits with several subtypes, Band, Semilattice,
  * BoundedSemilattice, Group, CommutativeGroup, Bifunctor, Bifoldable, Bitraverse.
  */
final class CatsDerivationScala3OracleSpec extends MacroSuite {
  import CatsDerivationOracleSpec.{pairs, shapeValues, sign, treeValues}

  private object KindlingsSide {
    import hearth.kindlings.catsderivation.extensions.*
    val orderShape: Order[CtShape] = Order.derived[CtShape]
    val orderTree: Order[CtTree] = Order.derived[CtTree]
    val band: Band[CtSets] = Band.derived[CtSets]
    val semilattice: Semilattice[CtSets] = Semilattice.derived[CtSets]
    val boundedSemilattice: BoundedSemilattice[CtSets] = BoundedSemilattice.derived[CtSets]
    val group: Group[CtGroup] = Group.derived[CtGroup]
    val commutativeGroup: CommutativeGroup[CtGroup] = CommutativeGroup.derived[CtGroup]
    val bifunctor: Bifunctor[CtBi] = Bifunctor.derived[CtBi]
    val bifoldable: Bifoldable[CtBi] = Bifoldable.derived[CtBi]
    val bitraverse: Bitraverse[CtBi] = Bitraverse.derived[CtBi]
    val bifunctorResult: Bifunctor[CtResult] = Bifunctor.derived[CtResult]
    val bifoldableResult: Bifoldable[CtResult] = Bifoldable.derived[CtResult]
    val bitraverseResult: Bitraverse[CtResult] = Bitraverse.derived[CtResult]
    val bifunctorNested: Bifunctor[CtNestedEither] = Bifunctor.derived[CtNestedEither]
    val bifoldableNested: Bifoldable[CtNestedEither] = Bifoldable.derived[CtNestedEither]
    val bitraverseNested: Bitraverse[CtNestedEither] = Bitraverse.derived[CtNestedEither]
    val functorAtLeastOne: cats.Functor[CtAtLeastOne] = cats.Functor.derived[CtAtLeastOne]
    val foldableAtLeastOne: cats.Foldable[CtAtLeastOne] = cats.Foldable.derived[CtAtLeastOne]
    val traverseAtLeastOne: cats.Traverse[CtAtLeastOne] = cats.Traverse.derived[CtAtLeastOne]
  }

  private object KittensSide {
    import cats.derived.semiauto
    val orderShape: Order[CtShape] = semiauto.order[CtShape]
    val orderTree: Order[CtTree] = semiauto.order[CtTree]
    val band: Band[CtSets] = semiauto.band[CtSets]
    val semilattice: Semilattice[CtSets] = semiauto.semilattice[CtSets]
    val boundedSemilattice: BoundedSemilattice[CtSets] = semiauto.boundedSemilattice[CtSets]
    val group: Group[CtGroup] = semiauto.group[CtGroup]
    val commutativeGroup: CommutativeGroup[CtGroup] = semiauto.commutativeGroup[CtGroup]
    val bifunctor: Bifunctor[CtBi] = semiauto.bifunctor[CtBi]
    val bifoldable: Bifoldable[CtBi] = semiauto.bifoldable[CtBi]
    val bitraverse: Bitraverse[CtBi] = semiauto.bitraverse[CtBi]
    val bifunctorResult: Bifunctor[CtResult] = semiauto.bifunctor[CtResult]
    val bifoldableResult: Bifoldable[CtResult] = semiauto.bifoldable[CtResult]
    val bitraverseResult: Bitraverse[CtResult] = semiauto.bitraverse[CtResult]
    val bifunctorNested: Bifunctor[CtNestedEither] = semiauto.bifunctor[CtNestedEither]
    val bifoldableNested: Bifoldable[CtNestedEither] = semiauto.bifoldable[CtNestedEither]
    val bitraverseNested: Bitraverse[CtNestedEither] = semiauto.bitraverse[CtNestedEither]
    val functorAtLeastOne: cats.Functor[CtAtLeastOne] = semiauto.functor[CtAtLeastOne]
    val foldableAtLeastOne: cats.Foldable[CtAtLeastOne] = semiauto.foldable[CtAtLeastOne]
    val traverseAtLeastOne: cats.Traverse[CtAtLeastOne] = semiauto.traverse[CtAtLeastOne]
  }

  private def check[R](label: String)(kindlings: R, kittens: R): Unit = {
    val differences = Oracle.diff(kindlings, kittens)
    assertEquals(differences, Nil, s"$label: Kindlings vs kittens differences:\n${differences.mkString("\n")}")
  }

  private val sets = List(CtSets(Set(1), Set("a")), CtSets(Set(1, 2), Set.empty), CtSets(Set.empty, Set("b")))
  private val groups = List(CtGroup(1, 2L), CtGroup(-3, 10L), CtGroup(0, 0L))
  private val bis = List(CtBi(1, "x", "l1"), CtBi(2, "yy", "l2"))
  private type CtLog[A] = State[List[String], A]

  test("Order on sealed traits: subtypes are ordered by declaration, like kittens 3") {
    check("order shape")(
      pairs(shapeValues).map { case (x, y) => sign(KindlingsSide.orderShape.compare(x, y)) },
      pairs(shapeValues).map { case (x, y) => sign(KittensSide.orderShape.compare(x, y)) }
    )
    check("order tree")(
      pairs(treeValues).map { case (x, y) => sign(KindlingsSide.orderTree.compare(x, y)) },
      pairs(treeValues).map { case (x, y) => sign(KittensSide.orderTree.compare(x, y)) }
    )
  }

  test("Band, Semilattice, BoundedSemilattice") {
    check("band")(pairs(sets).map(KindlingsSide.band.combine.tupled), pairs(sets).map(KittensSide.band.combine.tupled))
    check("semilattice")(
      pairs(sets).map(KindlingsSide.semilattice.combine.tupled),
      pairs(sets).map(KittensSide.semilattice.combine.tupled)
    )
    check("bounded semilattice")(
      KindlingsSide.boundedSemilattice.empty :: pairs(sets).map(KindlingsSide.boundedSemilattice.combine.tupled),
      KittensSide.boundedSemilattice.empty :: pairs(sets).map(KittensSide.boundedSemilattice.combine.tupled)
    )
  }

  test("Group, CommutativeGroup") {
    def observe(g: Group[CtGroup]) =
      (g.empty, groups.map(g.inverse), pairs(groups).map(g.combine.tupled), pairs(groups).map(g.remove.tupled))
    check("group")(observe(KindlingsSide.group), observe(KittensSide.group))
    check("commutative group")(observe(KindlingsSide.commutativeGroup), observe(KittensSide.commutativeGroup))
  }

  test("Bifunctor, Bifoldable, Bitraverse: results and evaluation order") {
    def observe(bf: Bifunctor[CtBi], bfo: Bifoldable[CtBi], bt: Bitraverse[CtBi]) = (
      bis.map(bf.bimap(_)(_ * 10, _.length)),
      bis.map(bfo.bifoldLeft(_, List.empty[String])((acc, a) => acc :+ s"a$a", (acc, b) => acc :+ s"b$b")),
      bis.map(
        bfo
          .bifoldRight(_, Eval.now(List.empty[String]))(
            (a, acc) => acc.map(s"a$a" :: _),
            (b, acc) => acc.map(s"b$b" :: _)
          )
          .value
      ),
      bis.map(fa =>
        bt.bitraverse[CtLog, Int, String, String, Int](fa)(
          a => State((log: List[String]) => (log :+ s"a$a", s"<$a>")),
          b => State((log: List[String]) => (log :+ s"b$b", b.length))
        ).run(Nil)
          .value
      )
    )
    check("bi")(
      observe(KindlingsSide.bifunctor, KindlingsSide.bifoldable, KindlingsSide.bitraverse),
      observe(KittensSide.bifunctor, KittensSide.bifoldable, KittensSide.bitraverse)
    )
  }

  test("NonEmptyAlternative and Alternative (every field wraps the type parameter)") {
    import hearth.kindlings.catsderivation.extensions.*
    val kindlingsNea: cats.NonEmptyAlternative[CtLists] = cats.NonEmptyAlternative.derived[CtLists]
    val kindlingsAlt: cats.Alternative[CtLists] = cats.Alternative.derived[CtLists]
    val kittensNea: cats.NonEmptyAlternative[CtLists] = cats.derived.semiauto.nonEmptyAlternative[CtLists]
    val kittensAlt: cats.Alternative[CtLists] = cats.derived.semiauto.alternative[CtLists]
    val values = List(CtLists(List(1, 2), Some(1)), CtLists(Nil, None), CtLists(List(3), Some(3)))
    def observe(nea: cats.NonEmptyAlternative[CtLists]) = (
      nea.pure(5),
      pairs(values).map { case (x, y) => nea.combineK(x, y) },
      pairs(values).map { case (x, y) => nea.map2(x, y)(_ * 10 + _) },
      values.map(v => nea.map(v)(_ + 1))
    )
    check("non-empty alternative")(observe(kindlingsNea), observe(kittensNea))
    check("alternative")((observe(kindlingsAlt), kindlingsAlt.empty[Int]), (observe(kittensAlt), kittensAlt.empty[Int]))
    // A field of the type parameter itself has no NonEmptyAlternative: rejected by both.
    assert(compileErrors("""
      import hearth.kindlings.catsderivation.extensions.*
      cats.NonEmptyAlternative.derived[hearth.kindlings.oracle.catsderivation.CtProd]
    """).contains("has no NonEmptyAlternative"))
    assert(compileErrors("""
      cats.derived.semiauto.nonEmptyAlternative[hearth.kindlings.oracle.catsderivation.CtProd]
    """).nonEmpty)
  }

  test("Bifunctor, Bifoldable, Bitraverse: fields nesting the type parameters (kittens' Result and Nested)") {
    val results: List[CtResult[Int, String]] = List(
      CtDone,
      CtMaybeR(Left(1)),
      CtMaybeR(Right("ab")),
      CtOk(2, 7),
      CtErr("x", List("yy", "zzz"))
    )
    val nesteds: List[CtNestedEither[Int, String]] =
      List(Left(Some(1)), Left(None), Right(Left(2)), Right(Right("abc")))
    def observe[F[_, _]](values: List[F[Int, String]])(bf: Bifunctor[F], bfo: Bifoldable[F], bt: Bitraverse[F]) = (
      values.map(bf.bimap(_)(_ * 10, _.length)),
      values.map(bfo.bifoldLeft(_, List.empty[String])((acc, a) => acc :+ s"a$a", (acc, b) => acc :+ s"b$b")),
      values.map(
        bfo
          .bifoldRight(_, Eval.now(List.empty[String]))(
            (a, acc) => acc.map(s"a$a" :: _),
            (b, acc) => acc.map(s"b$b" :: _)
          )
          .value
      ),
      values.map(fa =>
        bt.bitraverse[CtLog, Int, String, String, Int](fa)(
          a => State((log: List[String]) => (log :+ s"a$a", s"<$a>")),
          b => State((log: List[String]) => (log :+ s"b$b", b.length))
        ).run(Nil)
          .value
      ),
      values.map(bt.bimap(_)(_ + 1, _.reverse))
    )
    check("bi result")(
      observe(results)(KindlingsSide.bifunctorResult, KindlingsSide.bifoldableResult, KindlingsSide.bitraverseResult),
      observe(results)(KittensSide.bifunctorResult, KittensSide.bifoldableResult, KittensSide.bitraverseResult)
    )
    check("bi nested")(
      observe(nesteds)(KindlingsSide.bifunctorNested, KindlingsSide.bifoldableNested, KindlingsSide.bitraverseNested),
      observe(nesteds)(KittensSide.bifunctorNested, KittensSide.bifoldableNested, KittensSide.bitraverseNested)
    )
  }

  test("Functor, Foldable, Traverse: recursion through another type constructor (kittens' AtLeastOne)") {
    val values: List[CtAtLeastOne[Int]] =
      List(CtSingle(1), CtMore(1, None), CtMore(1, Some(CtMore(2, Some(CtSingle(3))))))
    def observe(f: cats.Functor[CtAtLeastOne], fo: cats.Foldable[CtAtLeastOne], t: cats.Traverse[CtAtLeastOne]) = (
      values.map(f.map(_)(_ * 10)),
      values.map(fo.foldLeft(_, List.empty[Int])(_ :+ _)),
      values.map(fo.foldRight(_, Eval.now(List.empty[Int]))((a, acc) => acc.map(a :: _)).value),
      values.map(fa =>
        t.traverse[CtLog, Int, String](fa)(a => State((log: List[String]) => (log :+ s"visit $a", s"<$a>")))
          .run(Nil)
          .value
      )
    )
    check("at least one")(
      observe(KindlingsSide.functorAtLeastOne, KindlingsSide.foldableAtLeastOne, KindlingsSide.traverseAtLeastOne),
      observe(KittensSide.functorAtLeastOne, KittensSide.foldableAtLeastOne, KittensSide.traverseAtLeastOne)
    )
  }
}
