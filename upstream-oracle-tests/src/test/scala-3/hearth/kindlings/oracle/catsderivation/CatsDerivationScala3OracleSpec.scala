package hearth.kindlings.oracle.catsderivation

import cats.{Bifoldable, Bifunctor, Bitraverse, Eval}
import cats.data.State
import cats.kernel.{Band, BoundedSemilattice, CommutativeGroup, Group, Order, Semilattice}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

final case class CtSets(a: Set[Int], b: Set[String])
final case class CtGroup(a: Int, b: Long)
final case class CtBi[A, B](a: A, b: B, label: String)

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

  test("documented gaps: NonEmptyAlternative and Alternative need fields nesting a type constructor") {
    // kittens derives them for e.g. `CtLists(list: List[A], opt: Option[A])` (every field must form one, and `A`
    // itself does not); Kindlings' Applicative part supports only fields of type `A`. Documented as Gap in
    // docs/user-guide/feature-parity.md (cats-derivation).
    assert(compileErrors("""
      import hearth.kindlings.catsderivation.extensions.*
      cats.Alternative.derived[hearth.kindlings.oracle.catsderivation.CtLists]
    """).contains("Macro derivation failed"))
    assert(compileErrors("""
      import hearth.kindlings.catsderivation.extensions.*
      cats.NonEmptyAlternative.derived[hearth.kindlings.oracle.catsderivation.CtLists]
    """).contains("Macro derivation failed"))
    assert(cats.derived.semiauto.alternative[CtLists] != null)
    assert(cats.derived.semiauto.nonEmptyAlternative[CtLists] != null)
  }
}
