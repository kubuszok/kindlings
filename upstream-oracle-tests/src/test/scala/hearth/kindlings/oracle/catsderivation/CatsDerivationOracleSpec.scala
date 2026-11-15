package hearth.kindlings.oracle.catsderivation

import _root_.cats.{
  Applicative,
  Apply,
  Contravariant,
  Eval,
  Foldable,
  Functor,
  Invariant,
  MonoidK,
  NonEmptyTraverse,
  Reducible,
  SemigroupK,
  Show,
  Traverse
}
import _root_.cats.data.State
import _root_.cats.kernel.{CommutativeMonoid, CommutativeSemigroup, Eq, Hash, Monoid, Order, PartialOrder, Semigroup}
import alleycats.{Empty, EmptyK, Pure}
import hearth.MacroSuite
import hearth.kindlings.oracle.Oracle

/** What both sides of the oracle provide: the instances derived for the shared fixtures. */
trait CtInstances {
  def showNested: Show[CtNested]
  def showNoFields: Show[CtNoFields]
  def showShape: Show[CtShape]
  def showTree: Show[CtTree]
  def showPrettyNested: CtInstances.Lines[CtNested]
  def showPrettyNoFields: CtInstances.Lines[CtNoFields]
  def showPrettyShape: CtInstances.Lines[CtShape]
  def showPrettyTree: CtInstances.Lines[CtTree]
  def eqNested: Eq[CtNested]
  def eqShape: Eq[CtShape]
  def eqTree: Eq[CtTree]
  def orderPerson: Order[CtPerson]
  def orderNested: Order[CtNested]
  def partialOrderNested: PartialOrder[CtNested]
  def partialOrderShape: PartialOrder[CtShape]
  def partialOrderPartial: PartialOrder[CtPartial]
  def partialOrderPShape: PartialOrder[CtPShape]
  def hashNested: Hash[CtNested]
  def hashShape: Hash[CtShape]

  /** kittens on Scala 2 cannot derive a recursive `Hash` (its `HashBuilder` is not `Lazy`). */
  def hashTree: Option[Hash[CtTree]]
  def semigroupStats: Semigroup[CtStats]
  def monoidStats: Monoid[CtStats]
  def commutativeSemigroupCounts: CommutativeSemigroup[CtCounts]
  def commutativeMonoidCounts: CommutativeMonoid[CtCounts]
  def emptyStats: Empty[CtStats]
  def emptyMaybe: Empty[CtMaybe]
  def functorTagged: Functor[CtTagged]
  def functorIList: Functor[CtIList]
  def foldableTagged: Foldable[CtTagged]
  def foldableIList: Foldable[CtIList]
  def traverseTagged: Traverse[CtTagged]
  def traverseIList: Traverse[CtIList]
  def reducibleTriple: Reducible[CtTriple]
  def nonEmptyTraverseTriple: NonEmptyTraverse[CtTriple]
  def contravariantPred: Contravariant[CtPred]
  def invariantCodec: Invariant[CtCodec]
  def applyTriple: Apply[CtTriple]
  def applicativeTriple: Applicative[CtTriple]
  def pureTriple: Pure[CtTriple]
  // Fields wrapping the type parameter.
  def reducibleNel: Reducible[CtNel]
  def reducibleNelPlus: Reducible[CtNelPlus]
  def reducibleRose: Reducible[CtRose]
  def reducibleDeep: Reducible[CtDeep]
  def nonEmptyTraverseNel: NonEmptyTraverse[CtNel]
  def nonEmptyTraverseNelPlus: NonEmptyTraverse[CtNelPlus]
  def nonEmptyTraverseRose: NonEmptyTraverse[CtRose]
  def nonEmptyTraverseDeep: NonEmptyTraverse[CtDeep]
  def applyProd: Apply[CtProd]
  def applyWrapPair: Apply[CtWrapPair]
  def applyDeep: Apply[CtDeep]
  def applicativeProd: Applicative[CtProd]
  def applicativeWrapPair: Applicative[CtWrapPair]
  def applicativeDeep: Applicative[CtDeep]
  def pureProd: Pure[CtProd]
  def pureWrapPair: Pure[CtWrapPair]
  def pureDeep: Pure[CtDeep]
  def semigroupKLists: SemigroupK[CtLists]
  def monoidKLists: MonoidK[CtLists]
  def emptyKLists: EmptyK[CtLists]
}
object CtInstances {

  /** `ShowPretty.showLines` (Kindlings and kittens each define their own `ShowPretty` trait). */
  type Lines[A] = A => List[String]
}

/** Kindlings' cats-derivation (`TypeClass.derived`) vs kittens' `cats.derived.semiauto`, on the same fixtures: rendered
  * strings, comparison results on many value pairs (including different subtypes), hash consistency, combine/empty
  * results, map/fold/traverse results and the order in which effects run.
  */
final class CatsDerivationOracleSpec extends MacroSuite {
  import CatsDerivationOracleSpec.*

  private val k: CtInstances = CtKindlingsSide
  private val u: CtInstances = CtKittensSide

  /** Runs `observe` on both sides and returns `label: kindlings=... kittens=...` for each differing result. */
  private def compare[R](label: String)(observe: CtInstances => R): List[String] = {
    def run(i: CtInstances): Any =
      try observe(i)
      catch { case e @ (_: Exception | _: StackOverflowError) => s"threw ${e.getClass.getSimpleName}" }
    val kr = run(k)
    val ur = run(u)
    Oracle.diff(kr, ur).map(d => s"$label ${d.stripPrefix("$")}".replace("upstream=", "kittens="))
  }

  private def check(differences: List[String], expected: List[String] = Nil): Unit =
    assertEquals(differences, expected, s"Kindlings vs kittens differences:\n${differences.mkString("\n")}")

  test("Show") {
    check(
      compare("show nested")(i => nestedValues.map(i.showNested.show)) ++
        compare("show no fields")(i => List(i.showNoFields.show(CtNoFields()))) ++
        compare("show shape")(i => shapeValues.map(i.showShape.show)) ++
        compare("show tree")(i => treeValues.map(i.showTree.show)),
      CtPlatformExpectations.show
    )
  }

  test("ShowPretty") {
    check(
      compare("showPretty nested")(i => nestedValues.map(i.showPrettyNested)) ++
        compare("showPretty no fields")(i => List(i.showPrettyNoFields(CtNoFields()))) ++
        compare("showPretty shape")(i => shapeValues.map(i.showPrettyShape)) ++
        compare("showPretty tree")(i => treeValues.map(i.showPrettyTree)),
      CtPlatformExpectations.showPretty
    )
  }

  test("Eq on every pair") {
    check(
      compare("eqv nested")(i => pairs(nestedValues).map { case (x, y) => i.eqNested.eqv(x, y) }) ++
        compare("eqv shape")(i => pairs(shapeValues).map { case (x, y) => i.eqShape.eqv(x, y) }) ++
        compare("eqv tree")(i => pairs(treeValues).map { case (x, y) => i.eqTree.eqv(x, y) })
    )
  }

  test("Order and PartialOrder on every pair (signs; NaN for incomparable values)") {
    check(
      compare("compare person")(i => pairs(personValues).map { case (x, y) => sign(i.orderPerson.compare(x, y)) }) ++
        compare("compare nested")(i => pairs(nestedValues).map { case (x, y) => sign(i.orderNested.compare(x, y)) }) ++
        compare("partialCompare nested")(i =>
          pairs(nestedValues).map { case (x, y) => sign(i.partialOrderNested.partialCompare(x, y)) }
        ) ++
        compare("partialCompare shape")(i =>
          pairs(shapeValues).map { case (x, y) => sign(i.partialOrderShape.partialCompare(x, y)) }
        ) ++
        compare("partialCompare partial")(i =>
          pairs(partialValues).map { case (x, y) => sign(i.partialOrderPartial.partialCompare(x, y)) }
        ) ++
        compare("partialCompare pshape")(i =>
          pairs(pshapeValues).map { case (x, y) => sign(i.partialOrderPShape.partialCompare(x, y)) }
        )
    )
  }

  test("Hash is consistent with Eq and separates the fixtures like kittens' does") {
    def hashPattern[A](values: List[A], h: Hash[A]): List[(Boolean, Boolean)] =
      pairs(values).map { case (x, y) => (h.eqv(x, y), h.hash(x) == h.hash(y)) }
    check(
      compare("hash nested")(i => hashPattern(nestedValues, i.hashNested)) ++
        compare("hash shape")(i => hashPattern(shapeValues, i.hashShape)) ++
        (if (u.hashTree.isEmpty) Nil else compare("hash tree")(i => hashPattern(treeValues, i.hashTree.get)))
    )
    // Equal values must hash equally (structurally equal copies, not just the same reference).
    nestedValues.foreach { x =>
      val copy = x.copy(person = x.person.copy())
      assertEquals(k.hashNested.hash(x), k.hashNested.hash(copy))
    }
  }

  test("Semigroup, Monoid, CommutativeSemigroup, CommutativeMonoid and Empty") {
    check(
      compare("combine stats")(i => pairs(statsValues).map { case (x, y) => i.semigroupStats.combine(x, y) }) ++
        compare("monoid combine stats")(i => pairs(statsValues).map { case (x, y) => i.monoidStats.combine(x, y) }) ++
        compare("monoid empty stats")(i => List(i.monoidStats.empty, i.monoidStats.combineAll(statsValues))) ++
        compare("commutative combine counts")(i =>
          pairs(countsValues).map { case (x, y) => i.commutativeSemigroupCounts.combine(x, y) }
        ) ++
        compare("commutative monoid counts")(i =>
          i.commutativeMonoidCounts.empty :: pairs(countsValues).map { case (x, y) =>
            i.commutativeMonoidCounts.combine(x, y)
          }
        ) ++
        compare("empty stats")(i => List(i.emptyStats.empty)) ++
        compare("empty maybe")(i => List(i.emptyMaybe.empty))
    )
  }

  test("Functor, Foldable and Traverse: results and evaluation order") {
    check(
      compare("map tagged")(i => taggedValues.map(i.functorTagged.map(_)(_ * 10))) ++
        compare("map ilist")(i => ilistValues.map(i.functorIList.map(_)(_ + 1))) ++
        compare("foldLeft tagged")(i => taggedValues.map(i.foldableTagged.foldLeft(_, List.empty[Int])(_ :+ _))) ++
        compare("foldRight tagged")(i =>
          taggedValues.map(i.foldableTagged.foldRight(_, Eval.now(List.empty[Int]))((a, b) => b.map(a :: _)).value)
        ) ++
        compare("foldLeft ilist")(i => ilistValues.map(i.foldableIList.foldLeft(_, List.empty[Int])(_ :+ _))) ++
        compare("lazy foldRight tagged")(i => taggedValues.map(lazyFoldRight(i.foldableTagged, _))) ++
        compare("lazy foldRight ilist")(i => ilistValues.map(lazyFoldRight(i.foldableIList, _))) ++
        compare("size/exists/toList tagged")(i =>
          taggedValues.map { t =>
            val f = i.foldableTagged
            (f.size(t), f.exists(t)(_ > 2), f.toList(t), f.foldMap(t)(_.toString))
          }
        ) ++
        compare("traverse tagged")(i => taggedValues.map(fa => logTraverse(i.traverseTagged, fa))) ++
        compare("traverse ilist")(i => ilistValues.map(fa => logTraverse(i.traverseIList, fa))) ++
        compare("traverse option tagged")(i =>
          taggedValues.map(i.traverseTagged.traverse(_)(a => if (a > 3) None else Some(a.toString)))
        ) ++
        compare("sequence tagged")(i =>
          i.traverseTagged.sequence[List, Int](CtTagged(List(List(1, 2)), Some(List(3)), List(4, 5), Nil))
        )
    )
  }

  test("Reducible and NonEmptyTraverse: results and evaluation order") {
    check(
      compare("reduceLeft triple")(i => tripleValues.map(i.reducibleTriple.reduceLeft(_)((a, b) => a * 10 + b))) ++
        compare("reduceRight triple")(i =>
          tripleValues.map(i.reducibleTriple.reduceRight(_)((a, eb) => eb.map(b => a * 10 + b)).value)
        ) ++
        compare("reduceRightTo triple")(i =>
          tripleValues.map(i.reducibleTriple.reduceRightTo(_)(_.toString)((a, eb) => eb.map(a.toString + _)).value)
        ) ++
        compare("lazy reduceRight triple")(i => tripleValues.map(lazyReduceRight(i.reducibleTriple, _))) ++
        compare("lazy foldRight triple")(i => tripleValues.map(lazyFoldRight(i.reducibleTriple, _))) ++
        compare("nonEmptyTraverse triple")(i =>
          tripleValues.map(fa => logNonEmptyTraverse(i.nonEmptyTraverseTriple, fa))
        ) ++
        compare("traverse triple")(i => tripleValues.map(fa => logTraverse(i.nonEmptyTraverseTriple, fa))),
      List(
        // Same result, but Kindlings' `reduceRightTo` is lazier: when the function short-circuits on the first field
        // (`3 > 2`) the rest is never evaluated, while kittens evaluates the function on the later fields first.
        // Documented in docs/user-guide/feature-parity.md (cats-derivation, "Upstream oracle").
        "lazy reduceRight triple [1]._1: kindlings=1 kittens=2"
      )
    )
  }

  test("Contravariant and Invariant") {
    check(
      compare("contramap pred")(i =>
        List(1, 2, 3).map { n =>
          val p = i.contravariantPred.contramap(CtPred[String](_.length > 1, _.length))((m: Int) => "x" * m)
          (p.run(n), p.size(n))
        }
      ) ++
        compare("imap codec")(i =>
          List(1, 2, 3).map { n =>
            val c = i.invariantCodec.imap(CtCodec[Int](_.toString, _.toInt))(_.toLong * 2)(_.toInt / 2)
            (c.encode(n.toLong * 2), c.decode(n.toString))
          }
        )
    )
  }

  test("Apply, Applicative, Pure") {
    val t1 = CtTriple(1, 2, 3)
    val t2 = CtTriple(10, 20, 30)
    check(
      compare("map2 triple")(i => i.applyTriple.map2(t1, t2)(_ + _)) ++
        compare("product triple")(i => i.applyTriple.product(t1, t2)) ++
        compare("ap triple")(i => i.applyTriple.ap(CtTriple[Int => Int](_ + 1, _ * 2, _ * 3))(t1)) ++
        compare("applicative map2 triple")(i => i.applicativeTriple.map2(t1, t2)(_ + _)) ++
        compare("applicative pure triple")(i => i.applicativeTriple.pure(5)) ++
        compare("applicative unit triple")(i => i.applicativeTriple.unit) ++
        compare("pure triple")(i => i.pureTriple.pure("x"))
    )
  }

  test("Reducible and NonEmptyTraverse with fields wrapping the type parameter") {
    def observe[F[_]](r: Reducible[F], t: NonEmptyTraverse[F], values: List[F[Int]]) = values.map { v =>
      (
        r.reduceLeft(v)((a, b) => a * 10 + b),
        r.reduceRightTo(v)(_.toString)((a, eb) => eb.map(a.toString + _)).value,
        r.foldLeft(v, List.empty[Int])(_ :+ _),
        lazyReduceRight(r, v),
        lazyFoldRight(r, v),
        r.size(v),
        logNonEmptyTraverse(t, v),
        logTraverse(t, v),
        t.nonEmptyTraverse(v)(a => if (a > 3) None else Some(a * 2))
      )
    }
    check(
      compare("nel")(i => observe(i.reducibleNel, i.nonEmptyTraverseNel, nelValues)) ++
        compare("nelPlus")(i => observe(i.reducibleNelPlus, i.nonEmptyTraverseNelPlus, nelPlusValues)) ++
        compare("rose")(i => observe(i.reducibleRose, i.nonEmptyTraverseRose, roseValues)) ++
        compare("deep")(i => observe(i.reducibleDeep, i.nonEmptyTraverseDeep, deepValues))
    )
  }

  test("Apply, Applicative, Pure with fields wrapping the type parameter") {
    def observe[F[_]](ap: Apply[F], app: Applicative[F], values: List[F[Int]]) = (
      pairs(values).map { case (x, y) => ap.map2(x, y)(_ * 100 + _) },
      pairs(values).map { case (x, y) => ap.ap(ap.map(x)(a => (b: Int) => a - b))(y) },
      pairs(values).map { case (x, y) => app.map2(x, y)(_ * 100 + _) },
      app.pure(5),
      app.unit
    )
    check(
      compare("prod")(i => (observe(i.applyProd, i.applicativeProd, prodValues), i.pureProd.pure("x"))) ++
        // Not compared for the recursive `CtRose`: kittens' derived Apply/Applicative overflow the stack there
        // (Kindlings' work - see KittensParitySpec in cats-derivation).
        compare("wrapPair")(i =>
          (observe(i.applyWrapPair, i.applicativeWrapPair, wrapPairValues), i.pureWrapPair.pure("x"))
        ) ++
        compare("deep")(i => (observe(i.applyDeep, i.applicativeDeep, deepValues), i.pureDeep.pure("x")))
    )
  }

  test("shapes kittens rejects are rejected") {
    // No field guarantees an element (Option and Vector are not Reducible).
    assert(compileErrors("""
      import hearth.kindlings.catsderivation.extensions.*
      cats.Reducible.derived[hearth.kindlings.oracle.catsderivation.CtWrapPair]
    """).contains("no field guarantees an element"))
    assert(compileErrors("""
      cats.derived.semiauto.reducible[hearth.kindlings.oracle.catsderivation.CtWrapPair]
    """).nonEmpty)
    // A wrapping type without the instance (Set has no Apply).
    assert(compileErrors("""
      import hearth.kindlings.catsderivation.extensions.*
      cats.Apply.derived[hearth.kindlings.oracle.catsderivation.CtSetField]
    """).contains("no Apply"))
    assert(compileErrors("""
      cats.derived.semiauto.apply[hearth.kindlings.oracle.catsderivation.CtSetField]
    """).nonEmpty)
  }

  test("SemigroupK, MonoidK, EmptyK") {
    val l1 = CtLists(List(1), None)
    val l2 = CtLists(List(2, 3), Some(2))
    val l3 = CtLists(Nil, Some(3))
    check(
      compare("combineK lists")(i => pairs(List(l1, l2, l3)).map { case (x, y) => i.semigroupKLists.combineK(x, y) }) ++
        compare("monoidK lists")(i =>
          (i.monoidKLists.empty[Int], pairs(List(l1, l2, l3)).map { case (x, y) => i.monoidKLists.combineK(x, y) })
        ) ++
        compare("emptyK lists")(i => i.emptyKLists.empty[Int])
    )
  }
}
object CatsDerivationOracleSpec {

  type CtLog[A] = State[List[String], A]

  val personValues: List[CtPerson] = List(CtPerson("a", 1), CtPerson("a", 2), CtPerson("b", 0), CtPerson("", 5))
  val nestedValues: List[CtNested] = List(
    CtNested(1L, CtPerson("ann", 30), List("x", "y"), Some("a")),
    CtNested(1L, CtPerson("ann", 30), List("x"), Some("a")),
    CtNested(1L, CtPerson("ann", 30), Nil, None),
    CtNested(2L, CtPerson("bob", 20), List("z"), None),
    CtNested(0L, CtPerson("", 0), List("multi\nline"), Some("b"))
  )
  val shapeValues: List[CtShape] =
    List(CtZCircle(1.0), CtZCircle(2.5), CtARect(1.0, 2.0), CtARect(1.0, 1.0), CtMOrigin)
  val treeValues: List[CtTree] =
    List(CtLeaf(1), CtLeaf(2), CtNode(CtLeaf(1), CtLeaf(2)), CtNode(CtNode(CtLeaf(1), CtLeaf(2)), CtLeaf(3)))
  val partialValues: List[CtPartial] =
    List(CtPartial(Set(1), 1), CtPartial(Set(1, 2), 0), CtPartial(Set(2), 3), CtPartial(Set(1), 2))
  val pshapeValues: List[CtPShape] =
    List(CtPCircle(Set(1)), CtPCircle(Set(1, 2)), CtPCircle(Set(3)), CtPSquare(1), CtPSquare(2))
  val statsValues: List[CtStats] = List(
    CtStats(1, 10L, List("a"), Some(1)),
    CtStats(2, 20L, List("b", "c"), None),
    CtStats(0, 0L, Nil, Some(5))
  )
  val countsValues: List[CtCounts] =
    List(CtCounts(1, 2L, Map("a" -> 1)), CtCounts(3, 4L, Map("a" -> 2, "b" -> 3)), CtCounts(0, 0L, Map.empty))
  val taggedValues: List[CtTagged[Int]] = List(
    CtTagged(List(1, 2), Some(3), 4, List(Some(5), None, Some(6))),
    CtTagged(Nil, None, 1, Nil),
    CtTagged(List(5), None, 2, List(None, Some(3)))
  )
  val ilistValues: List[CtIList[Int]] =
    List(CtINil[Int](), CtICons(1, CtINil()), CtICons(1, CtICons(2, CtICons(3, CtICons(4, CtINil())))))
  val tripleValues: List[CtTriple[Int]] = List(CtTriple(1, 2, 3), CtTriple(3, 1, 2), CtTriple(0, 0, 5))
  val nelValues: List[CtNel[Int]] = List(CtNel(1, Nil), CtNel(1, List(2, 3)), CtNel(4, List(1)))
  val nelPlusValues: List[CtNelPlus[Int]] = List(
    CtNelPlus(None, CtNel(1, Nil), Nil),
    CtNelPlus(Some(1), CtNel(2, List(3)), List(4, 5)),
    CtNelPlus(None, CtNel(7, List(8)), List(9))
  )
  val roseValues: List[CtRose[Int]] = List(
    CtRose(1, Nil),
    CtRose(1, List(CtRose(2, Nil), CtRose(3, List(CtRose(4, Nil))))),
    CtRose(5, List(CtRose(1, Nil)))
  )
  val wrapPairValues: List[CtWrapPair[Int]] =
    List(CtWrapPair(Some(1), Vector(2, 3)), CtWrapPair(None, Vector.empty), CtWrapPair(Some(4), Vector(5)))
  val deepValues: List[CtDeep[Int]] =
    List(CtDeep(List(Some(1), None, Some(2)), 3, "x"), CtDeep(Nil, 1, ""), CtDeep(List(Some(5)), 2, "y"))
  val prodValues: List[CtProd[Int]] = List(CtProd(1, List(1, 2)), CtProd(10, List(10, 20)), CtProd(0, Nil))

  def pairs[A](values: List[A]): List[(A, A)] = for { x <- values; y <- values } yield (x, y)

  def sign(i: Int): Int = Integer.signum(i)
  def sign(d: Double): String = if (d.isNaN) "NaN" else math.signum(d).toString

  /** `foldRight` that stops at the first element > 2: how many elements were visited, and the result. */
  def lazyFoldRight[F[_]](f: Foldable[F], fa: F[Int]): (Int, Boolean) = {
    var visited = 0
    val result = f.foldRight(fa, Eval.now(false)) { (a, rest) =>
      visited += 1
      if (a > 2) Eval.now(true) else rest
    }
    val r = result.value
    (visited, r)
  }

  def lazyReduceRight[F[_]](f: Reducible[F], fa: F[Int]): (Int, Int) = {
    var visited = 0
    val result = f.reduceRightTo(fa)(identity) { (a, rest) =>
      visited += 1
      if (a > 2) Eval.now(a) else rest.map(_ + a)
    }
    val r = result.value
    (visited, r)
  }

  def logTraverse[F[_]](t: Traverse[F], fa: F[Int]): (List[String], F[String]) =
    t.traverse[CtLog, Int, String](fa)(a => State((log: List[String]) => (log :+ s"visit $a", s"<$a>"))).run(Nil).value

  def logNonEmptyTraverse[F[_]](t: NonEmptyTraverse[F], fa: F[Int]): (List[String], F[String]) =
    t.nonEmptyTraverse[CtLog, Int, String](fa)(a => State((log: List[String]) => (log :+ s"visit $a", s"<$a>")))
      .run(Nil)
      .value
}
