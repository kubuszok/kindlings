package hearth.kindlings.catsderivation

import alleycats.{EmptyK, Pure}
import cats.{
  Alternative,
  Applicative,
  Apply,
  Bifoldable,
  Bifunctor,
  Bitraverse,
  Contravariant,
  Foldable,
  Functor,
  Invariant,
  MonoidK,
  NonEmptyAlternative,
  NonEmptyTraverse,
  Reducible,
  SemigroupK,
  Traverse
}
import cats.data.NonEmptyList
import cats.laws.discipline.{
  AlternativeTests,
  ApplicativeTests,
  ApplyTests,
  BifoldableTests,
  BifunctorTests,
  BitraverseTests,
  ContravariantTests,
  ExhaustiveCheck,
  FoldableTests,
  FunctorTests,
  InvariantTests,
  MiniInt,
  MonoidKTests,
  NonEmptyAlternativeTests,
  NonEmptyTraverseTests,
  ReducibleTests,
  SemigroupKTests,
  SerializableTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary.{catsLawsArbitraryForMiniInt, catsLawsCogenForMiniInt}
import cats.laws.discipline.eq.catsLawsEqForFn1Exhaustive
import hearth.kindlings.catsderivation.extensions.*
import org.scalacheck.Cogen

/** The law checks of kittens' polymorphic suites (`FunctorSuite`, `ContravariantSuite`, `InvariantSuite`, `ApplySuite`,
  * `ApplicativeSuite`, `FoldableSuite`, `TraverseSuite`, `ReducibleSuite`, `NonEmptyTraverseSuite`, `SemigroupKSuite`,
  * `MonoidKSuite`, `NonEmptyAlternativeSuite`, `AlternativeSuite`, `PureSuite`, `EmptyKSuite`, `BifunctorSuite`,
  * `BifoldableSuite`, `BitraverseSuite` in `core/src/test/scala-3/cats/derived`, kittens 3.5.0), with their data types
  * and type aliases, on the instances Kindlings derives (kittens' `semiauto` block of each suite; Kindlings has no
  * auto/semi-auto split, and `StrictDerivation` / `derives` are covered by other specs).
  */
final class KittensHktLawsSpec extends KittensLawsSuite {
  import KittensAdts.*
  import KittensHktLawsSpec.*

  // FunctorSuite: the exhaustive check for the nested predicate.
  implicit val exhaustivePredicate: ExhaustiveCheck[Predicate[Boolean]] =
    ExhaustiveCheck.instance(List(_ => true, _ => false, identity, !_))
  implicit val cogenPredicate: Cogen[Predicate[Boolean]] =
    Cogen[(Boolean, Boolean)].contramap(p => (p(true), p(false)))

  // FunctorSuite
  checkAll("Functor[IList]", FunctorTests[IList](functors.iList).functor[Int, String, Long])
  checkAll("Functor[Tree]", FunctorTests[Tree](functors.tree).functor[Int, String, Long])
  checkAll("Functor[GenericAdt]", FunctorTests[GenericAdt](functors.genericAdt).functor[Int, String, Long])
  checkAll("Functor[OptList]", FunctorTests[OptList](functors.optList).functor[Int, String, Long])
  checkAll("Functor[ListSnoc]", FunctorTests[ListSnoc](functors.listSnoc).functor[Int, String, Long])
  checkAll("Functor[AndChar]", FunctorTests[AndChar](functors.andChar).functor[Int, String, Long])
  checkAll("Functor[Interleaved]", FunctorTests[Interleaved](functors.interleaved).functor[Int, String, Long])
  checkAll("Functor[NestedPred]", FunctorTests[NestedPred](functors.nestedPred).functor[Boolean, Int, Boolean])
  checkAll("Functor[EnumK1]", FunctorTests[EnumK1](functors.enumK1).functor[Boolean, Int, Boolean])
  checkAll("Functor[Many]", FunctorTests[Many](functors.many).functor[Boolean, Int, Boolean])
  checkAll("Functor[AtMostOne]", FunctorTests[AtMostOne](functors.atMostOne).functor[Boolean, Int, Boolean])
  checkAll("Functor[AtLeastOne]", FunctorTests[AtLeastOne](functors.atLeastOne).functor[Boolean, Int, Boolean])
  checkAll("Functor[Singletons]", FunctorTests[Singletons](functors.singletons).functor[Boolean, Int, Boolean])
  checkAll("Functor[Search]", FunctorTests[Search](functors.search).functor[Boolean, Int, Boolean])
  checkAll("Functor is Serializable", SerializableTests.serializable(functors.tree))

  // ContravariantSuite (kittens skips InterleavedPred: typelevel/kittens#473)
  checkAll(
    "Contravariant[OptPred]",
    ContravariantTests[OptPred](contravariants.optPred).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[TreePred]",
    ContravariantTests[TreePred](contravariants.treePred).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[ListPred]",
    ContravariantTests[ListPred](contravariants.listPred).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[GenericAdtPred]",
    ContravariantTests[GenericAdtPred](contravariants.genericAdtPred).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[AndCharPred]",
    ContravariantTests[AndCharPred](contravariants.andCharPred).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[ListSnocF]",
    ContravariantTests[ListSnocF](contravariants.listSnocF).contravariant[MiniInt, String, Boolean]
  )
  checkAll(
    "Contravariant[EnumK1Contra]",
    ContravariantTests[EnumK1Contra](contravariants.enumK1Contra).contravariant[MiniInt, String, Boolean]
  )
  checkAll("Contravariant is Serializable", SerializableTests.serializable(contravariants.treePred))

  // InvariantSuite (kittens skips InterleavedF: typelevel/kittens#473)
  checkAll("Invariant[TreeF]", InvariantTests[TreePred](invariants.treeF).invariant[MiniInt, String, Boolean])
  checkAll(
    "Invariant[GenericAdtF]",
    InvariantTests[GenericAdtPred](invariants.genericAdtF).invariant[MiniInt, String, Boolean]
  )
  checkAll("Invariant[AndCharF]", InvariantTests[AndCharPred](invariants.andCharF).invariant[MiniInt, String, Boolean])
  checkAll("Invariant[ListSnoc]", InvariantTests[ListSnoc](invariants.listSnoc).invariant[MiniInt, String, Boolean])
  checkAll("Invariant[IList]", InvariantTests[IList](invariants.iList).invariant[MiniInt, String, Boolean])
  checkAll("Invariant[Bivariant]", InvariantTests[Bivariant](invariants.bivariant).invariant[MiniInt, String, Boolean])
  checkAll("Invariant[EnumK1Inv]", InvariantTests[EnumK1Inv](invariants.enumK1Inv).invariant[MiniInt, String, Boolean])
  checkAll("Invariant[Many]", InvariantTests[Many](invariants.many).invariant[MiniInt, String, Boolean])
  checkAll(
    "Invariant[AtLeastOne]",
    InvariantTests[AtLeastOne](invariants.atLeastOne).invariant[MiniInt, String, Boolean]
  )
  checkAll("Invariant[AtMostOne]", InvariantTests[AtMostOne](invariants.atMostOne).invariant[MiniInt, String, Boolean])
  checkAll(
    "Invariant[Singletons]",
    InvariantTests[Singletons](invariants.singletons).invariant[MiniInt, String, Boolean]
  )
  checkAll("Invariant[Search]", InvariantTests[Search](invariants.search).invariant[MiniInt, String, Boolean])
  checkAll("Invariant is Serializable", SerializableTests.serializable(invariants.treeF))

  // ApplySuite
  locally {
    implicit val F: Apply[CaseClassWOption] = applies.caseClassWOption
    checkAll("Apply[CaseClassWOption]", ApplyTests[CaseClassWOption].apply[Int, String, Long])
  }
  locally {
    implicit val F: Apply[OptList] = applies.optList
    checkAll("Apply[OptList]", ApplyTests[OptList].apply[Int, String, Long])
  }
  locally {
    implicit val F: Apply[AndInt] = applies.andInt
    checkAll("Apply[AndInt]", ApplyTests[AndInt].apply[Int, String, Long])
  }
  locally {
    implicit val F: Apply[Interleaved] = applies.interleaved
    checkAll("Apply[Interleaved]", ApplyTests[Interleaved].apply[Int, String, Long])
  }
  locally {
    implicit val F: Apply[ListBox] = applies.listBox
    checkAll("Apply[ListBox]", ApplyTests[ListBox].apply[Int, String, Long])
  }
  locally {
    implicit val F: Apply[Search] = applies.search
    checkAll("Apply[Search]", ApplyTests[Search].apply[Int, String, Long])
  }
  checkAll("Apply is Serializable", SerializableTests.serializable(applies.interleaved))

  // ApplicativeSuite
  locally {
    implicit val F: Applicative[CaseClassWOption] = applicatives.caseClassWOption
    checkAll("Applicative[CaseClassWOption]", ApplicativeTests[CaseClassWOption].applicative[Int, String, Long])
  }
  locally {
    implicit val F: Applicative[OptList] = applicatives.optList
    checkAll("Applicative[OptList]", ApplicativeTests[OptList].applicative[Int, String, Long])
  }
  locally {
    implicit val F: Applicative[AndInt] = applicatives.andInt
    checkAll("Applicative[AndInt]", ApplicativeTests[AndInt].applicative[Int, String, Long])
  }
  locally {
    implicit val F: Applicative[Interleaved] = applicatives.interleaved
    checkAll("Applicative[Interleaved]", ApplicativeTests[Interleaved].applicative[Int, String, Long])
  }
  locally {
    implicit val F: Applicative[ListBox] = applicatives.listBox
    checkAll("Applicative[ListBox]", ApplicativeTests[ListBox].applicative[Int, String, Long])
  }
  checkAll("Applicative is Serializable", SerializableTests.serializable(applicatives.interleaved))

  // FoldableSuite
  checkAll("Foldable[IList]", FoldableTests[IList](foldables.iList).foldable[Int, Long])
  checkAll("Foldable[Tree]", FoldableTests[Tree](foldables.tree).foldable[Int, Long])
  checkAll("Foldable[GenericAdt]", FoldableTests[GenericAdt](foldables.genericAdt).foldable[Int, Long])
  checkAll("Foldable[OptList]", FoldableTests[OptList](foldables.optList).foldable[Int, Long])
  checkAll("Foldable[ListSnoc]", FoldableTests[ListSnoc](foldables.listSnoc).foldable[Int, Long])
  checkAll("Foldable[AndChar]", FoldableTests[AndChar](foldables.andChar).foldable[Int, Long])
  checkAll("Foldable[Interleaved]", FoldableTests[Interleaved](foldables.interleaved).foldable[Int, Long])
  checkAll("Foldable[BoxNel]", FoldableTests[BoxNel](foldables.boxNel).foldable[Int, Long])
  checkAll("Foldable[EnumK1]", FoldableTests[EnumK1](foldables.enumK1).foldable[Int, Long])
  checkAll("Foldable[Many]", FoldableTests[Many](foldables.many).foldable[Int, Long])
  checkAll("Foldable[AtMostOne]", FoldableTests[AtMostOne](foldables.atMostOne).foldable[Int, Long])
  checkAll("Foldable[AtLeastOne]", FoldableTests[AtLeastOne](foldables.atLeastOne).foldable[Int, Long])
  checkAll("Foldable[Singletons]", FoldableTests[Singletons](foldables.singletons).foldable[Int, Long])
  checkAll("Foldable[Search]", FoldableTests[Search](foldables.search).foldable[Int, Long])
  checkAll("Foldable is Serializable", SerializableTests.serializable(foldables.tree))

  test("Foldable respects existing instances") {
    // Nel's own Foldable skips the head.
    assertEquals(foldables.boxNel.toList(Box(Nel(1, List(2, 3)))), List(2, 3))
  }

  // TraverseSuite
  checkAll(
    "Traverse[IList]",
    TraverseTests[IList](traverses.iList).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll("Traverse[Tree]", TraverseTests[Tree](traverses.tree).traverse[Int, Double, String, Long, Option, Option])
  checkAll(
    "Traverse[GenericAdt]",
    TraverseTests[GenericAdt](traverses.genericAdt).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[OptList]",
    TraverseTests[OptList](traverses.optList).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[ListSnoc]",
    TraverseTests[ListSnoc](traverses.listSnoc).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[AndChar]",
    TraverseTests[AndChar](traverses.andChar).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[Interleaved]",
    TraverseTests[Interleaved](traverses.interleaved).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[EnumK1]",
    TraverseTests[EnumK1](traverses.enumK1).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll("Traverse[Many]", TraverseTests[Many](traverses.many).traverse[Int, Double, String, Long, Option, Option])
  checkAll(
    "Traverse[AtMostOne]",
    TraverseTests[AtMostOne](traverses.atMostOne).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[AtLeastOne]",
    TraverseTests[AtLeastOne](traverses.atLeastOne).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[Singletons]",
    TraverseTests[Singletons](traverses.singletons).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll(
    "Traverse[Search]",
    TraverseTests[Search](traverses.search).traverse[Int, Double, String, Long, Option, Option]
  )
  checkAll("Traverse is Serializable", SerializableTests.serializable(traverses.tree))

  // ReducibleSuite
  checkAll("Reducible[ICons]", ReducibleTests[ICons](reducibles.iCons).reducible[Option, Int, Long])
  checkAll("Reducible[Tree]", ReducibleTests[Tree](reducibles.tree).reducible[Option, Int, Long])
  checkAll("Reducible[NelSCons]", ReducibleTests[NelSCons](reducibles.nelSCons).reducible[Option, Int, Long])
  checkAll("Reducible[NelAndOne]", ReducibleTests[NelAndOne](reducibles.nelAndOne).reducible[Option, Int, Long])
  checkAll("Reducible[VecAndNel]", ReducibleTests[VecAndNel](reducibles.vecAndNel).reducible[Option, Int, Long])
  checkAll("Reducible[Interleaved]", ReducibleTests[Interleaved](reducibles.interleaved).reducible[Option, Int, Long])
  checkAll("Reducible[BoxZipper]", ReducibleTests[BoxZipper](reducibles.boxZipper).reducible[Option, Int, Long])
  checkAll("Reducible[EnumK1]", ReducibleTests[EnumK1](reducibles.enumK1).reducible[Option, Int, Long])
  checkAll("Reducible[AtLeastOne]", ReducibleTests[AtLeastOne](reducibles.atLeastOne).reducible[Option, Int, Long])
  checkAll("Reducible[Singletons]", ReducibleTests[Singletons](reducibles.singletons).reducible[Option, Int, Long])
  checkAll("Reducible[Search]", ReducibleTests[Search](reducibles.search).reducible[Option, Int, Long])
  checkAll("Reducible is Serializable", SerializableTests.serializable(reducibles.tree))

  test("Reducible respects existing instances") {
    // Zipper's own Reducible skips `left`.
    assertEquals(reducibles.boxZipper.toList(Box(Zipper(List(1), 2, List(3)))), List(2, 3))
  }

  // NonEmptyTraverseSuite
  checkAll(
    "NonEmptyTraverse[ICons]",
    NonEmptyTraverseTests[ICons](nonEmptyTraverses.iCons).nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[Tree]",
    NonEmptyTraverseTests[Tree](nonEmptyTraverses.tree).nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[NelSCons]",
    NonEmptyTraverseTests[NelSCons](nonEmptyTraverses.nelSCons)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[NelAndOne]",
    NonEmptyTraverseTests[NelAndOne](nonEmptyTraverses.nelAndOne)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[VecAndNel]",
    NonEmptyTraverseTests[VecAndNel](nonEmptyTraverses.vecAndNel)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[Interleaved]",
    NonEmptyTraverseTests[Interleaved](nonEmptyTraverses.interleaved)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[EnumK1]",
    NonEmptyTraverseTests[EnumK1](nonEmptyTraverses.enumK1)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[AtLeastOne]",
    NonEmptyTraverseTests[AtLeastOne](nonEmptyTraverses.atLeastOne)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[Singletons]",
    NonEmptyTraverseTests[Singletons](nonEmptyTraverses.singletons)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll(
    "NonEmptyTraverse[Search]",
    NonEmptyTraverseTests[Search](nonEmptyTraverses.search)
      .nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option]
  )
  checkAll("NonEmptyTraverse is Serializable", SerializableTests.serializable(nonEmptyTraverses.tree))

  // SemigroupKSuite
  checkAll("SemigroupK[ComplexProduct]", SemigroupKTests[ComplexProduct](semigroupKs.complexProduct).semigroupK[Char])
  checkAll(
    "SemigroupK[CaseClassWOption]",
    SemigroupKTests[CaseClassWOption](semigroupKs.caseClassWOption).semigroupK[Char]
  )
  checkAll("SemigroupK[BoxMul]", SemigroupKTests[BoxMulK](semigroupKs.boxMul).semigroupK[Char])
  checkAll("SemigroupK is Serializable", SerializableTests.serializable(semigroupKs.complexProduct))

  test("SemigroupK respects existing instances") {
    assertEquals(semigroupKs.boxMul.combineK(Box(MulK[Char](5)), Box(MulK[Char](5))), Box(MulK[Char](25)))
  }

  // MonoidKSuite
  checkAll("MonoidK[ComplexProduct]", MonoidKTests[ComplexProduct](monoidKs.complexProduct).monoidK[Char])
  checkAll("MonoidK[CaseClassWOption]", MonoidKTests[CaseClassWOption](monoidKs.caseClassWOption).monoidK[Char])
  checkAll("MonoidK[BoxMul]", MonoidKTests[BoxMulK](monoidKs.boxMul).monoidK[Char])
  checkAll("MonoidK is Serializable", SerializableTests.serializable(monoidKs.complexProduct))

  test("MonoidK respects existing instances") {
    assertEquals(monoidKs.boxMul.empty[Char], Box(MulK[Char](1)))
    assertEquals(monoidKs.boxMul.combineK(Box(MulK[Char](5)), Box(MulK[Char](5))), Box(MulK[Char](25)))
  }

  // NonEmptyAlternativeSuite
  locally {
    implicit val F: NonEmptyAlternative[CaseClassWOption] = nonEmptyAlternatives.caseClassWOption
    checkAll(
      "NonEmptyAlternative[CaseClassWOption]",
      NonEmptyAlternativeTests[CaseClassWOption].nonEmptyAlternative[Int, String, Long]
    )
  }
  locally {
    implicit val F: NonEmptyAlternative[OptList] = nonEmptyAlternatives.optList
    checkAll("NonEmptyAlternative[OptList]", NonEmptyAlternativeTests[OptList].nonEmptyAlternative[Int, String, Long])
  }
  locally {
    implicit val F: NonEmptyAlternative[UnCons] = nonEmptyAlternatives.unCons
    checkAll("NonEmptyAlternative[UnCons]", NonEmptyAlternativeTests[UnCons].nonEmptyAlternative[Int, String, Long])
  }
  checkAll("NonEmptyAlternative is Serializable", SerializableTests.serializable(nonEmptyAlternatives.unCons))

  // AlternativeSuite
  locally {
    implicit val F: Alternative[CaseClassWOption] = alternatives.caseClassWOption
    checkAll("Alternative[CaseClassWOption]", AlternativeTests[CaseClassWOption].alternative[Int, String, Long])
  }
  locally {
    implicit val F: Alternative[OptList] = alternatives.optList
    checkAll("Alternative[OptList]", AlternativeTests[OptList].alternative[Int, String, Long])
  }
  locally {
    implicit val F: Alternative[UnCons] = alternatives.unCons
    checkAll("Alternative[UnCons]", AlternativeTests[UnCons].alternative[Int, String, Long])
  }
  checkAll("Alternative is Serializable", SerializableTests.serializable(alternatives.unCons))

  // PureSuite
  test("Pure[LOption]")(assertEquals(pures.lOption.pure(42), List(Some(42))))
  test("Pure[PList]")(assertEquals(pures.pList.pure("Scala"), (List("Scala"), List("Scala"))))
  test("Pure[CaseClassWOption]")(assertEquals(pures.caseClassWOption.pure(3.14), CaseClassWOption(Some(3.14))))
  test("Pure[NelOption]")(assertEquals(pures.nelOption.pure(42), NonEmptyList.of(Some(42))))
  test("Pure[Interleaved]")(assertEquals(pures.interleaved.pure('x'), Interleaved(0, 'x', 0L, Vector('x'), "")))
  test("Pure[Singletons]")(assertEquals(pures.singletons.pure('x'), Singletons('x')))
  test("Pure respects existing instances")(assertEquals(pures.boxColor.pure(()), Box(Color[Unit](255, 255, 255))))
  checkAll("Pure is Serializable", SerializableTests.serializable(pures.interleaved))
  test("Pure is not derived for types with a subtype not wrapping the parameter (IList, Snoc)") {
    assert(compileErrors(
      """
      import hearth.kindlings.catsderivation.extensions.*
      alleycats.Pure.derived[hearth.kindlings.catsderivation.KittensAdts.IList]
      """
          ).contains("Macro derivation failed"))
    assert(compileErrors(
      """
      import hearth.kindlings.catsderivation.extensions.*
      alleycats.Pure.derived[hearth.kindlings.catsderivation.KittensAdts.Snoc]
      """
          ).contains("Macro derivation failed"))
  }

  // EmptyKSuite
  test("EmptyK[LOption]")(assertEquals(emptyKs.lOption.empty[Int], List.empty[Option[Int]]))
  test("EmptyK[PList]")(assertEquals(emptyKs.pList.empty[Int], (List.empty[Int], List.empty[Int])))
  test("EmptyK[CaseClassWOption]")(assertEquals(emptyKs.caseClassWOption.empty[Int], CaseClassWOption[Int](None)))
  test("EmptyK[NelOption]")(assertEquals(emptyKs.nelOption.empty[Int], NonEmptyList.one(Option.empty[Int])))
  test("EmptyK[IList]")(assertEquals(emptyKs.iList.empty[Int], INil[Int](): IList[Int]))
  test("EmptyK[Snoc]")(assertEquals(emptyKs.snoc.empty[Int], SNil[Int](): Snoc[Int]))
  test("EmptyK respects existing instances") {
    assertEquals(emptyKs.boxColor.empty[Int], Box(Color[Int](255, 255, 255)))
  }
  checkAll("EmptyK is Serializable", SerializableTests.serializable(emptyKs.lOption))

  // BifunctorSuite
  checkAll(
    "Bifunctor[Result]",
    BifunctorTests[Result](bifunctors.result).bifunctor[Int, Int, Int, String, String, String]
  )
  checkAll(
    "Bifunctor[Nested]",
    BifunctorTests[NestedEither](bifunctors.nested).bifunctor[Int, Int, Int, String, String, String]
  )
  checkAll("Bifunctor is Serializable", SerializableTests.serializable(bifunctors.result))

  // BifoldableSuite
  checkAll("Bifoldable[Result]", BifoldableTests[Result](bifoldables.result).bifoldable[String, Long, Int])
  checkAll("Bifoldable[Nested]", BifoldableTests[NestedEither](bifoldables.nested).bifoldable[String, Long, Int])
  checkAll("Bifoldable is Serializable", SerializableTests.serializable(bifoldables.result))

  // BitraverseSuite
  checkAll(
    "Bitraverse[Result]",
    BitraverseTests[Result](bitraverses.result).bitraverse[Option, Int, Int, Int, Int, Int, Int]
  )
  checkAll(
    "Bitraverse[Nested]",
    BitraverseTests[NestedEither](bitraverses.nested).bitraverse[Option, Int, Int, Int, Int, Int, Int]
  )
  checkAll("Bitraverse is Serializable", SerializableTests.serializable(bitraverses.result))
}

object KittensHktLawsSpec {
  import KittensAdts.*

  object functors {
    val iList: Functor[IList] = Functor.derived[IList]
    val tree: Functor[Tree] = Functor.derived[Tree]
    val genericAdt: Functor[GenericAdt] = Functor.derived[GenericAdt]
    val optList: Functor[OptList] = Functor.derived[OptList]
    val listSnoc: Functor[ListSnoc] = Functor.derived[ListSnoc]
    val andChar: Functor[AndChar] = Functor.derived[AndChar]
    val interleaved: Functor[Interleaved] = Functor.derived[Interleaved]
    val nestedPred: Functor[NestedPred] = Functor.derived[NestedPred]
    val enumK1: Functor[EnumK1] = Functor.derived[EnumK1]
    val many: Functor[Many] = Functor.derived[Many]
    val atMostOne: Functor[AtMostOne] = Functor.derived[AtMostOne]
    val atLeastOne: Functor[AtLeastOne] = Functor.derived[AtLeastOne]
    val singletons: Functor[Singletons] = Functor.derived[Singletons]
    val search: Functor[Search] = Functor.derived[Search]
  }

  object contravariants {
    val optPred: Contravariant[OptPred] = Contravariant.derived[OptPred]
    val treePred: Contravariant[TreePred] = Contravariant.derived[TreePred]
    val listPred: Contravariant[ListPred] = Contravariant.derived[ListPred]
    val genericAdtPred: Contravariant[GenericAdtPred] = Contravariant.derived[GenericAdtPred]
    val andCharPred: Contravariant[AndCharPred] = Contravariant.derived[AndCharPred]
    val listSnocF: Contravariant[ListSnocF] = Contravariant.derived[ListSnocF]
    val enumK1Contra: Contravariant[EnumK1Contra] = Contravariant.derived[EnumK1Contra]
  }

  object invariants {
    val treeF: Invariant[TreePred] = Invariant.derived[TreePred]
    val genericAdtF: Invariant[GenericAdtPred] = Invariant.derived[GenericAdtPred]
    val andCharF: Invariant[AndCharPred] = Invariant.derived[AndCharPred]
    val listSnoc: Invariant[ListSnoc] = Invariant.derived[ListSnoc]
    val iList: Invariant[IList] = Invariant.derived[IList]
    val bivariant: Invariant[Bivariant] = Invariant.derived[Bivariant]
    val enumK1Inv: Invariant[EnumK1Inv] = Invariant.derived[EnumK1Inv]
    val many: Invariant[Many] = Invariant.derived[Many]
    val atLeastOne: Invariant[AtLeastOne] = Invariant.derived[AtLeastOne]
    val atMostOne: Invariant[AtMostOne] = Invariant.derived[AtMostOne]
    val singletons: Invariant[Singletons] = Invariant.derived[Singletons]
    val search: Invariant[Search] = Invariant.derived[Search]
  }

  object applies {
    val caseClassWOption: Apply[CaseClassWOption] = Apply.derived[CaseClassWOption]
    val optList: Apply[OptList] = Apply.derived[OptList]
    val andInt: Apply[AndInt] = Apply.derived[AndInt]
    val interleaved: Apply[Interleaved] = Apply.derived[Interleaved]
    val listBox: Apply[ListBox] = Apply.derived[ListBox]
    val search: Apply[Search] = Apply.derived[Search]
  }

  object applicatives {
    val caseClassWOption: Applicative[CaseClassWOption] = Applicative.derived[CaseClassWOption]
    val optList: Applicative[OptList] = Applicative.derived[OptList]
    val andInt: Applicative[AndInt] = Applicative.derived[AndInt]
    val interleaved: Applicative[Interleaved] = Applicative.derived[Interleaved]
    val listBox: Applicative[ListBox] = Applicative.derived[ListBox]
  }

  object foldables {
    val iList: Foldable[IList] = Foldable.derived[IList]
    val tree: Foldable[Tree] = Foldable.derived[Tree]
    val genericAdt: Foldable[GenericAdt] = Foldable.derived[GenericAdt]
    val optList: Foldable[OptList] = Foldable.derived[OptList]
    val listSnoc: Foldable[ListSnoc] = Foldable.derived[ListSnoc]
    val andChar: Foldable[AndChar] = Foldable.derived[AndChar]
    val interleaved: Foldable[Interleaved] = Foldable.derived[Interleaved]
    val boxNel: Foldable[BoxNel] = Foldable.derived[BoxNel]
    val enumK1: Foldable[EnumK1] = Foldable.derived[EnumK1]
    val many: Foldable[Many] = Foldable.derived[Many]
    val atMostOne: Foldable[AtMostOne] = Foldable.derived[AtMostOne]
    val atLeastOne: Foldable[AtLeastOne] = Foldable.derived[AtLeastOne]
    val singletons: Foldable[Singletons] = Foldable.derived[Singletons]
    val search: Foldable[Search] = Foldable.derived[Search]
  }

  object traverses {
    val iList: Traverse[IList] = Traverse.derived[IList]
    val tree: Traverse[Tree] = Traverse.derived[Tree]
    val genericAdt: Traverse[GenericAdt] = Traverse.derived[GenericAdt]
    val optList: Traverse[OptList] = Traverse.derived[OptList]
    val listSnoc: Traverse[ListSnoc] = Traverse.derived[ListSnoc]
    val andChar: Traverse[AndChar] = Traverse.derived[AndChar]
    val interleaved: Traverse[Interleaved] = Traverse.derived[Interleaved]
    val enumK1: Traverse[EnumK1] = Traverse.derived[EnumK1]
    val many: Traverse[Many] = Traverse.derived[Many]
    val atMostOne: Traverse[AtMostOne] = Traverse.derived[AtMostOne]
    val atLeastOne: Traverse[AtLeastOne] = Traverse.derived[AtLeastOne]
    val singletons: Traverse[Singletons] = Traverse.derived[Singletons]
    val search: Traverse[Search] = Traverse.derived[Search]
  }

  object reducibles {
    val iCons: Reducible[ICons] = Reducible.derived[ICons]
    val tree: Reducible[Tree] = Reducible.derived[Tree]
    val nelSCons: Reducible[NelSCons] = Reducible.derived[NelSCons]
    val nelAndOne: Reducible[NelAndOne] = Reducible.derived[NelAndOne]
    val vecAndNel: Reducible[VecAndNel] = Reducible.derived[VecAndNel]
    val interleaved: Reducible[Interleaved] = Reducible.derived[Interleaved]
    val boxZipper: Reducible[BoxZipper] = Reducible.derived[BoxZipper]
    val enumK1: Reducible[EnumK1] = Reducible.derived[EnumK1]
    val atLeastOne: Reducible[AtLeastOne] = Reducible.derived[AtLeastOne]
    val singletons: Reducible[Singletons] = Reducible.derived[Singletons]
    val search: Reducible[Search] = Reducible.derived[Search]
  }

  object nonEmptyTraverses {
    val iCons: NonEmptyTraverse[ICons] = NonEmptyTraverse.derived[ICons]
    val tree: NonEmptyTraverse[Tree] = NonEmptyTraverse.derived[Tree]
    val nelSCons: NonEmptyTraverse[NelSCons] = NonEmptyTraverse.derived[NelSCons]
    val nelAndOne: NonEmptyTraverse[NelAndOne] = NonEmptyTraverse.derived[NelAndOne]
    val vecAndNel: NonEmptyTraverse[VecAndNel] = NonEmptyTraverse.derived[VecAndNel]
    val interleaved: NonEmptyTraverse[Interleaved] = NonEmptyTraverse.derived[Interleaved]
    val enumK1: NonEmptyTraverse[EnumK1] = NonEmptyTraverse.derived[EnumK1]
    val atLeastOne: NonEmptyTraverse[AtLeastOne] = NonEmptyTraverse.derived[AtLeastOne]
    val singletons: NonEmptyTraverse[Singletons] = NonEmptyTraverse.derived[Singletons]
    val search: NonEmptyTraverse[Search] = NonEmptyTraverse.derived[Search]
  }

  object semigroupKs {
    val complexProduct: SemigroupK[ComplexProduct] = SemigroupK.derived[ComplexProduct]
    val caseClassWOption: SemigroupK[CaseClassWOption] = SemigroupK.derived[CaseClassWOption]
    val boxMul: SemigroupK[BoxMulK] = SemigroupK.derived[BoxMulK]
  }

  object monoidKs {
    val complexProduct: MonoidK[ComplexProduct] = MonoidK.derived[ComplexProduct]
    val caseClassWOption: MonoidK[CaseClassWOption] = MonoidK.derived[CaseClassWOption]
    val boxMul: MonoidK[BoxMulK] = MonoidK.derived[BoxMulK]
  }

  object nonEmptyAlternatives {
    val caseClassWOption: NonEmptyAlternative[CaseClassWOption] = NonEmptyAlternative.derived[CaseClassWOption]
    val optList: NonEmptyAlternative[OptList] = NonEmptyAlternative.derived[OptList]
    val unCons: NonEmptyAlternative[UnCons] = NonEmptyAlternative.derived[UnCons]
  }

  object alternatives {
    val caseClassWOption: Alternative[CaseClassWOption] = Alternative.derived[CaseClassWOption]
    val optList: Alternative[OptList] = Alternative.derived[OptList]
    val unCons: Alternative[UnCons] = Alternative.derived[UnCons]
  }

  object pures {
    val lOption: Pure[LOption] = Pure.derived[LOption]
    val pList: Pure[PList] = Pure.derived[PList]
    val caseClassWOption: Pure[CaseClassWOption] = Pure.derived[CaseClassWOption]
    val nelOption: Pure[NelOption] = Pure.derived[NelOption]
    val interleaved: Pure[Interleaved] = Pure.derived[Interleaved]
    val singletons: Pure[Singletons] = Pure.derived[Singletons]
    val boxColor: Pure[BoxColor] = Pure.derived[BoxColor]
  }

  object emptyKs {
    // kittens' EmptyKSuite imports alleycats' std instances (EmptyK[List], EmptyK[Option], ...).
    import alleycats.std.all.*
    val lOption: EmptyK[LOption] = EmptyK.derived[LOption]
    val pList: EmptyK[PList] = EmptyK.derived[PList]
    val caseClassWOption: EmptyK[CaseClassWOption] = EmptyK.derived[CaseClassWOption]
    val nelOption: EmptyK[NelOption] = EmptyK.derived[NelOption]
    val iList: EmptyK[IList] = EmptyK.derived[IList]
    val snoc: EmptyK[Snoc] = EmptyK.derived[Snoc]
    val boxColor: EmptyK[BoxColor] = EmptyK.derived[BoxColor]
  }

  object bifunctors {
    val result: Bifunctor[Result] = Bifunctor.derived[Result]
    val nested: Bifunctor[NestedEither] = Bifunctor.derived[NestedEither]
  }

  object bifoldables {
    val result: Bifoldable[Result] = Bifoldable.derived[Result]
    val nested: Bifoldable[NestedEither] = Bifoldable.derived[NestedEither]
  }

  object bitraverses {
    val result: Bitraverse[Result] = Bitraverse.derived[Result]
    val nested: Bitraverse[NestedEither] = Bitraverse.derived[NestedEither]
  }
}
