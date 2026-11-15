package hearth.kindlings.oracle.catsderivation

import _root_.cats.{
  Applicative,
  Apply,
  Contravariant,
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
import _root_.cats.kernel.{CommutativeMonoid, CommutativeSemigroup, Eq, Hash, Monoid, Order, PartialOrder, Semigroup}
import alleycats.{Empty, EmptyK, Pure}

/** Kindlings: `TypeClass.derived` from `hearth.kindlings.catsderivation.extensions`. */
object CtKindlingsSide extends CtInstances {
  import hearth.kindlings.catsderivation.ShowPretty
  import hearth.kindlings.catsderivation.extensions.*

  val showNested: Show[CtNested] = Show.derived[CtNested]
  val showNoFields: Show[CtNoFields] = Show.derived[CtNoFields]
  val showShape: Show[CtShape] = Show.derived[CtShape]
  val showTree: Show[CtTree] = Show.derived[CtTree]
  val showPrettyNested: CtInstances.Lines[CtNested] = ShowPretty.derived[CtNested].showLines(_)
  val showPrettyNoFields: CtInstances.Lines[CtNoFields] = ShowPretty.derived[CtNoFields].showLines(_)
  val showPrettyShape: CtInstances.Lines[CtShape] = ShowPretty.derived[CtShape].showLines(_)
  val showPrettyTree: CtInstances.Lines[CtTree] = ShowPretty.derived[CtTree].showLines(_)
  val eqNested: Eq[CtNested] = Eq.derived[CtNested]
  val eqShape: Eq[CtShape] = Eq.derived[CtShape]
  val eqTree: Eq[CtTree] = Eq.derived[CtTree]
  val orderPerson: Order[CtPerson] = Order.derived[CtPerson]
  val orderNested: Order[CtNested] = Order.derived[CtNested]
  val partialOrderNested: PartialOrder[CtNested] = PartialOrder.derived[CtNested]
  val partialOrderShape: PartialOrder[CtShape] = PartialOrder.derived[CtShape]
  val partialOrderPartial: PartialOrder[CtPartial] =
    PartialOrder.derived[CtPartial]
  val partialOrderPShape: PartialOrder[CtPShape] =
    PartialOrder.derived[CtPShape]
  val hashNested: Hash[CtNested] = Hash.derived[CtNested]
  val hashShape: Hash[CtShape] = Hash.derived[CtShape]
  val hashTree: Option[Hash[CtTree]] = Some(Hash.derived[CtTree])
  val semigroupStats: Semigroup[CtStats] = Semigroup.derived[CtStats]
  val monoidStats: Monoid[CtStats] = Monoid.derived[CtStats]
  val commutativeSemigroupCounts: CommutativeSemigroup[CtCounts] = CommutativeSemigroup.derived[CtCounts]
  val commutativeMonoidCounts: CommutativeMonoid[CtCounts] = CommutativeMonoid.derived[CtCounts]
  val emptyStats: Empty[CtStats] = Empty.derived[CtStats]
  val emptyMaybe: Empty[CtMaybe] = Empty.derived[CtMaybe]
  val functorTagged: Functor[CtTagged] = Functor.derived[CtTagged]
  val functorIList: Functor[CtIList] = Functor.derived[CtIList]
  val foldableTagged: Foldable[CtTagged] = Foldable.derived[CtTagged]
  val foldableIList: Foldable[CtIList] = Foldable.derived[CtIList]
  val traverseTagged: Traverse[CtTagged] = Traverse.derived[CtTagged]
  val traverseIList: Traverse[CtIList] = Traverse.derived[CtIList]
  val reducibleTriple: Reducible[CtTriple] = Reducible.derived[CtTriple]
  val nonEmptyTraverseTriple: NonEmptyTraverse[CtTriple] = NonEmptyTraverse.derived[CtTriple]
  val applyTriple: Apply[CtTriple] = Apply.derived[CtTriple]
  val applicativeTriple: Applicative[CtTriple] = Applicative.derived[CtTriple]
  val pureTriple: Pure[CtTriple] = Pure.derived[CtTriple]
  val contravariantPred: Contravariant[CtPred] = Contravariant.derived[CtPred]
  val invariantCodec: Invariant[CtCodec] = Invariant.derived[CtCodec]
  val semigroupKLists: SemigroupK[CtLists] = SemigroupK.derived[CtLists]
  val monoidKLists: MonoidK[CtLists] = MonoidK.derived[CtLists]
  val emptyKLists: EmptyK[CtLists] = WithAlleycatsStd.emptyKLists

  // The same `EmptyK[List]`/`EmptyK[Option]` instances as the kittens side.
  private object WithAlleycatsStd {
    import alleycats.std.all.*
    val emptyKLists: EmptyK[CtLists] = EmptyK.derived[CtLists]
  }
}

/** kittens: `cats.derived.semiauto`, as its docs show (recursive types derived without the instance in scope). */
object CtKittensSide extends CtInstances with CtKittensVersionSpecific {
  import _root_.cats.derived.semiauto

  val showNested: Show[CtNested] = semiauto.show[CtNested]
  val showNoFields: Show[CtNoFields] = semiauto.show[CtNoFields]
  val showShape: Show[CtShape] = semiauto.show[CtShape]
  val showTree: Show[CtTree] = semiauto.show[CtTree]
  val showPrettyNested: CtInstances.Lines[CtNested] = semiauto.showPretty[CtNested].showLines(_)
  val showPrettyNoFields: CtInstances.Lines[CtNoFields] = semiauto.showPretty[CtNoFields].showLines(_)
  val showPrettyShape: CtInstances.Lines[CtShape] = semiauto.showPretty[CtShape].showLines(_)
  val showPrettyTree: CtInstances.Lines[CtTree] = semiauto.showPretty[CtTree].showLines(_)
  val eqNested: Eq[CtNested] = semiauto.eq[CtNested]
  val eqShape: Eq[CtShape] = semiauto.eq[CtShape]
  val eqTree: Eq[CtTree] = semiauto.eq[CtTree]
  val orderPerson: Order[CtPerson] = semiauto.order[CtPerson]
  val orderNested: Order[CtNested] = semiauto.order[CtNested]
  val partialOrderNested: PartialOrder[CtNested] = semiauto.partialOrder[CtNested]
  val partialOrderShape: PartialOrder[CtShape] = semiauto.partialOrder[CtShape]
  val partialOrderPartial: PartialOrder[CtPartial] = semiauto.partialOrder[CtPartial]
  val partialOrderPShape: PartialOrder[CtPShape] = semiauto.partialOrder[CtPShape]
  val hashNested: Hash[CtNested] = semiauto.hash[CtNested]
  val hashShape: Hash[CtShape] = semiauto.hash[CtShape]
  val semigroupStats: Semigroup[CtStats] = semiauto.semigroup[CtStats]
  val monoidStats: Monoid[CtStats] = semiauto.monoid[CtStats]
  val commutativeSemigroupCounts: CommutativeSemigroup[CtCounts] = semiauto.commutativeSemigroup[CtCounts]
  val commutativeMonoidCounts: CommutativeMonoid[CtCounts] = semiauto.commutativeMonoid[CtCounts]
  val emptyStats: Empty[CtStats] = semiauto.empty[CtStats]
  val emptyMaybe: Empty[CtMaybe] = semiauto.empty[CtMaybe]
  val functorTagged: Functor[CtTagged] = semiauto.functor[CtTagged]
  val foldableTagged: Foldable[CtTagged] = semiauto.foldable[CtTagged]
  val traverseTagged: Traverse[CtTagged] = semiauto.traverse[CtTagged]
  val contravariantPred: Contravariant[CtPred] = semiauto.contravariant[CtPred]
  val invariantCodec: Invariant[CtCodec] = semiauto.invariant[CtCodec]
  val reducibleTriple: Reducible[CtTriple] = semiauto.reducible[CtTriple]
  val nonEmptyTraverseTriple: NonEmptyTraverse[CtTriple] = semiauto.nonEmptyTraverse[CtTriple]
  val applyTriple: Apply[CtTriple] = semiauto.apply[CtTriple]
  val applicativeTriple: Applicative[CtTriple] = semiauto.applicative[CtTriple]
  val pureTriple: Pure[CtTriple] = semiauto.pure[CtTriple]

  /** The shapes Kindlings does not support yet (see the "documented gaps" test). */
  val nelGaps: List[Any] = List(
    semiauto.reducible[CtNel],
    semiauto.reducible[CtNelPlus],
    semiauto.nonEmptyTraverse[CtNel],
    semiauto.nonEmptyTraverse[CtNelPlus],
    semiauto.apply[CtProd],
    semiauto.applicative[CtProd],
    semiauto.pure[CtProd]
  )
  val semigroupKLists: SemigroupK[CtLists] = semiauto.semigroupK[CtLists]
  val monoidKLists: MonoidK[CtLists] = semiauto.monoidK[CtLists]
  val emptyKLists: EmptyK[CtLists] = WithAlleycatsStd.emptyKLists

  // kittens' tests take `EmptyK[List]`/`EmptyK[Option]` from alleycats' std instances.
  private object WithAlleycatsStd {
    import alleycats.std.all.*
    val emptyKLists: EmptyK[CtLists] = semiauto.emptyK[CtLists]
  }
}
