package hearth.kindlings.catsderivation

import cats.kernel.{
  Band,
  BoundedSemilattice,
  CommutativeGroup,
  CommutativeMonoid,
  CommutativeSemigroup,
  Eq,
  Group,
  Hash,
  Monoid,
  Order,
  PartialOrder,
  Semigroup,
  Semilattice
}
import cats.kernel.laws.discipline.{
  BandTests,
  BoundedSemilatticeTests,
  CommutativeGroupTests,
  CommutativeMonoidTests,
  CommutativeSemigroupTests,
  EqTests,
  GroupTests,
  HashTests,
  MonoidTests,
  OrderTests,
  PartialOrderTests,
  SemigroupTests,
  SemilatticeTests,
  SerializableTests
}
import hearth.kindlings.catsderivation.extensions.*

/** The law checks of kittens' `EqSuite`, `HashSuite`, `OrderSuite`, `PartialOrderSuite`, `SemigroupSuite`,
  * `MonoidSuite`, `CommutativeSemigroupSuite`, `CommutativeMonoidSuite`, `BandSuite`, `SemilatticeSuite`,
  * `BoundedSemilatticeSuite`, `GroupSuite` and `CommutativeGroupSuite` (their `semiauto` instances), plus their
  * "respects existing instances" tests, on Kindlings-derived instances.
  */
final class KittensKernelLawsSpec extends KittensLawsSuite {
  import KittensAdts.*
  import KittensKernelLawsSpec.*

  // EqSuite
  checkAll("Eq[Foo]", EqTests[Foo](eqs.foo).eqv)
  checkAll("Eq[IList[Int]]", EqTests[IList[Int]](eqs.iList).eqv)
  checkAll("Eq[Inner]", EqTests[Inner](eqs.inner).eqv)
  checkAll("Eq[Outer]", EqTests[Outer](eqs.outer).eqv)
  checkAll("Eq[Interleaved[Int]]", EqTests[Interleaved[Int]](eqs.interleaved).eqv)
  checkAll("Eq[Tree[Int]]", EqTests[Tree[Int]](eqs.tree).eqv)
  checkAll("Eq[Recursive]", EqTests[Recursive](eqs.recursive).eqv)
  checkAll("Eq[Singletons[Int]]", EqTests[Singletons[Int]](eqs.singletons).eqv)
  checkAll("Eq is Serializable", SerializableTests.serializable(eqs.foo))

  // HashSuite
  checkAll("Hash[IList[Int]]", HashTests[IList[Int]](hashes.iList).hash)
  checkAll("Hash[Inner]", HashTests[Inner](hashes.inner).hash)
  checkAll("Hash[Outer]", HashTests[Outer](hashes.outer).hash)
  checkAll("Hash[Interleaved[Int]]", HashTests[Interleaved[Int]](hashes.interleaved).hash)
  checkAll("Hash[Tree[Int]]", HashTests[Tree[Int]](hashes.tree).hash)
  checkAll("Hash[Recursive]", HashTests[Recursive](hashes.recursive).hash)
  checkAll("Hash[EnumK0]", HashTests[EnumK0](hashes.enumK0).hash)
  checkAll("Hash[Singletons[Int]]", HashTests[Singletons[Int]](hashes.singletons).hash)
  checkAll("Hash is Serializable", SerializableTests.serializable(hashes.inner))

  // OrderSuite
  checkAll("Order[Inner]", OrderTests[Inner](orders.inner).order)
  checkAll("Order[Outer]", OrderTests[Outer](orders.outer).order)
  checkAll("Order[Interleaved[Int]]", OrderTests[Interleaved[Int]](orders.interleaved).order)
  checkAll("Order[Recursive]", OrderTests[Recursive](orders.recursive).order)
  checkAll("Order[GenericAdt[Int]]", OrderTests[GenericAdt[Int]](orders.genericAdt).order)
  checkAll("Order[EnumK0]", OrderTests[EnumK0](orders.enumK0).order)
  checkAll("Order[Singletons[Int]]", OrderTests[Singletons[Int]](orders.singletons).order)
  checkAll("Order is Serializable", SerializableTests.serializable(orders.interleaved))

  // PartialOrderSuite
  checkAll("PartialOrder[IList[Int]]", PartialOrderTests[IList[Int]](partialOrders.iList).partialOrder)
  checkAll("PartialOrder[Inner]", PartialOrderTests[Inner](partialOrders.inner).partialOrder)
  checkAll("PartialOrder[Outer]", PartialOrderTests[Outer](partialOrders.outer).partialOrder)
  checkAll(
    "PartialOrder[Interleaved[Int]]",
    PartialOrderTests[Interleaved[Int]](partialOrders.interleaved).partialOrder
  )
  checkAll("PartialOrder[Tree[Int]]", PartialOrderTests[Tree[Int]](partialOrders.tree).partialOrder)
  checkAll("PartialOrder[Recursive]", PartialOrderTests[Recursive](partialOrders.recursive).partialOrder)
  checkAll("PartialOrder[Box[KeyValue]]", PartialOrderTests[Box[KeyValue]](partialOrders.boxKeyValue).partialOrder)
  checkAll("PartialOrder[EnumK0]", PartialOrderTests[EnumK0](partialOrders.enumK0).partialOrder)
  checkAll(
    "PartialOrder[Singletons[Int]]",
    PartialOrderTests[Singletons[Int]](partialOrders.singletons).partialOrder
  )
  checkAll("PartialOrder is Serializable", SerializableTests.serializable(partialOrders.tree))

  test("PartialOrder respects existing instances") {
    val x = Box(KeyValue("red", 1))
    val y = Box(KeyValue("red", 2))
    val z = Box(KeyValue("blue", 1))
    assert(partialOrders.boxKeyValue.partialCompare(x, y) < 0)
    assert(partialOrders.boxKeyValue.partialCompare(y, z).isNaN)
  }

  // SemigroupSuite
  checkAll("Semigroup[Foo]", SemigroupTests[Foo](semigroups.foo).semigroup)
  checkAll("Semigroup[Interleaved[Int]]", SemigroupTests[Interleaved[Int]](semigroups.interleaved).semigroup)
  checkAll("Semigroup[Box[Mul]]", SemigroupTests[Box[Mul]](semigroups.boxMul).semigroup)
  checkAll("Semigroup[Recursive]", SemigroupTests[Recursive](semigroups.recursive).semigroup)
  checkAll("Semigroup is Serializable", SerializableTests.serializable(semigroups.foo))

  test("Semigroup respects existing instances") {
    assertEquals(semigroups.boxMul.combine(Box(Mul(5)), Box(Mul(5))).content.value, 25)
  }

  // MonoidSuite
  checkAll("Monoid[Foo]", MonoidTests[Foo](monoids.foo).monoid)
  checkAll("Monoid[Interleaved[Int]]", MonoidTests[Interleaved[Int]](monoids.interleaved).monoid)
  checkAll("Monoid[Box[Mul]]", MonoidTests[Box[Mul]](monoids.boxMul).monoid)
  checkAll("Monoid[Recursive]", MonoidTests[Recursive](monoids.recursive).monoid)
  checkAll("Monoid is Serializable", SerializableTests.serializable(monoids.foo))

  test("Monoid respects existing instances") {
    assertEquals(monoids.boxMul.empty, Box(Mul(1)))
    assertEquals(monoids.boxMul.combine(Box(Mul(5)), Box(Mul(5))), Box(Mul(25)))
  }

  // CommutativeSemigroupSuite
  checkAll(
    "CommutativeSemigroup[CommutativeFoo]",
    CommutativeSemigroupTests[CommutativeFoo](commutativeSemigroups.commutativeFoo).commutativeSemigroup
  )
  checkAll(
    "CommutativeSemigroup[Recursive]",
    CommutativeSemigroupTests[Recursive](commutativeSemigroups.recursive).commutativeSemigroup
  )
  checkAll(
    "CommutativeSemigroup[Box[Mul]]",
    CommutativeSemigroupTests[Box[Mul]](commutativeSemigroups.boxMul).commutativeSemigroup
  )
  checkAll(
    "CommutativeSemigroup is Serializable",
    SerializableTests.serializable(commutativeSemigroups.commutativeFoo)
  )

  test("CommutativeSemigroup respects existing instances") {
    assertEquals(commutativeSemigroups.boxMul.combine(Box(Mul(5)), Box(Mul(5))).content.value, 25)
  }

  // CommutativeMonoidSuite
  checkAll(
    "CommutativeMonoid[CommutativeFoo]",
    CommutativeMonoidTests[CommutativeFoo](commutativeMonoids.commutativeFoo).commutativeMonoid
  )
  checkAll(
    "CommutativeMonoid[Recursive]",
    CommutativeMonoidTests[Recursive](commutativeMonoids.recursive).commutativeMonoid
  )
  checkAll("CommutativeMonoid[Box[Mul]]", CommutativeMonoidTests[Box[Mul]](commutativeMonoids.boxMul).commutativeMonoid)
  checkAll("CommutativeMonoid is Serializable", SerializableTests.serializable(commutativeMonoids.commutativeFoo))

  test("CommutativeMonoid respects existing instances") {
    assertEquals(commutativeMonoids.boxMul.empty, Box(Mul(1)))
    assertEquals(commutativeMonoids.boxMul.combine(Box(Mul(5)), Box(Mul(5))), Box(Mul(25)))
  }

  // BandSuite, SemilatticeSuite, BoundedSemilatticeSuite
  checkAll("Band[Masked[String]]", BandTests[Masked[String]](lattices.band).band)
  checkAll("Band is Serializable", SerializableTests.serializable(lattices.band))
  checkAll("Semilattice[Masked[String]]", SemilatticeTests[Masked[String]](lattices.semilattice).semilattice)
  checkAll("Semilattice is Serializable", SerializableTests.serializable(lattices.semilattice))
  checkAll(
    "BoundedSemilattice[Masked[String]]",
    BoundedSemilatticeTests[Masked[String]](lattices.boundedSemilattice).boundedSemilattice
  )
  checkAll("BoundedSemilattice is Serializable", SerializableTests.serializable(lattices.boundedSemilattice))

  // GroupSuite, CommutativeGroupSuite
  checkAll("Group[Slice]", GroupTests[Slice](groups.slice).group)
  checkAll("Group[Compared]", GroupTests[Compared](groups.compared).group)
  checkAll("Group is Serializable", SerializableTests.serializable(groups.slice))
  checkAll("CommutativeGroup[Slice]", CommutativeGroupTests[Slice](groups.commutativeSlice).commutativeGroup)
  checkAll("CommutativeGroup[Compared]", CommutativeGroupTests[Compared](groups.commutativeCompared).commutativeGroup)
  checkAll("CommutativeGroup is Serializable", SerializableTests.serializable(groups.commutativeSlice))
}

object KittensKernelLawsSpec {
  import KittensAdts.*

  object eqs {
    val foo: Eq[Foo] = Eq.derived[Foo]
    val iList: Eq[IList[Int]] = Eq.derived[IList[Int]]
    val inner: Eq[Inner] = Eq.derived[Inner]
    val outer: Eq[Outer] = Eq.derived[Outer]
    val interleaved: Eq[Interleaved[Int]] = Eq.derived[Interleaved[Int]]
    val tree: Eq[Tree[Int]] = Eq.derived[Tree[Int]]
    val recursive: Eq[Recursive] = Eq.derived[Recursive]
    val singletons: Eq[Singletons[Int]] = Eq.derived[Singletons[Int]]
  }

  object hashes {
    val iList: Hash[IList[Int]] = Hash.derived[IList[Int]]
    val inner: Hash[Inner] = Hash.derived[Inner]
    val outer: Hash[Outer] = Hash.derived[Outer]
    val interleaved: Hash[Interleaved[Int]] = Hash.derived[Interleaved[Int]]
    val tree: Hash[Tree[Int]] = Hash.derived[Tree[Int]]
    val recursive: Hash[Recursive] = Hash.derived[Recursive]
    val enumK0: Hash[EnumK0] = Hash.derived[EnumK0]
    val singletons: Hash[Singletons[Int]] = Hash.derived[Singletons[Int]]
  }

  object orders {
    val inner: Order[Inner] = Order.derived[Inner]
    val outer: Order[Outer] = Order.derived[Outer]
    val interleaved: Order[Interleaved[Int]] = Order.derived[Interleaved[Int]]
    val recursive: Order[Recursive] = Order.derived[Recursive]
    val genericAdt: Order[GenericAdt[Int]] = Order.derived[GenericAdt[Int]]
    val enumK0: Order[EnumK0] = Order.derived[EnumK0]
    val singletons: Order[Singletons[Int]] = Order.derived[Singletons[Int]]
  }

  object partialOrders {
    val iList: PartialOrder[IList[Int]] = PartialOrder.derived[IList[Int]]
    val inner: PartialOrder[Inner] = PartialOrder.derived[Inner]
    val outer: PartialOrder[Outer] = PartialOrder.derived[Outer]
    val interleaved: PartialOrder[Interleaved[Int]] = PartialOrder.derived[Interleaved[Int]]
    val tree: PartialOrder[Tree[Int]] = PartialOrder.derived[Tree[Int]]
    val recursive: PartialOrder[Recursive] = PartialOrder.derived[Recursive]
    val boxKeyValue: PartialOrder[Box[KeyValue]] = PartialOrder.derived[Box[KeyValue]]
    val enumK0: PartialOrder[EnumK0] = PartialOrder.derived[EnumK0]
    val singletons: PartialOrder[Singletons[Int]] = PartialOrder.derived[Singletons[Int]]
  }

  object semigroups {
    val foo: Semigroup[Foo] = Semigroup.derived[Foo]
    val interleaved: Semigroup[Interleaved[Int]] = Semigroup.derived[Interleaved[Int]]
    val boxMul: Semigroup[Box[Mul]] = Semigroup.derived[Box[Mul]]
    val recursive: Semigroup[Recursive] = Semigroup.derived[Recursive]
  }

  object monoids {
    val foo: Monoid[Foo] = Monoid.derived[Foo]
    val interleaved: Monoid[Interleaved[Int]] = Monoid.derived[Interleaved[Int]]
    val boxMul: Monoid[Box[Mul]] = Monoid.derived[Box[Mul]]
    val recursive: Monoid[Recursive] = Monoid.derived[Recursive]
  }

  object commutativeSemigroups {
    val commutativeFoo: CommutativeSemigroup[CommutativeFoo] = CommutativeSemigroup.derived[CommutativeFoo]
    val recursive: CommutativeSemigroup[Recursive] = CommutativeSemigroup.derived[Recursive]
    val boxMul: CommutativeSemigroup[Box[Mul]] = CommutativeSemigroup.derived[Box[Mul]]
  }

  object commutativeMonoids {
    val commutativeFoo: CommutativeMonoid[CommutativeFoo] = CommutativeMonoid.derived[CommutativeFoo]
    val recursive: CommutativeMonoid[Recursive] = CommutativeMonoid.derived[Recursive]
    val boxMul: CommutativeMonoid[Box[Mul]] = CommutativeMonoid.derived[Box[Mul]]
  }

  object lattices {
    val band: Band[Masked[String]] = Band.derived[Masked[String]]
    val semilattice: Semilattice[Masked[String]] = Semilattice.derived[Masked[String]]
    val boundedSemilattice: BoundedSemilattice[Masked[String]] = BoundedSemilattice.derived[Masked[String]]
  }

  object groups {
    val slice: Group[Slice] = Group.derived[Slice]
    val compared: Group[Compared] = Group.derived[Compared]
    val commutativeSlice: CommutativeGroup[Slice] = CommutativeGroup.derived[Slice]
    val commutativeCompared: CommutativeGroup[Compared] = CommutativeGroup.derived[Compared]
  }
}
