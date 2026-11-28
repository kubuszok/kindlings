package hearth.kindlings.catsderivation

import hearth.MacroSuite
import hearth.kindlings.catsderivation.extensions.*

final case class CatsPrivate(a: Int, private val b: String, c: Int) {
  def bValue: String = b
}

final case class CatsPrivateBox[A](a: A, private val b: String, c: A) {
  def bValue: String = b
}

object CatsPrivate {
  val show: cats.Show[CatsPrivate] = cats.Show.derived
  val eq: cats.kernel.Eq[CatsPrivate] = cats.kernel.Eq.derived
  val order: cats.kernel.Order[CatsPrivate] = cats.kernel.Order.derived
  val hash: cats.kernel.Hash[CatsPrivate] = cats.kernel.Hash.derived
  val semigroup: cats.kernel.Semigroup[CatsPrivate] = cats.kernel.Semigroup.derived
  val monoid: cats.kernel.Monoid[CatsPrivate] = cats.kernel.Monoid.derived
  val functor: cats.Functor[CatsPrivateBox] = cats.Functor.derived
  val foldable: cats.Foldable[CatsPrivateBox] = cats.Foldable.derived
  val traverse: cats.Traverse[CatsPrivateBox] = cats.Traverse.derived
}

// Hearth's `caseFieldValuesAt` has no `private val` constructor parameters (they have no accessor the generated code could
// call): derivations which read fields include them anyway (through `Product.productElement`), matched by name, so the
// fields after them are not shifted.
final class PrivateFieldsSpec extends MacroSuite {

  private val x = CatsPrivate(1, "two", 3)
  private val y = CatsPrivate(1, "zwei", 3)

  test("Show includes private val constructor fields, as kittens") {
    CatsPrivate.show.show(x) ==> "CatsPrivate(a = 1, b = two, c = 3)"
  }

  test("Eq, Order and Hash compare private val constructor fields") {
    CatsPrivate.eq.eqv(x, x) ==> true
    CatsPrivate.eq.eqv(x, y) ==> false
    assert(CatsPrivate.order.compare(x, y) < 0)
    assert(CatsPrivate.order.compare(CatsPrivate(1, "two", 3), CatsPrivate(1, "two", 4)) < 0)
    assert(CatsPrivate.hash.hash(x) != CatsPrivate.hash.hash(y))
  }

  test("Semigroup and Monoid combine private val constructor fields") {
    CatsPrivate.semigroup.combine(x, y) ==> CatsPrivate(2, "twozwei", 6)
    CatsPrivate.monoid.empty ==> CatsPrivate(0, "", 0)
  }

  test("Functor, Foldable and Traverse keep private val constructor fields in their places") {
    val box = CatsPrivateBox(1, "two", 3)
    CatsPrivate.functor.map(box)(_ * 10) ==> CatsPrivateBox(10, "two", 30)
    CatsPrivate.functor.map(box)(_ * 10).bValue ==> "two"
    CatsPrivate.foldable.foldLeft(box, 0)(_ + _) ==> 4
    CatsPrivate.traverse.traverse[Option, Int, Int](box)(i => Some(i + 1)) ==> Some(CatsPrivateBox(2, "two", 4))
  }
}
