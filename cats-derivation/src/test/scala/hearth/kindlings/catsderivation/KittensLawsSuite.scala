package hearth.kindlings.catsderivation

import cats.Eq
import cats.data.{NonEmptyList, OneAnd}
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Test.Parameters

import scala.concurrent.duration.*

/** Base of the suites running the discipline law checks of kittens' own test suites (the `Suite`s in
  * `core/src/test/scala-3/cats/derived`, kittens 3.5.0) on the instances Kindlings derives for kittens' test data types
  * ([[KittensAdts]]). Mirrors kittens' `KittensSuite`: the ScalaCheck parameters and the `Eq` instances the laws are
  * checked with (written by hand, like kittens' `ADTs.EqInstances`).
  */
abstract class KittensLawsSuite extends DisciplineSuite {
  import KittensAdts.*

  /** kittens uses `cats.platform.Platform.isJvm`, which is not accessible from here. */
  private val isJvm: Boolean =
    !Option(System.getProperty("java.vm.name")).exists(name => name.contains("Scala.js") || name.contains("Native"))

  override val scalaCheckTestParameters: Parameters = super.scalaCheckTestParameters
    .withMinSuccessfulTests(if (isJvm) 50 else 5)
    .withMaxDiscardRatio(if (isJvm) 5 else 50)
    .withMaxSize(if (isJvm) 10 else 5)
    .withMinSize(0)

  // Like kittens' KittensSuite: Arbitrary.arbContainer is not found for List[X] behind type aliases (ListSnoc, ...).
  implicit def arbitraryList[A: Arbitrary]: Arbitrary[List[A]] = Arbitrary(Gen.listOf(Arbitrary.arbitrary[A]))

  // The default Arbitrary[Duration] causes overflow.
  implicit val arbitraryDuration: Arbitrary[Duration] =
    Arbitrary(Gen.chooseNum(-750.days.toNanos, 750.days.toNanos).map(Duration.fromNanos))

  implicit val arbitrarySlice: Arbitrary[Slice] = Arbitrary(for {
    count <- Gen.chooseNum(-1000000L, 1000000L)
    percentile <- Gen.chooseNum(-1000.0, 1000.0)
    duration <- arbitraryDuration.arbitrary
  } yield Slice(count, percentile, duration))

  implicit val arbitraryCompared: Arbitrary[Compared] =
    Arbitrary(Gen.zip(arbitrarySlice.arbitrary, arbitrarySlice.arbitrary).map { case (x, y) => Compared(x, y) })

  // Some tolerance for numeric underflow.
  private def closeTo(x: Double, y: Double): Boolean = x == y || (x - y).abs < 1e-6
  implicit val eqSlice: Eq[Slice] = Eq.instance { (x, y) =>
    x.count == y.count && closeTo(x.percentile, y.percentile) && x.duration == y.duration
  }
  implicit val eqCompared: Eq[Compared] = Eq.instance((x, y) => eqSlice.eqv(x.x, y.x) && eqSlice.eqv(x.y, y.y))

  implicit def eqIList[A: Eq]: Eq[IList[A]] = Eq.by(IList.toList[A])
  implicit def eqICons[A: Eq]: Eq[ICons[A]] = Eq.by((cons: ICons[A]) => IList.toList[A](cons))
  implicit def eqSnoc[A: Eq]: Eq[Snoc[A]] = Eq.by(Snoc.toList[A])
  implicit def eqSCons[A: Eq]: Eq[SCons[A]] = Eq.by((cons: SCons[A]) => Snoc.toList[A](cons))
  implicit def eqTree[A: Eq]: Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match {
      case (Leaf(vx), Leaf(vy))         => Eq[A].eqv(vx, vy)
      case (Node(lx, rx), Node(ly, ry)) => eqv(lx, ly) && eqv(rx, ry)
      case _                            => false
    }
  }
  implicit def eqGenericAdt[A: Eq]: Eq[GenericAdt[A]] = Eq.by { case GenericAdtCase(value) => value }
  implicit def eqInterleaved[A: Eq]: Eq[Interleaved[A]] = Eq.by(x => (x.i, x.t, x.l, x.tt, x.s))
  implicit def eqBox[A: Eq]: Eq[Box[A]] = Eq.by(_.content)
  implicit def eqCaseClassWOption[A: Eq]: Eq[CaseClassWOption[A]] = Eq.by(_.value)
  implicit def eqUnCons[A: Eq]: Eq[UnCons[A]] = Eq.by(x => (x.head, x.tail))
  implicit def eqSingletons[A: Eq]: Eq[Singletons[A]] = Eq.by(_.value)
  implicit def eqNel[A: Eq]: Eq[Nel[A]] = Eq.by(x => (x.head, x.tail))
  implicit def eqZipper[A: Eq]: Eq[Zipper[A]] = Eq.by(x => (x.left, x.focus, x.right))
  implicit def eqSimple[A: Eq]: Eq[Simple[A]] = Eq.by(x => (x.value1, x.value2))
  implicit def eqMulK[A]: Eq[MulK[A]] = Eq.by(_.value)
  implicit def eqMasked[A]: Eq[Masked[A]] =
    Eq.instance((x, y) => x.mask == y.mask && Eq[Set[A]].eqv(x.values, y.values))
  implicit val eqMul: Eq[Mul] = Eq.fromUniversalEquals
  implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals
  implicit val eqCommutativeFoo: Eq[CommutativeFoo] = Eq.fromUniversalEquals
  implicit val eqRecursive: Eq[Recursive] = Eq.fromUniversalEquals
  implicit val eqEnumK0: Eq[EnumK0] = Eq.fromUniversalEquals

  implicit def eqEnumK1[A: Eq]: Eq[EnumK1[A]] = new Eq[EnumK1[A]] {
    def eqv(x: EnumK1[A], y: EnumK1[A]): Boolean = (x, y) match {
      case (EnumK1.Leaf(v1), EnumK1.Leaf(v2))       => Eq[A].eqv(v1, v2)
      case (EnumK1.Rec(l1, r1), EnumK1.Rec(l2, r2)) => eqv(l1, l2) && eqv(r1, r2)
      case _                                        => false
    }
  }
  implicit def eqEnumK1Contra[A](implicit fn: Eq[A => Unit]): Eq[EnumK1Contra[A]] = new Eq[EnumK1Contra[A]] {
    def eqv(x: EnumK1Contra[A], y: EnumK1Contra[A]): Boolean = (x, y) match {
      case (EnumK1Contra.Leaf(v1), EnumK1Contra.Leaf(v2))       => fn.eqv(v1, v2)
      case (EnumK1Contra.Rec(l1, r1), EnumK1Contra.Rec(l2, r2)) => eqv(l1, l2) && eqv(r1, r2)
      case _                                                    => false
    }
  }
  implicit def eqEnumK1Inv[A: Eq](implicit fn: Eq[A => Unit]): Eq[EnumK1Inv[A]] = new Eq[EnumK1Inv[A]] {
    def eqv(x: EnumK1Inv[A], y: EnumK1Inv[A]): Boolean = (x, y) match {
      case (EnumK1Inv.Leaf(c1, k1), EnumK1Inv.Leaf(c2, k2)) => Eq[A].eqv(c1, c2) && fn.eqv(k1, k2)
      case (EnumK1Inv.Rec(l1, r1), EnumK1Inv.Rec(l2, r2))   => eqv(l1, l2) && eqv(r1, r2)
      case _                                                => false
    }
  }
  implicit def eqMany[A: Eq]: Eq[Many[A]] = new Eq[Many[A]] {
    @scala.annotation.tailrec
    def eqv(x: Many[A], y: Many[A]): Boolean = (x, y) match {
      case (Many.Naught, Many.Naught)             => true
      case (Many.More(v1, r1), Many.More(v2, r2)) => Eq[A].eqv(v1, v2) && eqv(r1, r2)
      case _                                      => false
    }
  }
  implicit def eqAtMostOne[A: Eq]: Eq[AtMostOne[A]] = Eq.instance {
    case (AtMostOne.Naught, AtMostOne.Naught)         => true
    case (AtMostOne.Single(v1), AtMostOne.Single(v2)) => Eq[A].eqv(v1, v2)
    case _                                            => false
  }
  implicit def eqAtLeastOne[A: Eq]: Eq[AtLeastOne[A]] = new Eq[AtLeastOne[A]] {
    def eqv(x: AtLeastOne[A], y: AtLeastOne[A]): Boolean = (x, y) match {
      case (AtLeastOne.Single(v1), AtLeastOne.Single(v2))     => Eq[A].eqv(v1, v2)
      case (AtLeastOne.More(v1, r1), AtLeastOne.More(v2, r2)) =>
        Eq[A].eqv(v1, v2) && ((r1, r2) match {
          case (Some(a), Some(b)) => eqv(a, b)
          case (None, None)       => true
          case _                  => false
        })
      case _ => false
    }
  }
  implicit def eqSearch[A: Eq]: Eq[Search[A]] = new Eq[Search[A]] {
    def eqv(x: Search[A], y: Search[A]): Boolean =
      Eq[A].eqv(x.move, y.move) && ((x.child, y.child) match {
        case (Some(a), Some(b)) => eqv(a, b)
        case (None, None)       => true
        case _                  => false
      }) && x.variations.size == y.variations.size && x.variations.zip(y.variations).forall { case (a, b) =>
        eqv(a, b)
      }
  }
  implicit def eqResult[A: Eq, E: Eq]: Eq[Result[A, E]] = Eq.instance {
    case (Result.Done, Result.Done)                 => true
    case (Result.Maybe(e1), Result.Maybe(e2))       => Eq[Either[A, E]].eqv(e1, e2)
    case (Result.Ok(v1, c1), Result.Ok(v2, c2))     => c1 == c2 && Eq[A].eqv(v1, v2)
    case (Result.Err(e1, es1), Result.Err(e2, es2)) => Eq[E].eqv(e1, e2) && Eq[List[E]].eqv(es1, es2)
    case _                                          => false
  }

  implicit def arbitraryOneAnd[A: Arbitrary]: Arbitrary[OneAnd[Vector, A]] =
    Arbitrary(Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[Vector[A]]).map { case (h, t) => OneAnd(h, t) })
  implicit def arbitraryNel[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
    Arbitrary(Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[List[A]]).map { case (h, t) => NonEmptyList(h, t) })
  implicit def cogenMany[A: Cogen]: Cogen[Many[A]] = Cogen[List[A]].contramap { many =>
    Iterator
      .iterate(many) {
        case Many.More(_, rest) => rest
        case Many.Naught        => Many.Naught
      }
      .takeWhile(_ != Many.Naught)
      .collect { case Many.More(value, _) => value }
      .toList
  }
}
