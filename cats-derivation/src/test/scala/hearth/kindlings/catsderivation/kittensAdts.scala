package hearth.kindlings.catsderivation

import cats.{Eq, Eval}
import cats.data.{NonEmptyList, OneAnd}
import org.scalacheck.{Arbitrary, Cogen, Gen}

import scala.collection.immutable.BitSet
import scala.concurrent.duration.Duration

/** The data types of kittens' own test suites (`core/src/test/scala-3/cats/derived/ADTs.scala` and
  * `core/src/test/scala-2/cats/derived/adtdefns.scala`, kittens 3.5.0), with their generators, so that
  * `KittensLawsSpec` and friends can run the checks kittens runs on the instances Kindlings derives.
  *
  * Scala 3 enums of the upstream file (`EnumK0`, `EnumK1`, `Many`, `AtMostOne`, `AtLeastOne`, `Result`, ...) are sealed
  * traits here, so the suites cross-compile; `CatsScala3Spec` covers actual enums.
  */
object KittensAdts {

  sealed trait EnumK0
  object EnumK0 {
    final case class LeafS(value: String) extends EnumK0
    final case class LeafI(value: Int) extends EnumK0

    implicit val arbitrary: Arbitrary[EnumK0] = Arbitrary(
      Gen.oneOf(Arbitrary.arbitrary[String].map(LeafS(_)), Arbitrary.arbitrary[Int].map(LeafI(_)))
    )
    implicit val cogen: Cogen[EnumK0] = Cogen[Either[String, Int]].contramap {
      case LeafS(s) => Left(s)
      case LeafI(i) => Right(i)
    }
  }

  sealed trait EnumK1[A]
  object EnumK1 {
    final case class Leaf[A](value: A) extends EnumK1[A]
    final case class Rec[A](l: EnumK1[A], r: EnumK1[A]) extends EnumK1[A]

    implicit def arbitrary[A: Arbitrary]: Arbitrary[EnumK1[A]] = {
      def gen(depth: Int): Gen[EnumK1[A]] = {
        val leaf: Gen[EnumK1[A]] = Arbitrary.arbitrary[A].map(Leaf(_))
        if (depth <= 0) leaf
        else Gen.oneOf(leaf, Gen.zip(gen(depth - 1), gen(depth - 1)).map { case (l, r) => Rec(l, r) })
      }
      Arbitrary(Gen.choose(0, 3).flatMap(gen))
    }
  }

  sealed trait EnumK1Contra[-A]
  object EnumK1Contra {
    final case class Leaf[A](value: A => Unit) extends EnumK1Contra[A]
    final case class Rec[A](l: EnumK1Contra[A], r: EnumK1Contra[A]) extends EnumK1Contra[A]

    implicit def arbitrary[A](implicit f: Arbitrary[A => Unit]): Arbitrary[EnumK1Contra[A]] = {
      def gen(depth: Int): Gen[EnumK1Contra[A]] = {
        val leaf: Gen[EnumK1Contra[A]] = f.arbitrary.map(Leaf(_))
        if (depth <= 0) leaf
        else Gen.oneOf(leaf, Gen.zip(gen(depth - 1), gen(depth - 1)).map { case (l, r) => Rec(l, r) })
      }
      Arbitrary(Gen.choose(0, 3).flatMap(gen))
    }
  }

  sealed trait EnumK1Inv[A]
  object EnumK1Inv {
    final case class Leaf[A](cov: A, contra: A => Unit) extends EnumK1Inv[A]
    final case class Rec[A](l: EnumK1Inv[A], r: EnumK1Inv[A]) extends EnumK1Inv[A]

    implicit def arbitrary[A: Arbitrary](implicit f: Arbitrary[A => Unit]): Arbitrary[EnumK1Inv[A]] = {
      def gen(depth: Int): Gen[EnumK1Inv[A]] = {
        val leaf: Gen[EnumK1Inv[A]] = Gen.zip(Arbitrary.arbitrary[A], f.arbitrary).map { case (c, k) => Leaf(c, k) }
        if (depth <= 0) leaf
        else Gen.oneOf(leaf, Gen.zip(gen(depth - 1), gen(depth - 1)).map { case (l, r) => Rec(l, r) })
      }
      Arbitrary(Gen.choose(0, 3).flatMap(gen))
    }
  }

  sealed trait Rgb
  object Rgb {
    case object Red extends Rgb
    case object Green extends Rgb
    case object Blue extends Rgb
  }

  final case class ComplexProduct[T](lbl: String, set: Set[T], fns: Vector[() => T], opt: Eval[Option[T]])
  object ComplexProduct {
    implicit def arbitrary[T: Arbitrary]: Arbitrary[ComplexProduct[T]] = Arbitrary(for {
      lbl <- Arbitrary.arbitrary[String]
      set <- Arbitrary.arbitrary[Set[T]]
      vec <- Arbitrary.arbitrary[Vector[T]]
      opt <- Arbitrary.arbitrary[Option[T]]
    } yield ComplexProduct(lbl, set, vec.map(x => () => x), Eval.now(opt)))

    implicit def eqv[T: Eq]: Eq[ComplexProduct[T]] = Eq.instance { (x, y) =>
      x.lbl == y.lbl && Eq[Set[T]].eqv(x.set, y.set) &&
      Eq[Vector[T]].eqv(x.fns.map(_.apply()), y.fns.map(_.apply())) && Eq[Option[T]].eqv(x.opt.value, y.opt.value)
    }
  }

  final case class Box[+A](content: A)
  object Box {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Box[A]] = Arbitrary(Arbitrary.arbitrary[A].map(Box(_)))
    implicit def cogen[A: Cogen]: Cogen[Box[A]] = Cogen[A].contramap(_.content)
  }

  final case class Recursive(i: Int, is: Option[Recursive])
  object Recursive {
    implicit val arbitrary: Arbitrary[Recursive] = {
      def recursive(size: Int): Gen[Recursive] = for {
        i <- Arbitrary.arbitrary[Int]
        is <- if (size <= 0) Gen.const(None) else Gen.option(recursive(size / 2))
      } yield Recursive(i, is)
      Arbitrary(Gen.sized(recursive))
    }
    implicit val cogen: Cogen[Recursive] = Cogen[List[Int]].contramap { rec =>
      Iterator.iterate(Option(rec))(_.flatMap(_.is)).takeWhile(_.isDefined).flatten.map(_.i).toList
    }
  }

  final case class Interleaved[T](i: Int, t: T, l: Long, tt: Vector[T], s: String)
  object Interleaved {
    def empty[T](t: T): Interleaved[T] = Interleaved(0, t, 0, Vector.empty, "")

    implicit def arbitrary[T: Arbitrary]: Arbitrary[Interleaved[T]] = Arbitrary(for {
      i <- Arbitrary.arbitrary[Int]
      t <- Arbitrary.arbitrary[T]
      l <- Arbitrary.arbitrary[Long]
      tt <- Arbitrary.arbitrary[Vector[T]]
      s <- Arbitrary.arbitrary[String]
    } yield Interleaved(i, t, l, tt, s))
    implicit def cogen[T: Cogen]: Cogen[Interleaved[T]] =
      Cogen[(Int, T, Long, Vector[T], String)].contramap(x => (x.i, x.t, x.l, x.tt, x.s))
  }

  final case class Bivariant[A](run: A => Boolean, store: A)
  object Bivariant {
    implicit def arbitrary[A: Arbitrary](implicit f: Arbitrary[A => Boolean]): Arbitrary[Bivariant[A]] =
      Arbitrary(Gen.zip(f.arbitrary, Arbitrary.arbitrary[A]).map { case (run, store) => Bivariant(run, store) })
    implicit def eqv[A: Eq]: Eq[Bivariant[A]] = Eq.instance { (x, y) =>
      Eq[A].eqv(x.store, y.store) && x.run(x.store) == y.run(y.store)
    }
  }

  final case class Pred[A](run: A => Boolean)

  sealed trait IList[A]
  final case class ICons[A](head: A, tail: IList[A]) extends IList[A]
  final case class INil[A]() extends IList[A]

  object ICons {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[ICons[A]] =
      Arbitrary(Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[IList[A]]).map { case (h, t) => ICons(h, t) })
  }

  object IList {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[IList[A]] = Arbitrary(Arbitrary.arbitrary[List[A]].map(fromSeq))
    implicit def cogen[A: Cogen]: Cogen[IList[A]] = Cogen[List[A]].contramap(toList)
    def fromSeq[T](ts: Seq[T]): IList[T] = ts.foldRight[IList[T]](INil())(ICons(_, _))
    def toList[T](list: IList[T]): List[T] = {
      @scala.annotation.tailrec def loop(list: IList[T], acc: List[T]): List[T] = list match {
        case INil()            => acc.reverse
        case ICons(head, tail) => loop(tail, head :: acc)
      }
      loop(list, Nil)
    }
  }

  sealed trait Snoc[A]
  final case class SCons[A](init: Snoc[A], last: A) extends Snoc[A]
  final case class SNil[A]() extends Snoc[A]

  object Snoc {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Snoc[A]] = Arbitrary(Arbitrary.arbitrary[List[A]].map(fromSeq))
    def fromSeq[T](ts: Seq[T]): Snoc[T] = ts.foldLeft[Snoc[T]](SNil())(SCons(_, _))
    def toList[T](snoc: Snoc[T]): List[T] = {
      @scala.annotation.tailrec def loop(snoc: Snoc[T], acc: List[T]): List[T] = snoc match {
        case SNil()            => acc
        case SCons(init, last) => loop(init, last :: acc)
      }
      loop(snoc, Nil)
    }
  }

  object SCons {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[SCons[A]] =
      Arbitrary(Gen.zip(Arbitrary.arbitrary[Snoc[A]], Arbitrary.arbitrary[A]).map { case (i, l) => SCons(i, l) })
  }

  sealed trait Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Tree[A]] = {
      val leaf: Gen[Tree[A]] = Arbitrary.arbitrary[A].map(Leaf(_))
      def tree(maxDepth: Int): Gen[Tree[A]] =
        if (maxDepth <= 0) leaf else Gen.oneOf(leaf, node(maxDepth))
      def node(maxDepth: Int): Gen[Tree[A]] = for {
        depthL <- Gen.choose(0, maxDepth - 1)
        depthR <- Gen.choose(0, maxDepth - 1)
        left <- tree(depthL)
        right <- tree(depthR)
      } yield Node(left, right)
      Arbitrary(Gen.choose(0, 4).flatMap(tree))
    }
    implicit def cogen[A: Cogen]: Cogen[Tree[A]] = Cogen[List[A]].contramap(toList)
    def toList[A](tree: Tree[A]): List[A] = tree match {
      case Leaf(value)       => List(value)
      case Node(left, right) => toList(left) ::: toList(right)
    }
  }

  final case class Foo(i: Int, b: Option[String])
  object Foo {
    implicit val arbitrary: Arbitrary[Foo] =
      Arbitrary(Gen.zip(Arbitrary.arbitrary[Int], Arbitrary.arbitrary[Option[String]]).map { case (i, b) => Foo(i, b) })
    implicit val cogen: Cogen[Foo] = Cogen[(Int, Option[String])].contramap(x => (x.i, x.b))
  }

  final case class CommutativeFoo(i: Int, b: Option[Long])
  object CommutativeFoo {
    implicit val arbitrary: Arbitrary[CommutativeFoo] = Arbitrary(
      Gen.zip(Arbitrary.arbitrary[Int], Arbitrary.arbitrary[Option[Long]]).map { case (i, b) => CommutativeFoo(i, b) }
    )
  }

  final case class Inner(i: Int)
  final case class Outer(in: Inner)
  object Inner {
    implicit val arbitrary: Arbitrary[Inner] = Arbitrary(Arbitrary.arbitrary[Int].map(Inner(_)))
    implicit val cogen: Cogen[Inner] = Cogen[Int].contramap(_.i)
  }
  object Outer {
    implicit val arbitrary: Arbitrary[Outer] = Arbitrary(Arbitrary.arbitrary[Inner].map(Outer(_)))
    implicit val cogen: Cogen[Outer] = Cogen[Inner].contramap(_.in)
  }

  sealed trait IntTree
  final case class IntLeaf(t: Int) extends IntTree
  final case class IntNode(l: IntTree, r: IntTree) extends IntTree

  sealed trait GenericAdt[A]
  final case class GenericAdtCase[A](value: Option[A]) extends GenericAdt[A]
  object GenericAdt {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[GenericAdt[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(GenericAdtCase(_)))
    implicit def cogen[A: Cogen]: Cogen[GenericAdt[A]] =
      Cogen[Option[A]].contramap { case GenericAdtCase(value) => value }
  }

  final case class CaseClassWOption[A](value: Option[A])
  object CaseClassWOption {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[CaseClassWOption[A]] =
      Arbitrary(Arbitrary.arbitrary[Option[A]].map(CaseClassWOption(_)))
  }

  final case class UnCons[+A](head: Option[A], tail: List[A])
  object UnCons {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[UnCons[A]] = Arbitrary(
      Gen.zip(Arbitrary.arbitrary[Option[A]], Arbitrary.arbitrary[List[A]]).map { case (h, t) => UnCons(h, t) }
    )
  }

  final case class First(value: String)
  final case class Second(value: String)
  final case class Middle(first: First, second: Option[Second])
  final case class Top(middle: Middle)
  final case class TopK[A](middle: Middle, k: CaseClassWOption[A])

  final case class Address(street: String, city: String, state: String)
  final case class ContactInfo(phoneNumber: String, address: Address)
  final case class People(name: String, contactInfo: ContactInfo)

  final case class ListField(a: String, b: List[ListFieldChild])
  final case class ListFieldChild(c: Int)

  final case class Singletons[A](value: A, str: "Scala" = "Scala", lng: 42L = 42L, dbl: 3.14 = 3.14)
  object Singletons {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Singletons[A]] =
      Arbitrary(Arbitrary.arbitrary[A].map(Singletons(_)))
    implicit def cogen[A: Cogen]: Cogen[Singletons[A]] = Cogen[A].contramap(_.value)
  }

  sealed trait Many[+A]
  object Many {
    case object Naught extends Many[Nothing]
    final case class More[+A](value: A, rest: Many[A]) extends Many[A]

    implicit def arbitrary[A: Arbitrary]: Arbitrary[Many[A]] = Arbitrary(
      Arbitrary.arbitrary[List[A]].map(_.foldRight[Many[A]](Naught)(More(_, _)))
    )
  }

  sealed trait AtMostOne[+A]
  object AtMostOne {
    case object Naught extends AtMostOne[Nothing]
    final case class Single[+A](value: A) extends AtMostOne[A]

    implicit def arbitrary[A: Arbitrary]: Arbitrary[AtMostOne[A]] = Arbitrary(
      Gen.oneOf(Gen.const[AtMostOne[A]](Naught), Arbitrary.arbitrary[A].map(Single(_)))
    )
  }

  sealed trait AtLeastOne[+A]
  object AtLeastOne {
    final case class Single[+A](value: A) extends AtLeastOne[A]
    final case class More[+A](value: A, rest: Option[AtLeastOne[A]]) extends AtLeastOne[A]

    implicit def arbitrary[A: Arbitrary]: Arbitrary[AtLeastOne[A]] = {
      def gen(depth: Int): Gen[AtLeastOne[A]] = {
        val single: Gen[AtLeastOne[A]] = Arbitrary.arbitrary[A].map(Single(_))
        if (depth <= 0) single
        else
          Gen.oneOf(single, Gen.zip(Arbitrary.arbitrary[A], Gen.option(gen(depth - 1))).map { case (v, r) => More(v, r) })
      }
      Arbitrary(Gen.choose(0, 4).flatMap(gen))
    }
  }

  final case class Search[+A](move: A, child: Option[Search[A]], variations: List[Search[A]])
  object Search {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Search[A]] = {
      def gen(depth: Int): Gen[Search[A]] = for {
        move <- Arbitrary.arbitrary[A]
        child <- if (depth <= 0) Gen.const(None) else Gen.option(gen(depth - 1))
        variations <- if (depth <= 0) Gen.const(Nil) else Gen.choose(0, 2).flatMap(Gen.listOfN(_, gen(depth - 1)))
      } yield Search(move, child, variations)
      Arbitrary(Gen.choose(0, 2).flatMap(gen))
    }
  }

  final case class Slice(count: Long, percentile: Double, duration: Duration)
  final case class Compared(x: Slice, y: Slice)

  final case class Masked[A](mask: BitSet, values: Set[A])
  object Masked {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Masked[A]] = Arbitrary(for {
      mask <- Gen.listOf(Gen.oneOf(Gen.const(0), Gen.choose(1, 100))).map(xs => BitSet(xs*))
      values <- Arbitrary.arbitrary[Set[A]]
    } yield Masked(mask, values))
  }

  sealed trait Result[+A, +E]
  object Result {
    case object Done extends Result[Nothing, Nothing]
    final case class Maybe[+A, +E](either: Either[A, E]) extends Result[A, E]
    final case class Ok[+A](value: A, code: Int) extends Result[A, Nothing]
    final case class Err[+E](err: E, errs: List[E]) extends Result[Nothing, E]

    implicit def arbitrary[A: Arbitrary, E: Arbitrary]: Arbitrary[Result[A, E]] = Arbitrary(
      Gen.oneOf[Result[A, E]](
        Gen.const(Done),
        Arbitrary.arbitrary[Either[A, E]].map(Maybe(_)),
        Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[Int]).map { case (v, c) => Ok(v, c) },
        Gen.zip(Arbitrary.arbitrary[E], Arbitrary.arbitrary[List[E]]).map { case (e, es) => Err(e, es) }
      )
    )
  }

  // --- fixtures declared next to kittens' individual suites ---

  /** `CommutativeMonoidSuite.Mul` & co: a type with its own instance that the derivation has to respect. */
  final case class Mul(value: Int)
  object Mul {
    implicit val commutativeMonoid: cats.kernel.CommutativeMonoid[Mul] = new cats.kernel.CommutativeMonoid[Mul] {
      val empty: Mul = Mul(1)
      def combine(x: Mul, y: Mul): Mul = Mul(x.value * y.value)
    }
    implicit val arbitrary: Arbitrary[Mul] = Arbitrary(Arbitrary.arbitrary[Int].map(Mul(_)))
  }

  /** `MonoidKSuite.Mul` / `SemigroupKSuite.Mul`. */
  final case class MulK[T](value: Int)
  object MulK {
    implicit val monoidK: cats.MonoidK[MulK] = new cats.MonoidK[MulK] {
      def empty[A]: MulK[A] = MulK(1)
      def combineK[A](x: MulK[A], y: MulK[A]): MulK[A] = MulK(x.value * y.value)
    }
    implicit def arbitrary[T]: Arbitrary[MulK[T]] = Arbitrary(Arbitrary.arbitrary[Int].map(MulK(_)))
  }

  /** `PartialOrderSuite.KeyValue`. */
  final case class KeyValue(key: String, value: Int)
  object KeyValue {
    implicit val partialOrder: cats.kernel.PartialOrder[KeyValue] =
      cats.kernel.PartialOrder.from((x, y) =>
        if (x.key == y.key) x.value.toDouble - y.value.toDouble else Double.NaN
      )
    implicit val arbitrary: Arbitrary[KeyValue] = Arbitrary(
      Gen.zip(Gen.oneOf("a", "b", "c"), Gen.choose(-3, 3)).map { case (k, v) => KeyValue(k, v) }
    )
    implicit val cogen: Cogen[KeyValue] = Cogen[(String, Int)].contramap(x => (x.key, x.value))
  }

  /** `EmptyKSuite.Color` / `PureSuite.Color`. */
  final case class Color[A](r: Int, g: Int, b: Int)
  object Color {
    implicit val emptyK: alleycats.EmptyK[Color] = new alleycats.EmptyK[Color] {
      def empty[A]: Color[A] = Color(255, 255, 255)
    }
    implicit val pure: alleycats.Pure[Color] = new alleycats.Pure[Color] {
      def pure[A](value: A): Color[A] = Color(255, 255, 255)
    }
  }

  /** `EmptySuite.Mask` / `EmptySuite.Chain` / `EmptySuite.Dummy`. */
  final case class Mask(bits: Int)
  object Mask {
    implicit val empty: alleycats.Empty[Mask] = alleycats.Empty(Mask(0xffffffff))
  }
  final case class Chain(head: Int, tail: Chain)
  trait Dummy

  /** `FoldableSuite.Nel`: its `Foldable` deliberately skips the head, to show that the instance in scope is used. */
  final case class Nel[+A](head: A, tail: List[A])
  object Nel {
    implicit val foldable: cats.Foldable[Nel] = new cats.Foldable[Nel] {
      def foldLeft[A, B](fa: Nel[A], b: B)(f: (B, A) => B): B = fa.tail.foldLeft(b)(f)
      def foldRight[A, B](fa: Nel[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        cats.Foldable[List].foldRight(fa.tail, lb)(f)
    }
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Nel[A]] =
      Arbitrary(Gen.zip(Arbitrary.arbitrary[A], Arbitrary.arbitrary[List[A]]).map { case (h, t) => Nel(h, t) })
  }

  /** `ReducibleSuite.Zipper`: its `Reducible` deliberately skips `left`. */
  final case class Zipper[+A](left: List[A], focus: A, right: List[A])
  object Zipper {
    implicit val reducible: cats.Reducible[Zipper] = new cats.Reducible[Zipper] {
      def reduceLeftTo[A, B](fa: Zipper[A])(f: A => B)(g: (B, A) => B): B =
        cats.Reducible[NonEmptyList].reduceLeftTo(NonEmptyList(fa.focus, fa.right))(f)(g)
      def reduceRightTo[A, B](fa: Zipper[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        cats.Reducible[NonEmptyList].reduceRightTo(NonEmptyList(fa.focus, fa.right))(f)(g)
      def foldLeft[A, B](fa: Zipper[A], b: B)(f: (B, A) => B): B = (fa.focus :: fa.right).foldLeft(b)(f)
      def foldRight[A, B](fa: Zipper[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        cats.Foldable[List].foldRight(fa.focus :: fa.right, lb)(f)
    }
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Zipper[A]] = Arbitrary(for {
      left <- Arbitrary.arbitrary[List[A]]
      focus <- Arbitrary.arbitrary[A]
      right <- Arbitrary.arbitrary[List[A]]
    } yield Zipper(left, focus, right))
  }

  /** `MonoidKSuite.derivedInstances.Simple`. */
  final case class Simple[A](value1: List[A], value2: Set[A])
  object Simple {
    implicit def arbitrary[A: Arbitrary]: Arbitrary[Simple[A]] = Arbitrary(
      Gen.zip(Arbitrary.arbitrary[List[A]], Arbitrary.arbitrary[Set[A]]).map { case (l, s) => Simple(l, s) }
    )
  }

  // --- the type aliases kittens derives instances for (composition of type constructors, tuples, functions) ---

  type OptList[A] = Option[List[A]]
  type ListSnoc[A] = List[Snoc[A]]
  type AndChar[A] = (A, Char)
  type AndInt[A] = (A, Int)
  type ListBox[A] = List[Box[A]]
  type BoxNel[A] = Box[Nel[A]]
  type BoxZipper[A] = Box[Zipper[A]]
  type Predicate[A] = A => Boolean
  type NestedPred[A] = Predicate[Predicate[A]]
  type OptPred[A] = Option[A => Boolean]
  type ListPred[A] = List[A => Boolean]
  type GenericAdtPred[A] = GenericAdt[A => Boolean]
  type ListSnocF[A] = List[Snoc[A => Int]]
  type InterleavedPred[A] = Interleaved[A => Boolean]
  type AndCharPred[A] = (A => Boolean, Char)
  type TreePred[A] = Tree[A => Boolean]
  type LOption[A] = List[Option[A]]
  type PList[A] = (List[A], List[A])
  type NelOption[A] = NonEmptyList[Option[A]]
  type BoxColor[A] = Box[Color[A]]
  type BoxMulK[A] = Box[MulK[A]]
  type NelSCons[A] = NonEmptyList[SCons[A]]
  type NelAndOne[A] = NonEmptyList[OneAnd[Vector, A]]
  type VecAndNel[A] = (Vector[A], NonEmptyList[A])
  type NestedEither[A, B] = Either[Option[A], Either[A, B]]
}
