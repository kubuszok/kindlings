package hearth.kindlings.catsderivation

import hearth.kindlings.catsderivation.extensions.*

object examples {

  final case class Point(x: Int, y: Int)
  object Point {
    implicit val showPoint: cats.Show[Point] = cats.Show.derived
    implicit val eqPoint: cats.kernel.Eq[Point] = cats.kernel.Eq.derived
    implicit val orderPoint: cats.kernel.Order[Point] = cats.kernel.Order.derived
    implicit val hashPoint: cats.kernel.Hash[Point] = cats.kernel.Hash.derived
    implicit val semigroupPoint: cats.kernel.Semigroup[Point] = cats.kernel.Semigroup.derived
    implicit val monoidPoint: cats.kernel.Monoid[Point] = cats.kernel.Monoid.derived
    implicit val commSemigroupPoint: cats.kernel.CommutativeSemigroup[Point] =
      cats.kernel.CommutativeSemigroup.derived
    implicit val commMonoidPoint: cats.kernel.CommutativeMonoid[Point] = cats.kernel.CommutativeMonoid.derived
    implicit val emptyPoint: alleycats.Empty[Point] = alleycats.Empty.derived
  }

  final case class Person(name: String, age: Int)
  object Person {
    implicit val showPerson: cats.Show[Person] = cats.Show.derived
    implicit val eqPerson: cats.kernel.Eq[Person] = cats.kernel.Eq.derived
  }

  final case class Wrapper(value: String)
  object Wrapper {
    implicit val showWrapper: cats.Show[Wrapper] = cats.Show.derived
  }

  final case class Empty()
  object Empty {
    implicit val showEmpty: cats.Show[Empty] = cats.Show.derived
    implicit val eqEmpty: cats.kernel.Eq[Empty] = cats.kernel.Eq.derived
  }

  // Sealed trait (enum) examples
  sealed trait Shape
  final case class Circle(radius: Double) extends Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  object Shape {
    implicit val showShape: cats.Show[Shape] = cats.Show.derived
    implicit val eqShape: cats.kernel.Eq[Shape] = cats.kernel.Eq.derived
    implicit val orderShape: cats.kernel.Order[Shape] = cats.kernel.Order.derived
    implicit val hashShape: cats.kernel.Hash[Shape] = cats.kernel.Hash.derived
  }

  // Parameterized case classes for Functor/Apply/Applicative/Foldable derivation
  final case class Box[A](value: A)
  object Box {
    implicit val functorBox: cats.Functor[Box] = cats.Functor.derived
    implicit val applicativeBox: cats.Applicative[Box] = cats.Applicative.derived
    implicit val foldableBox: cats.Foldable[Box] = cats.Foldable.derived
    implicit val traverseBox: cats.Traverse[Box] = cats.Traverse.derived
    implicit val reducibleBox: cats.Reducible[Box] = cats.Reducible.derived
    implicit val nonEmptyTraverseBox: cats.NonEmptyTraverse[Box] = cats.NonEmptyTraverse.derived
  }

  final case class PairF[A](first: A, second: A)
  object PairF {
    implicit val functorPairF: cats.Functor[PairF] = cats.Functor.derived
    implicit val foldablePairF: cats.Foldable[PairF] = cats.Foldable.derived
    implicit val traversePairF: cats.Traverse[PairF] = cats.Traverse.derived
    implicit val reduciblePairF: cats.Reducible[PairF] = cats.Reducible.derived
    implicit val nonEmptyTraversePairF: cats.NonEmptyTraverse[PairF] = cats.NonEmptyTraverse.derived
  }

  final case class Labeled[A](value: A, label: String)
  object Labeled {
    implicit val functorLabeled: cats.Functor[Labeled] = cats.Functor.derived
    implicit val applyLabeled: cats.Apply[Labeled] = cats.Apply.derived
    implicit val applicativeLabeled: cats.Applicative[Labeled] = cats.Applicative.derived
    implicit val foldableLabeled: cats.Foldable[Labeled] = cats.Foldable.derived
    implicit val traverseLabeled: cats.Traverse[Labeled] = cats.Traverse.derived
  }

  // Parameterized case classes for Contravariant derivation
  final case class Predicate[A](run: A => Boolean)
  object Predicate {
    implicit val contravariantPredicate: cats.Contravariant[Predicate] = cats.Contravariant.derived
  }

  final case class Codec[A](encode: A => String, label: String)
  object Codec {
    implicit val contravariantCodec: cats.Contravariant[Codec] = cats.Contravariant.derived
  }

  // Parameterized case classes for Invariant derivation
  final case class Codec2[A](encode: A => String, decode: String => A)
  object Codec2 {
    implicit val invariantCodec2: cats.Invariant[Codec2] = cats.Invariant.derived
  }

  final case class Transform[A](value: A, check: A => Boolean)
  object Transform {
    implicit val invariantTransform: cats.Invariant[Transform] = cats.Invariant.derived
  }

  // Parameterized case classes for Pure derivation
  final case class Single[A](value: A)
  object Single {
    implicit val pureSingle: alleycats.Pure[Single] = alleycats.Pure.derived
  }

  final case class Triple[A](a: A, b: A, c: A)
  object Triple {
    implicit val pureTriple: alleycats.Pure[Triple] = alleycats.Pure.derived
  }

  // Parameterized case classes for EmptyK/SemigroupK/MonoidK derivation
  // Fields must be wrapped in type constructors with existing instances (List, Option, etc.)
  // because bare A fields have no Empty[Any]/Semigroup[Any]/Monoid[Any]
  final case class ListPair[A](xs: List[A], ys: List[A])
  object ListPair {
    implicit val emptyKListPair: alleycats.EmptyK[ListPair] = alleycats.EmptyK.derived
    implicit val semigroupKListPair: cats.SemigroupK[ListPair] = cats.SemigroupK.derived
    implicit val monoidKListPair: cats.MonoidK[ListPair] = cats.MonoidK.derived
  }

  final case class TaggedList[A](items: List[A], name: String)
  object TaggedList {
    implicit val emptyKTaggedList: alleycats.EmptyK[TaggedList] = alleycats.EmptyK.derived
    implicit val semigroupKTaggedList: cats.SemigroupK[TaggedList] = cats.SemigroupK.derived
    implicit val monoidKTaggedList: cats.MonoidK[TaggedList] = cats.MonoidK.derived
  }

  // Const-like type for NonEmptyAlternative/Alternative derivation
  // All fields must have Monoid instances (no direct A fields) because combineK needs Semigroup for all field types
  final case class Const[A](value: String)
  object Const {
    implicit val neaConst: cats.NonEmptyAlternative[Const] = cats.NonEmptyAlternative.derived
    implicit val altConst: cats.Alternative[Const] = cats.Alternative.derived
  }

  // Multi-field Const-like type
  final case class Counter[A](count: Int, label: String)
  object Counter {
    implicit val neaCounter: cats.NonEmptyAlternative[Counter] = cats.NonEmptyAlternative.derived
    implicit val altCounter: cats.Alternative[Counter] = cats.Alternative.derived
  }

  // Parameterized case classes for ConsK derivation
  // ConsK requires a container field (G[A] with ConsK[G]) to absorb the consed element
  final case class ListWrap[A](items: List[A])
  object ListWrap {
    implicit val consKListWrap: alleycats.ConsK[ListWrap] = alleycats.ConsK.derived
  }

  // Head + tail pattern: direct A field followed by nested container
  final case class HeadTail[A](head: A, tail: List[A])
  object HeadTail {
    implicit val consKHeadTail: alleycats.ConsK[HeadTail] = alleycats.ConsK.derived
  }

  // Container with invariant field
  final case class NamedList[A](items: List[A], name: String)
  object NamedList {
    implicit val consKNamedList: alleycats.ConsK[NamedList] = alleycats.ConsK.derived
  }

  // Sealed trait with case objects only
  sealed trait Color
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
  object Color {
    implicit val showColor: cats.Show[Color] = cats.Show.derived
    implicit val eqColor: cats.kernel.Eq[Color] = cats.kernel.Eq.derived
    implicit val orderColor: cats.kernel.Order[Color] = cats.kernel.Order.derived
    implicit val hashColor: cats.kernel.Hash[Color] = cats.kernel.Hash.derived
  }
}
