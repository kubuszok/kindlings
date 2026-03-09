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
