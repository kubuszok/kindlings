package hearth.kindlings.catsderivation

import hearth.MacroSuite

final class CatsDerivationSpec extends MacroSuite {

  group("Show") {

    test("case class") {
      examples.Person.showPerson.show(examples.Person("Alice", 30)) ==> "Person(name = Alice, age = 30)"
    }

    test("empty case class") {
      examples.Empty.showEmpty.show(examples.Empty()) ==> "Empty()"
    }
  }

  group("Eq") {

    test("equal case classes") {
      examples.Person.eqPerson.eqv(examples.Person("Alice", 30), examples.Person("Alice", 30)) ==> true
    }

    test("unequal case classes") {
      examples.Person.eqPerson.eqv(examples.Person("Alice", 30), examples.Person("Bob", 25)) ==> false
    }

    test("empty case class") {
      examples.Empty.eqEmpty.eqv(examples.Empty(), examples.Empty()) ==> true
    }
  }

  group("Order") {

    test("equal points") {
      examples.Point.orderPoint.compare(examples.Point(1, 2), examples.Point(1, 2)) ==> 0
    }

    test("first field differs") {
      assert(examples.Point.orderPoint.compare(examples.Point(1, 2), examples.Point(2, 2)) < 0)
    }

    test("second field differs") {
      assert(examples.Point.orderPoint.compare(examples.Point(1, 3), examples.Point(1, 2)) > 0)
    }
  }

  group("Hash") {

    test("equal values have equal hashes") {
      val a = examples.Point(1, 2)
      val b = examples.Point(1, 2)
      examples.Point.hashPoint.hash(a) ==> examples.Point.hashPoint.hash(b)
    }

    test("eqv delegates correctly") {
      examples.Point.hashPoint.eqv(examples.Point(1, 2), examples.Point(1, 2)) ==> true
      examples.Point.hashPoint.eqv(examples.Point(1, 2), examples.Point(3, 4)) ==> false
    }
  }

  group("Semigroup") {

    test("combines fields") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.semigroupPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Monoid") {

    test("empty") {
      examples.Point.monoidPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.monoidPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("CommutativeSemigroup") {

    test("combines fields") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.commSemigroupPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("CommutativeMonoid") {

    test("empty") {
      examples.Point.commMonoidPoint.empty ==> examples.Point(0, 0)
    }

    test("combine") {
      val a = examples.Point(1, 2)
      val b = examples.Point(3, 4)
      examples.Point.commMonoidPoint.combine(a, b) ==> examples.Point(4, 6)
    }
  }

  group("Show enum") {

    test("sealed trait with case classes") {
      examples.Shape.showShape.show(examples.Circle(1.5)) ==> "Circle(radius = 1.5)"
      examples.Shape.showShape.show(examples.Rectangle(3.0, 4.0)) ==> "Rectangle(width = 3.0, height = 4.0)"
    }

    test("sealed trait with case objects") {
      examples.Color.showColor.show(examples.Red) ==> "Red()"
      examples.Color.showColor.show(examples.Green) ==> "Green()"
    }
  }

  group("Eq enum") {

    test("same case class values") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Circle(1.5)) ==> true
    }

    test("different case class values") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Circle(2.0)) ==> false
    }

    test("different cases") {
      examples.Shape.eqShape.eqv(examples.Circle(1.5), examples.Rectangle(1.5, 1.5)) ==> false
    }

    test("case objects equal") {
      examples.Color.eqColor.eqv(examples.Red, examples.Red) ==> true
    }

    test("case objects not equal") {
      examples.Color.eqColor.eqv(examples.Red, examples.Blue) ==> false
    }
  }

  group("Order enum") {

    test("same case") {
      examples.Shape.orderShape.compare(examples.Circle(1.5), examples.Circle(1.5)) ==> 0
    }

    test("same case different values") {
      assert(examples.Shape.orderShape.compare(examples.Circle(1.0), examples.Circle(2.0)) < 0)
    }

    test("different cases are ordered") {
      // Different cases should produce non-zero comparison
      assert(examples.Shape.orderShape.compare(examples.Circle(1.0), examples.Rectangle(1.0, 1.0)) != 0)
    }

    test("case objects ordered") {
      examples.Color.orderColor.compare(examples.Red, examples.Red) ==> 0
      assert(examples.Color.orderColor.compare(examples.Red, examples.Green) != 0)
    }
  }

  group("Hash enum") {

    test("same values same hash") {
      examples.Shape.hashShape.hash(examples.Circle(1.5)) ==> examples.Shape.hashShape.hash(examples.Circle(1.5))
    }

    test("eqv for enum") {
      examples.Shape.hashShape.eqv(examples.Circle(1.5), examples.Circle(1.5)) ==> true
      examples.Shape.hashShape.eqv(examples.Circle(1.5), examples.Rectangle(1.5, 1.5)) ==> false
    }

    test("case objects same hash") {
      examples.Color.hashColor.hash(examples.Red) ==> examples.Color.hashColor.hash(examples.Red)
    }

    test("case objects eqv") {
      examples.Color.hashColor.eqv(examples.Red, examples.Red) ==> true
      examples.Color.hashColor.eqv(examples.Red, examples.Blue) ==> false
    }
  }

  group("Empty") {

    test("case class empty") {
      val empty = examples.Point.emptyPoint.empty
      empty ==> examples.Point(0, 0)
    }
  }
}
