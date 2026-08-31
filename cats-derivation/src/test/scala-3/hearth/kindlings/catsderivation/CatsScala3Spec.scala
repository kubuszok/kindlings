package hearth.kindlings.catsderivation

import hearth.MacroSuite
import hearth.kindlings.catsderivation.extensions.*

// Parameterless Scala 3 enum — singleton types erased at runtime.
// Regression test for kubuszok/kindlings#184: isInstanceOf on these types
// emits an "unchecked type test" warning that fails under -Werror.
enum Direction {
  case North, South, East, West
}

// Mixed Scala 3 enum — some cases are parameterless singletons, others are case classes.
enum TrafficLight {
  case Red
  case Yellow
  case Green(brightness: Int)
}

final class CatsScala3Spec extends MacroSuite {

  group("Eq for parameterless Scala 3 enum (#184)") {

    val eqDirection: cats.kernel.Eq[Direction] = cats.kernel.Eq.derived

    test("same singleton values are equal") {
      eqDirection.eqv(Direction.North, Direction.North) ==> true
    }

    test("different singleton values are not equal") {
      eqDirection.eqv(Direction.North, Direction.South) ==> false
    }

    test("all cases compared against each other") {
      val cases = List(Direction.North, Direction.South, Direction.East, Direction.West)
      for {
        a <- cases
        b <- cases
      }
        eqDirection.eqv(a, b) ==> (a == b)
    }
  }

  group("Order for parameterless Scala 3 enum (#184)") {

    val orderDirection: cats.kernel.Order[Direction] = cats.kernel.Order.derived

    test("same singleton compares as 0") {
      orderDirection.compare(Direction.North, Direction.North) ==> 0
    }

    test("different singletons have non-zero comparison") {
      assert(orderDirection.compare(Direction.North, Direction.South) != 0)
    }

    test("ordering is consistent across all cases") {
      val cases = List(Direction.North, Direction.South, Direction.East, Direction.West)
      for {
        a <- cases
        b <- cases
      } {
        val cmp = orderDirection.compare(a, b)
        if a == b then assertEquals(cmp, 0)
        else assert(cmp != 0, s"expected non-zero for $a vs $b")
      }
    }

    test("ordering is antisymmetric") {
      val cases = List(Direction.North, Direction.South, Direction.East, Direction.West)
      for {
        a <- cases
        b <- cases
      } {
        val ab = orderDirection.compare(a, b)
        val ba = orderDirection.compare(b, a)
        assertEquals(ab.sign, -ba.sign, s"antisymmetry violated for $a vs $b")
      }
    }
  }

  group("Eq for mixed Scala 3 enum (#184)") {

    val eqTL: cats.kernel.Eq[TrafficLight] = cats.kernel.Eq.derived

    test("same parameterless case") {
      eqTL.eqv(TrafficLight.Red, TrafficLight.Red) ==> true
    }

    test("different parameterless cases") {
      eqTL.eqv(TrafficLight.Red, TrafficLight.Yellow) ==> false
    }

    test("parameterless vs parameterized case") {
      eqTL.eqv(TrafficLight.Red, TrafficLight.Green(100)) ==> false
    }

    test("same parameterized case, same fields") {
      eqTL.eqv(TrafficLight.Green(100), TrafficLight.Green(100)) ==> true
    }

    test("same parameterized case, different fields") {
      eqTL.eqv(TrafficLight.Green(100), TrafficLight.Green(50)) ==> false
    }
  }

  group("Order for mixed Scala 3 enum (#184)") {

    val orderTL: cats.kernel.Order[TrafficLight] = cats.kernel.Order.derived

    test("same parameterless case compares as 0") {
      orderTL.compare(TrafficLight.Red, TrafficLight.Red) ==> 0
    }

    test("different parameterless cases have non-zero comparison") {
      assert(orderTL.compare(TrafficLight.Red, TrafficLight.Yellow) != 0)
    }

    test("parameterless vs parameterized case ordered by ordinal") {
      assert(orderTL.compare(TrafficLight.Red, TrafficLight.Green(100)) != 0)
    }

    test("same parameterized case, same fields compares as 0") {
      orderTL.compare(TrafficLight.Green(100), TrafficLight.Green(100)) ==> 0
    }

    test("same parameterized case, different fields orders by field") {
      assert(orderTL.compare(TrafficLight.Green(50), TrafficLight.Green(100)) < 0)
    }
  }
}
