package hearth.kindlings.diffderivation

import hearth.MacroSuite

// Parameterless Scala 3 enum — singleton types erased at runtime.
// Regression test for kubuszok/kindlings#184: isInstanceOf on these types
// emits an "unchecked type test" warning that fails under -Werror.
enum Season {
  case Spring, Summer, Autumn, Winter
}

// Mixed Scala 3 enum — some cases are parameterless singletons, others are case classes.
enum Outcome {
  case Success
  case Failure(reason: String)
}

final class DiffScala3Spec extends MacroSuite {

  group("Diff for parameterless Scala 3 enum (#184)") {

    val diffSeason: Diff[Season] = Diff.derived

    test("identical singletons") {
      val result = diffSeason.diff(Season.Spring, Season.Spring)
      assert(result.isIdentical, s"expected identical, got $result")
    }

    test("different singletons") {
      val result = diffSeason.diff(Season.Spring, Season.Winter)
      assert(!result.isIdentical, s"expected not identical, got $result")
    }

    test("all cases identical with themselves") {
      val cases = List(Season.Spring, Season.Summer, Season.Autumn, Season.Winter)
      for (c <- cases) {
        val result = diffSeason.diff(c, c)
        assert(result.isIdentical, s"expected identical for $c, got $result")
      }
    }

    test("all pairs of different cases are not identical") {
      val cases = List(Season.Spring, Season.Summer, Season.Autumn, Season.Winter)
      for {
        a <- cases
        b <- cases
        if a != b
      } {
        val result = diffSeason.diff(a, b)
        assert(!result.isIdentical, s"expected not identical for $a vs $b, got $result")
      }
    }

    test("snapshot of singleton") {
      val result = diffSeason.snapshot(Season.Summer)
      assert(result.isIdentical, s"snapshot should be identical, got $result")
    }
  }

  group("Diff for mixed Scala 3 enum (#184)") {

    val diffOutcome: Diff[Outcome] = Diff.derived

    test("identical parameterless singletons") {
      val result = diffOutcome.diff(Outcome.Success, Outcome.Success)
      assert(result.isIdentical, s"expected identical, got $result")
    }

    test("parameterless vs parameterized case") {
      val result = diffOutcome.diff(Outcome.Success, Outcome.Failure("oops"))
      assert(!result.isIdentical, s"expected not identical, got $result")
    }

    test("parameterized vs parameterless case") {
      val result = diffOutcome.diff(Outcome.Failure("oops"), Outcome.Success)
      assert(!result.isIdentical, s"expected not identical, got $result")
    }

    test("same parameterized case, same fields") {
      val result = diffOutcome.diff(Outcome.Failure("oops"), Outcome.Failure("oops"))
      assert(result.isIdentical, s"expected identical, got $result")
    }

    test("same parameterized case, different fields") {
      val result = diffOutcome.diff(Outcome.Failure("oops"), Outcome.Failure("other"))
      assert(!result.isIdentical, s"expected not identical, got $result")
    }
  }
}
