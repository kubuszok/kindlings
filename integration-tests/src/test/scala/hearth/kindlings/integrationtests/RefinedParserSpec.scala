package hearth.kindlings.integrationtests

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import hearth.MacroSuite
import hearth.kindlings.parser.*

/** Repetitions collected into refined collections (`IsValueType` provided by kindlings-refined-integration). */
final class RefinedParserSpec extends MacroSuite {

  import RefinedParserSpec.*

  group("Refined + Parser") {

    test("repetitions are collected into a refined collection") {
      numbers.parse("1 2 3") ==> Right(refineV[NonEmpty](List(1, 2, 3)).toOption.get)
      words.parse("[a, b]").map(_.value) ==> Right(Vector("a", "b"))
    }

    test("a refinement rejecting the values is reported as a parse error") {
      val error = numbers.parse("").swap.getOrElse(fail("expected a Left"))
      assert(error.getMessage.startsWith("Invalid"), error.getMessage)
      assert(words.parse("[]").isLeft)
    }
  }
}
object RefinedParserSpec {

  type Result[A] = Either[ParseError, A]

  val numbers: Parser[Result, List[Int] Refined NonEmpty] = Grammar.grammar[List[Int] Refined NonEmpty, Result] { g =>
    import g.*
    val s = nonTerminal[List[Int] Refined NonEmpty]
    skip(" +")
    s ::= all(rep(terminal("[0-9]+").map(_.toInt)).as[List[Int] Refined NonEmpty]).pure(ns => ns)
    s
  }

  // LL(1): parsed by the recursive-descent fast path, rejections are left to the machine
  val words: Parser[Result, Vector[String] Refined NonEmpty] =
    Grammar.grammar[Vector[String] Refined NonEmpty, Result] { g =>
      import g.*
      val s = nonTerminal[Vector[String] Refined NonEmpty]
      skip(" +")
      s ::= all("[", sepBy(terminal("[a-z]+"), ",").as[Vector[String] Refined NonEmpty], "]").pure((_, w, _) => w)
      s
    }
}
