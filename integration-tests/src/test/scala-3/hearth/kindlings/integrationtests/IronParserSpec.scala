package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.{given, *}
import hearth.MacroSuite
import hearth.kindlings.parser.*

/** Repetitions collected into Iron-constrained collections (`IsValueType` provided by kindlings-iron-integration). */
final class IronParserSpec extends MacroSuite {

  import IronParserSpec.*

  group("Iron + Parser") {

    test("repetitions are collected into a constrained collection") {
      numbers.parse("1 2 3").map(identity[List[Int]]) ==> Right(List(1, 2, 3))
    }

    test("a constraint rejecting the values is reported as a parse error") {
      val error = numbers.parse("1").swap.getOrElse(fail("expected a Left"))
      assert(error.getMessage.startsWith("Invalid"), error.getMessage)
    }
  }
}
object IronParserSpec {

  type Result[A] = Either[ParseError, A]
  type AtLeastTwo = List[Int] :| MinLength[2]

  val numbers: Parser[Result, AtLeastTwo] = Grammar.grammar[AtLeastTwo, Result] { g =>
    import g.*
    val s = nonTerminal[AtLeastTwo]
    skip(" +")
    s ::= all(rep(terminal("[0-9]+").map(_.toInt)).as[AtLeastTwo]).pure(ns => ns)
    s
  }
}
