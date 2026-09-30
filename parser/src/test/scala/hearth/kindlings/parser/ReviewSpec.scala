package hearth.kindlings.parser

import hearth.MacroSuite
import hearth.kindlings.parser.internal.runtime.{Input, Machine}
import java.io.StringReader

final class ReviewSpec extends MacroSuite {
  group("parser review regressions") {
    test("actions and conversions retain the enclosing instance") {
      val owner = new ReviewSpec.Owner(20)
      owner.parser.parse("1") ==> 41
    }

    test("unfinished literal tokens are valid input prefixes") {
      intercept[ParseError](ReviewSpec.keyword.parse("tru")).endOfInput ==> true
    }

    test("unfinished streamed literal tokens are valid input prefixes") {
      intercept[ParseError](ReviewSpec.keyword.parse(new StringReader("tru"))).endOfInput ==> true
    }

    test("nonpositive step budgets are rejected instead of yielding forever") {
      val _ = intercept[IllegalArgumentException](ReviewSpec.keyword.pushMachine().run(0))
      intercept[IllegalArgumentException](ReviewSpec.keyword.pushMachine().run(-1))
    }

    test("a token split across input refills is scanned in linear work") {
      val input = new ReviewSpec.CountingInput("a" * 1000)
      ParserEngine.id.run[String](() => new Machine(ReviewSpec.word.compiled, input)) ==> ("a" * 1000)
      assert(input.reads <= 3000, s"1000 input characters required ${input.reads} character reads")
    }
  }
}
object ReviewSpec {
  val word: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    start ::= all(terminal("a+")).pure(identity)
    start
  }

  final class CountingInput(text: String) extends Input {
    private var available = 0
    var reads = 0
    def ensure(pos: Long): Int =
      if (pos < available) Input.Available
      else if (available == text.length) Input.End
      else Input.NeedMore
    def charAt(pos: Long): Char = { reads += 1; text.charAt(pos.toInt) }
    def slice(start: Long, end: Long): String = text.substring(start.toInt, end.toInt)
    def release(pos: Long): Unit = ()
    def refill(): Unit = available += 1
    def lineColumn(pos: Long): (Int, Int) = (1, pos.toInt + 1)
  }

  val keyword: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    start ::= all("true").pure(identity)
    start
  }

  final class Owner(val offset: Int) {
    val parser: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
      import g.*
      val start = nonTerminal[Int]
      val number = terminal("[0-9]+").map(s => s.toInt + this.offset)
      start ::= all(number).pure(n => n + this.offset)
      start
    }
  }
}
