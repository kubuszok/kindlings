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

    test("incomplete token detection respects the current grammar state") {
      List(ReviewSpec.contextual, ReviewSpec.contextualInterpreted, ReviewSpec.contextualLL).foreach { parser =>
        List("yes tr" -> true, "yes fa" -> false, "yes tx" -> false, "no fal" -> true).foreach {
          case (text, incomplete) =>
            val error = intercept[ParseError](parser.parse(text))
            error.endOfInput ==> incomplete
            val streamed = intercept[ParseError](parser.parse(new StringReader(text), bufferSize = 1))
            streamed.endOfInput ==> incomplete
            if (incomplete) {
              error.offset ==> text.length.toLong
              error.found ==> "end of input"
            }
            val pushed = parser.pushMachine()
            var signal = Machine.NeedInput
            text.foreach { c =>
              if (signal != Machine.Error) {
                pushed.feed(c.toString)
                signal = pushed.run()
              }
            }
            if (signal != Machine.Error) {
              pushed.endOfInput()
              signal = pushed.run()
            }
            signal ==> Machine.Error
            pushed.error.endOfInput ==> incomplete
        }
      }
    }

    test("incomplete tokens after pending reductions and inside skipped comments") {
      List("tr", "/* unfinished", "/* complete */ tr", "yes tr").foreach { text =>
        intercept[ParseError](ReviewSpec.optional.parse(text)).endOfInput ==> true
        intercept[ParseError](ReviewSpec.optional.parse(new StringReader(text))).endOfInput ==> true
      }
      intercept[ParseError](ReviewSpec.optional.parse("true /* unfinished")).endOfInput ==> true
      intercept[ParseError](ReviewSpec.optional.parse("false")).endOfInput ==> false
    }

    test("streamed lexing retains maximal-munch rollback and resets after skipped tokens") {
      List("abbb", "abbbc", "xxx abbb", "xxx abbbc").foreach { text =>
        val expected = ReviewSpec.rollback.parse(text)
        (1 to text.length).foreach { size =>
          val machine = ReviewSpec.rollback.pushMachine(initialBufferSize = 16)
          text.grouped(size).foreach { chunk =>
            machine.feed(chunk)
            machine.run() ==> Machine.NeedInput
            machine.feed("")
            machine.run() ==> Machine.NeedInput
          }
          machine.endOfInput()
          machine.run() ==> Machine.Done
          machine.result ==> expected
        }
      }
      val text = "x" * 1000 + " abbb"
      val input = new ReviewSpec.CountingInput(text)
      ParserEngine.id.run[String](() => new Machine(ReviewSpec.rollback.compiled, input)) ==> "abbb"
      assert(input.reads < 3 * text.length, s"skipped token was rescanned: ${input.reads}")
    }
  }
}
object ReviewSpec {
  val contextual: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    skip(" +")
    start ::= (all("yes", "true").pure((_, b) => b) || all("no", "false").pure((_, b) => b))
    start
  }
  val contextualInterpreted: Parser[Id, String] = Grammar.interpreted[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    skip(" +")
    start ::= (all("yes", "true").pure((_, b) => b) || all("no", "false").pure((_, b) => b))
    start
  }
  val contextualLL: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    enable(RequireLL1)
    val start = nonTerminal[String]
    skip(" +")
    start ::= (all("yes", "true").pure((_, b) => b) || all("no", "false").pure((_, b) => b))
    start
  }
  val optional: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    skip(" +")
    skip("/\\*[^*]*\\*/")
    start ::= all(opt("yes"), "true").pure((_, b) => b)
    start
  }
  val rollback: Parser[Id, String] = Grammar.grammar[String, Id] { g =>
    import g.*
    val start = nonTerminal[String]
    skip("[x ]+")
    start ::= (all(terminal("ab+c")).pure(identity) || all("a", terminal("b+")).pure((a, b) => a + b))
    start
  }
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
