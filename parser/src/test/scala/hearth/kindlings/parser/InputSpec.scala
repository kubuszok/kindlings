package hearth.kindlings.parser

import hearth.MacroSuite
import hearth.kindlings.parser.internal.runtime.{Input, ReaderInput}

import java.io.{ByteArrayInputStream, Reader, StringReader}

final class InputSpec extends MacroSuite {

  import InputSpec.*

  group("Reader and InputStream inputs") {

    test("tokens crossing buffer refills are lexed correctly") {
      val text = (1 to 2000).map(i => s"item$i = ${i * 7}").mkString("\n")
      val expected = (1 to 2000).map(i => i * 7).sum
      assignments.parse(new StringReader(text), bufferSize = 7) ==> expected
      assignments.parse(text) ==> expected
    }

    test("UTF-8 input streams") {
      val text = "zażółć = 1\ngęślą = 2"
      assignments.parse(new ByteArrayInputStream(text.getBytes("UTF-8"))) ==> 3
    }

    test("errors in streamed input report lines after discarded text") {
      val text = (1 to 500).map(i => s"a$i = $i").mkString("\n") + "\nbroken = = 1"
      val error = intercept[ParseError](assignments.parse(new StringReader(text), bufferSize = 16))
      error.line ==> 501
      error.column ==> 10
      error.found ==> "\"=\""
    }

    test("large streamed input is parsed with a bounded buffer") {
      val count = 200000
      val reader = new GeneratedReader(count)
      assignments.parse(reader, bufferSize = 64) ==> (1 to count).map(_ % 10).sum
    }

    test("the buffer of a ReaderInput stays bounded when positions are released") {
      val input = new ReaderInput(new GeneratedReader(100000), 32)
      var pos = 0L
      var chars = 0L
      var done = false
      while (!done)
        input.ensure(pos) match {
          case Input.Available =>
            val _ = input.charAt(pos)
            pos += 1
            chars += 1
            input.release(pos)
          case Input.NeedMore => input.refill()
          case _              => done = true
        }
      assert(chars > 1000000L, chars)
      input.capacity ==> 32
    }
  }

  group("REPL support") {

    test("an input ending too early is reported as endOfInput") {
      intercept[ParseError](assignments.parse("x = ")).endOfInput ==> true
      intercept[ParseError](assignments.parse("x = = 1")).endOfInput ==> false
    }
  }
}
object InputSpec {

  /** `name = number` lines; the result is the sum of the numbers. */
  val assignments: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val program = nonTerminal[Int]
    val assignment = nonTerminal[Int]
    val name = terminal("[a-zżółćęśąźń][a-z0-9żółćęśąźń]*")
    val number = terminal("[0-9]+").map(_.toInt)
    skip("[ \\t\\r\\n]+")
    assignment ::= all(name, "=", number).pure((_, _, n) => n)
    program ::= all(rep1(assignment)).pure(_.sum)
    program
  }

  /** Produces `count` lines `a<i> = <i % 10>` without materializing them. */
  final class GeneratedReader(count: Int) extends Reader {
    private var line = 0
    private var current = ""
    private var index = 0

    def read(buffer: Array[Char], offset: Int, length: Int): Int = {
      if (index == current.length) {
        if (line == count) return -1
        line += 1
        current = s"a$line = ${line % 10}\n"
        index = 0
      }
      val n = math.min(length, current.length - index)
      current.getChars(index, index + n, buffer, offset)
      index += n
      n
    }

    def close(): Unit = ()
  }
}
