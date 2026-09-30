package hearth.kindlings.parser

import hearth.MacroSuite

final class SliceSpec extends MacroSuite {

  import SliceSpec.*

  group("mapSlice") {

    test("converts the token from the input and its bounds, then .map applies") {
      strings.parse("\"ab\" \"c\\\"d\" \"\"") ==> Right(List("AB!", "C\"D!", "!"))
    }

    test("gives the same values for String, Reader and interpreted parsing") {
      val input = "\"x\" \"y\\\\z\" \"long string with spaces\""
      strings.parse(new java.io.StringReader(input), 16) ==> strings.parse(input)
      interpreted.parse(input) ==> strings.parse(input)
    }

    test("runs when its token is read") {
      val counter = new CodegenSpec.Counter
      counting(counter).parse("\"a\" \"b\"") ==> Right(2)
      counter.count ==> 2
    }

    test("must be the first conversion") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          val s = nonTerminal[Int]
          val t = terminal("[a-z]+").map(_.length).mapSlice((in, a, b) => b - a)
          s ::= all(t).pure(n => n)
          s
        }
        """
      ).check("`.mapSlice` must be the first conversion")
    }

    test("must not share its pattern with another terminal") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[Int, Id] { g =>
          import g.*
          val s = nonTerminal[Int]
          val sliced = terminal("[a-z]+").mapSlice((in, a, b) => b - a)
          val plain = terminal("[a-z]+")
          s ::= all(sliced).pure(n => n) || all(plain, plain).pure((a, b) => a.length + b.length)
          s
        }
        """
      ).check("is used by a terminal with `mapSlice` and by another terminal")
    }
  }
}
object SliceSpec {

  type Result[A] = Either[String, A]

  /** Drops the quotes and resolves `\x` escapes with a single copy when there are none. */
  def unquote(input: String, start: Int, end: Int): String = {
    val from = start + 1
    val until = end - 1
    if (Slices.indexOf(input, '\\', from, until) < 0) input.substring(from, until)
    else {
      val sb = new StringBuilder
      var i = from
      while (i < until) {
        val c = input.charAt(i)
        if (c == '\\') { i += 1; sb.append(input.charAt(i)) }
        else sb.append(c)
        i += 1
      }
      sb.toString
    }
  }

  val strings: Parser[Result, List[String]] = Grammar.grammar[List[String], Result] { g =>
    import g.*
    val all_ = nonTerminal[List[String]]
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"").mapSlice(unquote).map(_.toUpperCase).map(_ + "!")
    skip(" +")
    all_ ::= all(rep(string)).pure(ss => ss)
    all_
  }

  val interpreted: Parser[Result, List[String]] = Grammar.interpreted[List[String], Result] { g =>
    import g.*
    val all_ = nonTerminal[List[String]]
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"").mapSlice(unquote).map(_.toUpperCase).map(_ + "!")
    skip(" +")
    all_ ::= all(rep(string)).pure(ss => ss)
    all_
  }

  def counting(counter: CodegenSpec.Counter): Parser[Result, Int] = Grammar.grammar[Int, Result] { g =>
    import g.*
    val all_ = nonTerminal[Int]
    val string = terminal("\"[a-z]*\"").mapSlice { (_, _, _) => counter.count += 1; () }
    skip(" +")
    all_ ::= all(rep(string)).pure(ss => ss.size)
    all_
  }
}
