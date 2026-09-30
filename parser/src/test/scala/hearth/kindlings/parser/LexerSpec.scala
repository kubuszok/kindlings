package hearth.kindlings.parser

import hearth.MacroSuite
import hearth.kindlings.parser.internal.runtime.GeneratedGrammar

/** The generated `String` lexer must behave exactly like the table-driven lexer (still used for `Reader` inputs):
  * longest match with backtracking, literals over regexes, non-ASCII ranges, skipped tokens, error positions.
  */
final class LexerSpec extends MacroSuite {

  import LexerSpec.*

  private def viaString(input: String) = tokens.parse(input)
  private def viaReader(input: String) = tokens.parse(new java.io.StringReader(input), 16)

  group("the generated String lexer") {

    test("is generated for small lexers") {
      tokens.compiled.asInstanceOf[GeneratedGrammar].reductions.hasStringLexer ==> true
    }

    test("tokenizes like the table lexer") {
      viaString("if iff x1 == 2.5e3 => \"a\\\"b\" // comment\n αβ_1 = 10") ==> Right(
        List(
          "kw:if",
          "id:iff",
          "id:x1",
          "op:==",
          "num:2.5e3",
          "op:=>",
          "str:\"a\\\"b\"",
          "id:αβ_1",
          "op:=",
          "num:10"
        )
      )
    }

    test("backtracks to the longest accepted prefix") {
      viaString("1e") ==> Right(List("num:1", "id:e"))
      viaString("1.") ==> viaReader("1.")
      viaString("===") ==> Right(List("op:==", "op:="))
    }

    test("agrees with the table lexer on edge cases, including errors") {
      val inputs = List(
        "",
        "   ",
        "// only a comment",
        "#",
        "a # b",
        "\"unterminated",
        "\"escape at end\\",
        "€",
        "x€",
        "αβγ δ",
        "1.5e+",
        "12.34.56",
        "if(x)",
        "= == => ==>",
        "\"\"\"\"",
        "a\n\n  b // c\n\"d\""
      )
      inputs.foreach(input => assertEquals(viaString(input), viaReader(input), input))
    }

    test("skips whitespace runs inline only when they are always skipped text") {
      // " x*" is skipped: an "x" after a space continues the skipped text, so the space alone may not be skipped inline
      trailing.parse("a xb") ==> Right(List("a", "b"))
      trailing.parse("a xb") ==> trailing.parse(new java.io.StringReader("a xb"), 16)
      trailingLL.parse("[a xb]") ==> Right(List("a", "b"))
      viaString("a   b\n\n  c") ==> Right(List("id:a", "id:b", "id:c"))
      // `[ \n]+` is skipped inline (its DFA has two states: after the first char and after more)
      val json = LL1Spec.jsonDefault.compiled.tables
      (json.simpleSkip(' '), json.simpleSkip('\n'), json.simpleSkip('x')) ==> ((true, true, false))
      trailing.compiled.tables.simpleSkip(' ') ==> false
    }

    test("stays table-driven for large lexers") {
      large.compiled.asInstanceOf[GeneratedGrammar].reductions.hasStringLexer ==> false
      large.parse("kemubcrdls gzhmxzhgqp") ==> Right(2)
    }

    test("agrees with the table lexer on generated inputs") {
      val alphabet = "aif1 0.e+-=>\"\\/\n#α€_".toVector
      val random = new scala.util.Random(42)
      (1 to 500).foreach { _ =>
        val input = Vector.fill(random.nextInt(12))(alphabet(random.nextInt(alphabet.size))).mkString
        assertEquals(viaString(input), viaReader(input), input)
      }
    }
  }
}
object LexerSpec {

  val trailing: Parser[Result, List[String]] = Grammar.grammar[List[String], Result] { g =>
    import g.*
    val words = nonTerminal[List[String]]
    skip(" x*")
    words ::= all(rep(terminal("[a-z]+"))).pure(ws => ws)
    words
  }

  val trailingLL: Parser[Result, List[String]] = Grammar.grammar[List[String], Result] { g =>
    import g.*
    enable(RequireLL1)
    val words = nonTerminal[List[String]]
    skip(" x*")
    words ::= all("[", rep(terminal("[a-z]+")), "]").pure((_, ws, _) => ws)
    words
  }

  /** 30 words of 10 letters: more DFA states than `CodegenPlan.MaxLexerStates`. */
  val large: Parser[Result, Int] = Grammar.grammar[Int, Result] { g =>
    import g.*
    val words = nonTerminal[Int]
    skip(" +")
    words ::= all(
      rep1(
        terminal(
          "kemubcrdls|bqgbcnnchc|rnbsdhuusb|ssmbhbrejn|erdsjrvfds|sugldrwcsb|tgpvrnykos|oljhzfwyhc|sjqpkxojtc|dqnfykepnb|vcyrszkkwl|tpszoccipw|vcbxwjusvo|jwmvlaolft|dpbgyjexhm|mpcfomrien|riwnlvmhec|fehvhapsfi|jaenrltske|wqtuvxboyv|zrmmmmdpum|bgcgofdktb|daserdltac|gtmeuiltlp|ddpoppjced|xkxipwfqag|qlewrayqju|cwiqlflyhr|ryqkuhtzzy|gzhmxzhgqp"
        )
      )
    ).pure(ws => ws.size)
    words
  }

  type Result[A] = Either[String, A]

  val tokens: Parser[Result, List[String]] = Grammar.grammar[List[String], Result] { g =>
    import g.*
    val all_ = nonTerminal[List[String]]
    val token = nonTerminal[String]
    val ident = terminal("[a-zA-Z_α-ω][a-zA-Z0-9_α-ω]*")
    val number = terminal("[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?")
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"")
    skip("[ \\t\\r\\n]+")
    skip("//[^\\n]*")
    all_ ::= all(rep(token)).pure(ts => ts)
    token ::= (
      all("if").pure(_ => "kw:if") ||
        all(ident).pure(s => "id:" + s) ||
        all(number).pure(s => "num:" + s) ||
        all(string).pure(s => "str:" + s) ||
        all("=").pure(_ => "op:=") ||
        all("==").pure(_ => "op:==") ||
        all("=>").pure(_ => "op:=>") ||
        all("(").pure(_ => "op:(") ||
        all(")").pure(_ => "op:)")
    )
    all_
  }
}
