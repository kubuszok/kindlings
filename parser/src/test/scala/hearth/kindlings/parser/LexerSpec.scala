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

    test("is generated for small lexers (with the parse loop)") {
      tokens.compiled.asInstanceOf[GeneratedGrammar].reductions.hasStringDriver ==> true
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

    test("stays table-driven for large lexers") {
      large.compiled.asInstanceOf[GeneratedGrammar].reductions.hasStringDriver ==> false
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
