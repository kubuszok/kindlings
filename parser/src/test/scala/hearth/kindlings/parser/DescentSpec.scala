package hearth.kindlings.parser

import hearth.MacroSuite
import hearth.kindlings.parser.internal.runtime.{GeneratedGrammar, Machine, StringInput}

import scala.util.Try

/** `String` inputs of LL(1) grammars are parsed by the generated recursive-descent parser first; on any error (or deep
  * nesting) the machine parses the input again. Both must give the same values and the same errors.
  */
final class DescentSpec extends MacroSuite {

  import DescentSpec.*

  private def hasDescent[F[_]](p: Parser[F, ?]): Boolean =
    p.compiled.asInstanceOf[GeneratedGrammar].reductions.hasDescent

  /** Whether parsing `input` gave the recursive-descent parser's result (rather than the machine's). */
  private def descends[F[_]](p: Parser[F, ?], input: String): Boolean = {
    val m = new Machine(p.compiled, new StringInput(input))
    val _ = m.run()
    m.descended
  }

  group("the recursive-descent parser") {

    test("is generated for LL(1) grammars without effects or precedence, unless RequireLALR") {
      hasDescent(lang) ==> true
      hasDescent(LL1Spec.jsonDefault) ==> true
      hasDescent(LLBackendSpec.statements) ==> true // RequireLL1: the LL(1) machine is the fallback
      hasDescent(LLBackendSpec.statementsLALR) ==> false
      hasDescent(LLBackendSpec.effectful) ==> false
      hasDescent(CalculatorSpec.calc) ==> false // precedence declarations
    }

    test("parses valid inputs, keywords, identifiers, comments and literals with mapSlice") {
      val input = "let x = 1 + (22 - -3); // comment\nprint x, abc + $ab ,2;{ $yes! IF 1 THEN print 3; END }$yes!"
      lang.parse(input) ==> Right("x=26;print(1,6,2);{v4;if(1,print(3))};v4")
      assert(descends(lang, input))
      // "let" is the keyword, "letter" the longer identifier
      lang.parse("let letter = let;") ==> lang.parseByMachine("let letter = let;")
      assert(lang.parse("let letter = let;").isLeft)
      lang.parse("let letter = letter + $x;") ==> Right("letter=8")
    }

    test("gives the machine's errors") {
      val inputs = List(
        "",
        "let",
        "let x = ;",
        "print 1 2;",
        "print ,;",
        "{ print 1;",
        "IF 1 THEN print 1;",
        "IF 1 THEN print 1; ENDX",
        "IFF",
        "let x = 1; }",
        "$yes",
        "$yes!!",
        "$",
        "$",
        "// only a comment",
        "let x = 1 // no semicolon"
      )
      inputs.foreach { input =>
        assertEquals(lang.parse(input), lang.parseByMachine(input), input)
        assert(input.isEmpty || input.startsWith("//") || !descends(lang, input), input)
      }
    }

    test("gives the same values and errors as the machine on generated inputs") {
      val pieces = Vector(
        "let", "letter", "x", "$y", "$", "=", "1", "42", "+", "-", "(", ")", ";", ",", "print", "!", "{", "}", "IF",
        "THEN", "END", "//c\n", " ", "\n", "#"
      )
      val random = new scala.util.Random(7)
      var descended = 0
      (1 to 3000).foreach { _ =>
        val input =
          Vector.fill(random.nextInt(16))(pieces(random.nextInt(pieces.size))).mkString(if (random.nextBoolean()) " " else "")
        assertEquals(lang.parse(input), lang.parseByMachine(input), input)
        if (descends(lang, input)) descended += 1
      }
      assert(descended > 50, s"only $descended inputs were parsed by recursive descent")
    }

    test("gives the same values and errors as the machine on JSON") {
      val pieces = Vector("{", "}", "[", "]", ",", ":", "\"a\"", "\"b\\\"c\"", "1", "-2.5e3", "true", "false", "null", " ", "x")
      val random = new scala.util.Random(3)
      (1 to 3000).foreach { _ =>
        val input = Vector.fill(random.nextInt(12))(pieces(random.nextInt(pieces.size))).mkString
        assertEquals(
          Try(LL1Spec.jsonDefault.parse(input)).toEither.left.map(_.getMessage),
          Try(LL1Spec.jsonDefault.parseByMachine(input)).toEither.left.map(_.getMessage),
          input
        )
      }
      val doc = """{"a": [1, -2, {"b": null}], "c": "de", "f": [true, false, []], "g": {}}"""
      assert(descends(LL1Spec.jsonDefault, doc))
      LL1Spec.jsonDefault.parse(doc) ==> LL1Spec.jsonDefault.parseByMachine(doc)
    }

    test("leaves deep nesting to the machine") {
      nesting.parse("(" * 10 + "x" + ")" * 10) ==> 10
      assert(descends(nesting, "(" * 10 + "x" + ")" * 10))
      val depth = 100000
      nesting.parse("(" * depth + "x" + ")" * depth) ==> depth
      assert(!descends(nesting, "(" * depth + "x" + ")" * depth))
    }

    test("leaves rejected values to the machine") {
      hasDescent(CollectionsSpec.nonEmpty) ==> true
      def message(input: String) = CollectionsSpec.nonEmpty.parse(input).left.map(_.getMessage)
      assert(message("").isLeft)
      message("") ==> CollectionsSpec.nonEmpty.parseByMachine("").left.map(_.getMessage)
      assert(!descends(CollectionsSpec.nonEmpty, ""))
      message("1 2") ==> CollectionsSpec.nonEmpty.parseByMachine("1 2").left.map(_.getMessage)
      assert(descends(CollectionsSpec.nonEmpty, "1 2"))
    }
  }
}
object DescentSpec {

  type Result[A] = Either[String, A]

  /** Keywords sharing first chars with identifiers (read by the lexer), keywords and punctuation with unique first chars
    * (compared with the input), comments (skipped, but not simple whitespace), a literal with `mapSlice`, primitive
    * non-terminals and repetitions.
    */
  val lang: Parser[Result, String] = Grammar.grammar[String, Result] { g =>
    import g.*
    val program = nonTerminal[String]
    val statement = nonTerminal[String]
    val expr = nonTerminal[Int]
    val more = nonTerminal[Int]
    val atom = nonTerminal[Int]
    val ident = terminal("[a-z][a-z0-9]*")
    val number = terminal("[0-9]+").map(_.toInt)
    val variable = terminal("\\$[a-z]+").mapSlice((_: String, start: Int, end: Int) => end - start)
    skip("[ \\n]+")
    skip("//[^\\n]*")
    program ::= all(rep(statement).as[Vector[String]]).pure(ss => ss.mkString(";"))
    statement ::= (
      all("let", ident, "=", expr, ";").pure((_, x, _, e, _) => s"$x=$e") ||
        all("print", sepBy(expr, ","), ";").pure((_, es, _) => es.mkString("print(", ",", ")")) ||
        all("{", rep(statement), "}").pure((_, ss, _) => ss.mkString("{", ";", "}")) ||
        all(variable, "!").pure((n, _) => s"v$n") ||
        all("IF", expr, "THEN", statement, "END").pure((_, c, _, s, _) => s"if($c,$s)")
    )
    expr ::= all(atom, rep(more)).pure((a, ms) => a + ms.sum)
    more ::= all("+", atom).pure((_, b) => b) || all("-", atom).pure((_, b) => -b)
    atom ::= (
      all(number).pure(n => n) ||
        all(ident).pure(x => x.length) ||
        all(variable).pure(n => n) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all("-", atom).pure((_, a) => -a)
    )
    program
  }

  /** LL(1), LALR by default: the fallback for deep nesting is the LALR machine. */
  val nesting: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val n = nonTerminal[Int]
    n ::= all("(", n, ")").pure((_, d, _) => d + 1) || all("x").pure(_ => 0)
    n
  }
}
