package hearth.kindlings.parser

import hearth.MacroSuite
import hearth.kindlings.parser.internal.runtime.GeneratedGrammar

import scala.util.{Failure, Success, Try}

/** Grammars with `enable(RequireLL1)` are parsed top down (the generated `runLL`) for `String` inputs; each grammar
  * here has an LALR(1) twin that must give exactly the same values and errors at the same positions.
  */
final class LLBackendSpec extends MacroSuite {

  import LLBackendSpec.*

  private def usesLL[F[_]](p: Parser[F, ?]): Boolean =
    p.compiled.asInstanceOf[GeneratedGrammar].reductions.hasLL

  group("the LL(1) backend") {

    test("is used with RequireLL1 only") {
      usesLL(statements) ==> true
      usesLL(statementsLALR) ==> false
      usesLL(LL1Spec.jsonDefault) ==> false // LL(1), but the LALR(1) parser is the default
      usesLL(CalculatorSpec.calc) ==> false
    }

    test("gives the same values and errors as the LALR parser") {
      val inputs = List(
        "",
        "let x = 1;",
        "let x = 1 + 2 - -3; print x, (x + 1), 2.5;",
        "{ let a = 1; { print a; } } print;",
        "print! 1, 2;",
        "let = 1;",
        "let x 1;",
        "print 1,;",
        "{ print 1;",
        "print (1 + ;",
        "let x = 1; }",
        "print 1 2;",
        "$"
      )
      inputs.foreach(input => assertEquals(statements.parse(input), statementsLALR.parse(input), input))
    }

    test("gives the same values and errors as the LALR parser on generated inputs") {
      val pieces =
        Vector("let", "x", "y1", "=", "1", "2.5", "+", "-", "(", ")", ";", ",", "print", "!", "{", "}", " ", "#")
      val random = new scala.util.Random(11)
      (1 to 2000).foreach { _ =>
        val input = Vector.fill(random.nextInt(14))(pieces(random.nextInt(pieces.size))).mkString(" ")
        // values are identical; errors are at the same position with the same unexpected token (the expected tokens
        // may differ: LALR merges look-aheads of different contexts, LL(1) uses those of the current rule)
        def comparable(result: Result[String]): Result[String] = result.left.map(_.takeWhile(_ != ','))
        assertEquals(comparable(statements.parse(input)), comparable(statementsLALR.parse(input)), input)
      }
    }

    test("reports only the tokens valid in the current context") {
      // at the top level only statements or the end of input can follow; the LALR parser also lists "}" because it
      // merges the look-aheads of statements at the top level and inside blocks
      statements.parse("print ; ,") ==> Left(
        "Unexpected token at 1:9: found \",\", expected one of: \"let\", \"print\", \"{\", end of input"
      )
      statementsLALR.parse("print ; ,") ==> Left(
        "Unexpected token at 1:9: found \",\", expected one of: \"let\", \"print\", \"{\", \"}\", end of input"
      )
    }

    test("parses deep nesting without using the JVM stack") {
      val depth = 100000
      usesLL(nesting) ==> true
      nesting.parse("(" * depth + "x" + ")" * depth) ==> depth
    }

    test("runs effectful actions") {
      effectful.parse("1 2 3") ==> Success(6)
      assert(effectful.parse("1 x").isFailure)
      effectful.parse("1 13 2") match {
        case Failure(e) => e.getMessage ==> "unlucky 13"
        case other      => fail(s"expected a failure, got $other")
      }
    }
  }
}
object LLBackendSpec {

  type Result[A] = Either[String, A]

  /** Statements with nested blocks, lists, options and expressions written with repetitions (LL(1)). */
  val statements: Parser[Result, String] = Grammar.grammar[String, Result] { g =>
    import g.*
    enable(RequireLL1)
    val program = nonTerminal[String]
    val statement = nonTerminal[String]
    val expr = nonTerminal[Double]
    val more = nonTerminal[Double => Double]
    val atom = nonTerminal[Double]
    val ident = terminal("[a-z][a-z0-9]*")
    val number = terminal("[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double)
    skip("[ \\n]+")
    program ::= all(rep(statement)).pure(ss => ss.mkString(" "))
    statement ::= (
      all("let", ident, "=", expr, ";").pure((_, x, _, e, _) => s"let($x=$e)") ||
        all("print", opt("!"), sepBy(expr, ","), ";").pure((_, bang, es, _) =>
          s"print${bang.fold("")(_ => "!")}(${es.mkString(",")})"
        ) ||
        all("{", rep(statement), "}").pure((_, ss, _) => s"block(${ss.mkString(" ")})")
    )
    expr ::= all(atom, rep(more)).pure((a, ops) => ops.foldLeft(a)((acc, op) => op(acc)))
    more ::= all("+", atom).pure((_, b) => (a: Double) => a + b) || all("-", atom).pure((_, b) => (a: Double) => a - b)
    atom ::= (
      all(number).pure(n => n) ||
        all(ident).pure(x => x.length.toDouble) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all("-", atom).pure((_, a) => -a)
    )
    program
  }

  val statementsLALR: Parser[Result, String] = Grammar.grammar[String, Result] { g =>
    import g.*
    enable(RequireLALR)
    val program = nonTerminal[String]
    val statement = nonTerminal[String]
    val expr = nonTerminal[Double]
    val more = nonTerminal[Double => Double]
    val atom = nonTerminal[Double]
    val ident = terminal("[a-z][a-z0-9]*")
    val number = terminal("[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double)
    skip("[ \\n]+")
    program ::= all(rep(statement)).pure(ss => ss.mkString(" "))
    statement ::= (
      all("let", ident, "=", expr, ";").pure((_, x, _, e, _) => s"let($x=$e)") ||
        all("print", opt("!"), sepBy(expr, ","), ";").pure((_, bang, es, _) =>
          s"print${bang.fold("")(_ => "!")}(${es.mkString(",")})"
        ) ||
        all("{", rep(statement), "}").pure((_, ss, _) => s"block(${ss.mkString(" ")})")
    )
    expr ::= all(atom, rep(more)).pure((a, ops) => ops.foldLeft(a)((acc, op) => op(acc)))
    more ::= all("+", atom).pure((_, b) => (a: Double) => a + b) || all("-", atom).pure((_, b) => (a: Double) => a - b)
    atom ::= (
      all(number).pure(n => n) ||
        all(ident).pure(x => x.length.toDouble) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all("-", atom).pure((_, a) => -a)
    )
    program
  }

  val nesting: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    enable(RequireLL1)
    val n = nonTerminal[Int]
    n ::= all("(", n, ")").pure((_, d, _) => d + 1) || all("x").pure(_ => 0)
    n
  }

  val effectful: Parser[Try, Int] = Grammar.grammar[Int, Try] { g =>
    import g.*
    enable(RequireLL1)
    val total = nonTerminal[Int]
    val n = nonTerminal[Int]
    skip(" +")
    n ::= all(terminal("[0-9]+"))(t => if (t == "13") Failure(new Exception("unlucky 13")) else Try(t.toInt))
    total ::= all(rep1(n)).pure(ns => ns.sum)
    total
  }
}
