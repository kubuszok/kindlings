package hearth.kindlings.parser

import hearth.MacroSuite

final class CalculatorSpec extends MacroSuite {

  import CalculatorSpec.*

  group("arithmetic with yacc-style precedence") {

    test("respects precedence and associativity") {
      calc.parse("1 + 2 * 3") ==> 7
      calc.parse("10 - 4 - 3") ==> 3
      calc.parse("2 ^ 3 ^ 2") ==> 512
      calc.parse("(1 + 2) * 3") ==> 9
      calc.parse("-2 ^ 2") ==> 4 // unary minus (NEG) is declared last, so it binds tighter than ^
      calc.parse("-(2 + 3) * 4") ==> -20
    }

    test("reports syntax errors with position and expected tokens") {
      val error = intercept[ParseError](calc.parse("1 + * 2"))
      error.line ==> 1
      error.column ==> 5
      assert(error.expected.contains("num"), error.getMessage)
      error.found ==> "\"*\""
      assert(error.getMessage.contains("found \"*\", expected"), error.getMessage)
    }

    test("reports unexpected end of input") {
      val error = intercept[ParseError](calc.parse("(1 + 2"))
      error.found ==> "end of input"
      assert(error.expected.contains("\")\""), error.getMessage)
    }

    test("reports unexpected characters") {
      val error = intercept[ParseError](calc.parse("1 + $"))
      error.column ==> 5
      error.detail ==> "Unexpected character"
    }

    test("deeply nested input does not use the JVM stack") {
      val depth = 100000
      calc.parse("(" * depth + "1" + ")" * depth) ==> 1
    }
  }
}
object CalculatorSpec {

  val calc: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val expr = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip("[ \\t\\n]+")
    left("+", "-")
    left("*", "/")
    right("^")
    nonassoc("NEG")
    expr ::= (
      all(expr, "+", expr).pure((a, _, b) => a + b) ||
        all(expr, "-", expr).pure((a, _, b) => a - b) ||
        all(expr, "*", expr).pure((a, _, b) => a * b) ||
        all(expr, "/", expr).pure((a, _, b) => a / b) ||
        all(expr, "^", expr).pure((a, _, b) => math.pow(a.toDouble, b.toDouble).toInt) ||
        all("-", expr).prec("NEG").pure((_, e) => -e) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all(num).pure(n => n)
    )
    expr
  }
}
