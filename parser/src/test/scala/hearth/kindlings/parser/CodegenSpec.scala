package hearth.kindlings.parser

import hearth.MacroSuite

final class CodegenSpec extends MacroSuite {

  import CodegenSpec.*

  group("generated actions") {

    test("generated and interpreted parsers agree") {
      val inputs = List("1 + 2 * 3", "(4 - 1) * (2 + 2)", "10 / 3 - 1")
      inputs.map(generatedCalc.parse) ==> inputs.map(interpretedCalc.parse)
      inputs.map(generatedCalc.parse) ==> List(7, 12, 2)
    }

    test("actions may use imports from the grammar block, pattern-matching lambdas and captured values") {
      pairs(10).parse("a:1, b:2, c:3") ==> List("a" -> 11, "b" -> 12, "c" -> 13)
    }

    test("unused values are not converted") {
      val counter = new Counter
      counting(counter).parse("1 2 3") ==> 3
      counter.count ==> 0
      countingUsed(counter).parse("1 2 3") ==> 6
      counter.count ==> 3
    }

    test("actions referring to grammar symbols are rejected") {
      compileErrors(
        """
        import hearth.kindlings.parser.*
        Grammar.grammar[String, Id] { g =>
          import g.*
          val s = nonTerminal[String]
          s ::= all("x").pure(_ => s.toString)
          s
        }
        """
      ).check("grammar symbols (and the grammar DSL) can only be used in productions")
    }
  }
}
object CodegenSpec {

  val generatedCalc: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val expr = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    left("+", "-")
    left("*", "/")
    expr ::= (
      all(expr, "+", expr).pure((a, _, b) => a + b) ||
        all(expr, "-", expr).pure((a, _, b) => a - b) ||
        all(expr, "*", expr).pure((a, _, b) => a * b) ||
        all(expr, "/", expr).pure((a, _, b) => a / b) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all(num).pure(n => n)
    )
    expr
  }

  val interpretedCalc: Parser[Id, Int] = Grammar.interpreted[Int, Id] { g =>
    import g.*
    val expr = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    left("+", "-")
    left("*", "/")
    expr ::= (
      all(expr, "+", expr).pure((a, _, b) => a + b) ||
        all(expr, "-", expr).pure((a, _, b) => a - b) ||
        all(expr, "*", expr).pure((a, _, b) => a * b) ||
        all(expr, "/", expr).pure((a, _, b) => a / b) ||
        all("(", expr, ")").pure((_, e, _) => e) ||
        all(num).pure(n => n)
    )
    expr
  }

  object Pairs {
    final case class Entry(key: String, value: Int)
    def shift(entries: List[Entry], by: Int): List[(String, Int)] = entries.map { case Entry(k, v) => k -> (v + by) }
  }

  def pairs(offset: Int): Parser[Id, List[(String, Int)]] = Grammar.grammar[List[(String, Int)], Id] { g =>
    import g.*
    import Pairs.*
    val all0 = nonTerminal[List[(String, Int)]]
    val entry = nonTerminal[Entry]
    val key = terminal("[a-z]+")
    val num = terminal("[0-9]+").map(s => s.toInt)
    skip(" +")
    entry ::= all(key, ":", num).pure { (k, _, n) =>
      val e = Entry(k, n)
      e match {
        case Entry(name, value) if name.nonEmpty => Entry(name, value)
        case other                               => other
      }
    }
    all0 ::= all(sepBy1(entry, ",")).pure(entries => shift(entries, offset))
    all0
  }

  final class Counter {
    var count = 0
    def apply(s: String): Int = { count += 1; s.toInt }
  }

  def counting(counter: Counter): Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val items = nonTerminal[Int]
    val num = terminal("[0-9]+").map(s => counter(s))
    skip(" +")
    items ::= (
      all(num).pure(_ => 1) ||
        all(items, num).pure((n, _) => n + 1)
    )
    items
  }

  def countingUsed(counter: Counter): Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val items = nonTerminal[Int]
    val num = terminal("[0-9]+").map(s => counter(s))
    skip(" +")
    items ::= (
      all(num).pure(n => n) ||
        all(items, num).pure((a, b) => a + b)
    )
    items
  }
}
