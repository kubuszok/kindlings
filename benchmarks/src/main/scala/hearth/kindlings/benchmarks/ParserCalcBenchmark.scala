package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** Arithmetic evaluation: every reduction of the grammar produces a `Double`, so it measures how parsers carry
  * primitive values (the JSON benchmark's values are all objects).
  */
object CalcModel {

  /** ~1 MB of arithmetic: numbers with `+ - * /`, nested parentheses. */
  val input: String = {
    val random = new scala.util.Random(1)
    val sb = new StringBuilder
    def expr(depth: Int): Unit = {
      val terms = 1 + random.nextInt(4)
      var i = 0
      while (i < terms) {
        if (i > 0) sb.append(" ").append("+-*/" (random.nextInt(4))).append(" ")
        if (depth < 6 && random.nextInt(4) == 0) { sb.append('('); expr(depth + 1); sb.append(')') }
        else sb.append(1 + random.nextInt(99)).append('.').append(random.nextInt(10))
        i += 1
      }
    }
    while (sb.length < 1000000) {
      if (sb.nonEmpty) sb.append(" + ")
      expr(0)
    }
    sb.toString
  }
}

object KindlingsCalc {
  import hearth.kindlings.parser.*

  val generated: Parser[Id, Double] = Grammar.grammar[Double, Id] { g =>
    import g.*
    val expr = nonTerminal[Double]
    val num = terminal("[0-9]+(\\.[0-9]+)?").map(_.toDouble)
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

  val fastNumbers: Parser[Id, Double] = Grammar.grammar[Double, Id] { g =>
    import g.*
    val expr = nonTerminal[Double]
    val num = terminal("[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double)
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
}

object FastparseCalc {
  import fastparse.*
  import NoWhitespace.*

  def space[$: P]: P[Unit] = P(CharsWhileIn(" ", 0))
  def number[$: P]: P[Double] = P(CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!.map(_.toDouble)
  def parens[$: P]: P[Double] = P("(" ~/ space ~ addSub ~ space ~ ")")
  def factor[$: P]: P[Double] = P(space ~ (number | parens) ~ space)
  def divMul[$: P]: P[Double] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)
  def addSub[$: P]: P[Double] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)
  def expr[$: P]: P[Double] = P(addSub ~ End)

  private def eval(tree: (Double, Seq[(String, Double)])): Double =
    tree._2.foldLeft(tree._1) { case (left, (op, right)) =>
      op match {
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case _   => left / right
      }
    }

  def parse(input: String): Double = fastparse.parse(input, expr(using _)).get.value
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ParserCalcBenchmark {
  import CalcModel.*

  @Setup def check(): Unit = {
    val expected = KindlingsCalc.generated.parse(input)
    val fast = FastparseCalc.parse(input)
    if (math.abs(expected - fast) > 1e-6 * math.abs(expected))
      throw new IllegalStateException(s"fastparse computed $fast, kindlings-parser $expected")
  }

  @Benchmark def kindlingsGenerated: Double = KindlingsCalc.generated.parse(input)
  @Benchmark def kindlingsFastNumbers: Double = KindlingsCalc.fastNumbers.parse(input)
  @Benchmark def fastparse: Double = FastparseCalc.parse(input)
}
