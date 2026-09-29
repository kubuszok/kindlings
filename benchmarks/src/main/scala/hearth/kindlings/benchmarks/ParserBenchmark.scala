package hearth.kindlings.benchmarks

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/** JSON text -> AST with kindlings-parser (generated actions, interpreted actions, streamed from a `Reader`) vs parser
  * combinator libraries (fastparse, parboiled2, cats-parse, Parsley), all building the same [[ParserModel.J]] AST.
  * circe's jawn-based parser is a hand-written reference (it builds circe's own `Json`).
  */
object ParserModel {

  sealed trait J
  case object JNull extends J
  final case class JBool(value: Boolean) extends J
  final case class JNum(value: Double) extends J
  final case class JStr(value: String) extends J
  final case class JArr(items: List[J]) extends J
  final case class JObj(fields: List[(String, J)]) extends J

  /** ~1 MB of JSON: an array of records with nested objects, arrays, numbers, strings, booleans and nulls. */
  val input: String = {
    val sb = new StringBuilder("[\n")
    var i = 0
    while (i < 4000) {
      if (i > 0) sb.append(",\n")
      sb.append(
        s"""  {"id": $i, "name": "item number $i", "price": ${i * 1.25}, "active": ${i % 2 == 0}, """ +
          s""""tags": ["alpha", "beta", "gamma${i % 7}"], "dimensions": {"w": ${i % 13}, "h": -${i % 5}.5e1, """ +
          s""""note": null}, "description": "a slightly longer text field with spaces to make strings realistic"}"""
      )
      i += 1
    }
    sb.append("\n]").toString
  }

  def unescape(s: String): String = if (s.indexOf('\\') < 0) s else StringContext.processEscapes(s)
}

object KindlingsParsers {
  import hearth.kindlings.parser.*
  import ParserModel.*

  val generated: Parser[Id, J] = Grammar.grammar[J, Id] { g =>
    import g.*
    val value = nonTerminal[J]
    val member = nonTerminal[(String, J)]
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"").map(s => unescape(s.substring(1, s.length - 1)))
    val number = terminal("-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?").map(_.toDouble)
    skip("[ \\t\\r\\n]+")
    value ::= (
      all("{", sepBy(member, ","), "}").pure((_, members, _) => JObj(members): J) ||
        all("[", sepBy(value, ","), "]").pure((_, items, _) => JArr(items): J) ||
        all(string).pure(s => JStr(s): J) ||
        all(number).pure(n => JNum(n): J) ||
        all("true").pure(_ => JBool(true): J) ||
        all("false").pure(_ => JBool(false): J) ||
        all("null").pure(_ => JNull: J)
    )
    member ::= all(string, ":", value).pure((k, _, v) => k -> v)
    value
  }

  val interpreted: Parser[Id, J] = Grammar.interpreted[J, Id] { g =>
    import g.*
    val value = nonTerminal[J]
    val member = nonTerminal[(String, J)]
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"").map(s => unescape(s.substring(1, s.length - 1)))
    val number = terminal("-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?").map(_.toDouble)
    skip("[ \\t\\r\\n]+")
    value ::= (
      all("{", sepBy(member, ","), "}").pure((_, members, _) => JObj(members): J) ||
        all("[", sepBy(value, ","), "]").pure((_, items, _) => JArr(items): J) ||
        all(string).pure(s => JStr(s): J) ||
        all(number).pure(n => JNum(n): J) ||
        all("true").pure(_ => JBool(true): J) ||
        all("false").pure(_ => JBool(false): J) ||
        all("null").pure(_ => JNull: J)
    )
    member ::= all(string, ":", value).pure((k, _, v) => k -> v)
    value
  }
}

object FastparseJson {
  import fastparse.*, NoWhitespace.*
  import ParserModel.*

  def space[$: P]: P[Unit] = P(CharsWhileIn(" \r\n\t", 0))
  def digits[$: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def exponent[$: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[$: P]: P[Unit] = P("." ~ digits)
  def integral[$: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)
  def number[$: P]: P[J] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(x => JNum(x.toDouble))
  def `null`[$: P]: P[J] = P("null").map(_ => JNull)
  def `false`[$: P]: P[J] = P("false").map(_ => JBool(false))
  def `true`[$: P]: P[J] = P("true").map(_ => JBool(true))
  def hexDigit[$: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[$: P]: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[$: P]: P[Unit] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  def strChars[$: P]: P[Unit] = P(CharsWhile(c => c != '"' && c != '\\'))
  def rawString[$: P]: P[String] = P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(unescape)
  def string[$: P]: P[J] = P(rawString).map(JStr(_))
  def array[$: P]: P[J] = P("[" ~/ jsonExpr.rep(sep = ","./) ~ space ~ "]").map(items => JArr(items.toList))
  def pair[$: P]: P[(String, J)] = P(rawString ~/ space ~ ":" ~/ jsonExpr)
  def obj[$: P]: P[J] = P("{" ~/ pair.rep(sep = ","./) ~ space ~ "}").map(fields => JObj(fields.toList))
  def jsonExpr[$: P]: P[J] = P(space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space)
  def json[$: P]: P[J] = P(jsonExpr ~ End)

  def parse(input: String): J = fastparse.parse(input, json(_)).get.value
}

class Parboiled2Json(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
  import org.parboiled2.*
  import CharPredicate.{Digit, Digit19}
  import ParserModel.*

  def Json: Rule1[J] = rule(WhiteSpace ~ Value ~ EOI)
  def JsonObject: Rule1[J] =
    rule(
      ws('{') ~ zeroOrMore(Pair).separatedBy(ws(',')) ~ ws('}') ~> ((fields: Seq[(String, J)]) => JObj(fields.toList))
    )
  def Pair: Rule1[(String, J)] = rule(JsonStringUnwrapped ~ ws(':') ~ Value ~> ((k: String, v: J) => (k, v)))
  def Value: Rule1[J] = rule(JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull)
  def JsonString: Rule1[J] = rule(JsonStringUnwrapped ~> ((s: String) => JStr(s)))
  def JsonStringUnwrapped: Rule1[String] =
    rule('"' ~ capture(zeroOrMore(NormalChar | '\\' ~ ANY)) ~ ws('"') ~> ((s: String) => unescape(s)))
  def NormalChar: Rule0 = rule(!Parboiled2Json.QuoteBackslash ~ ANY)
  def JsonNumber: Rule1[J] =
    rule(capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> ((s: String) => JNum(s.toDouble)) ~ WhiteSpace)
  def JsonArray: Rule1[J] =
    rule(ws('[') ~ zeroOrMore(Value).separatedBy(ws(',')) ~ ws(']') ~> ((items: Seq[J]) => JArr(items.toList)))
  def Integer: Rule0 = rule(optional('-') ~ (Digit19 ~ Digits | Digit))
  def Digits: Rule0 = rule(oneOrMore(Digit))
  def Frac: Rule0 = rule("." ~ Digits)
  def Exp: Rule0 = rule(ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits)
  def JsonTrue: Rule1[J] = rule("true" ~ WhiteSpace ~ push(JBool(true): J))
  def JsonFalse: Rule1[J] = rule("false" ~ WhiteSpace ~ push(JBool(false): J))
  def JsonNull: Rule1[J] = rule("null" ~ WhiteSpace ~ push(JNull: J))
  def WhiteSpace: Rule0 = rule(zeroOrMore(Parboiled2Json.WhiteSpaceChar))
  def ws(c: Char): Rule0 = rule(c ~ WhiteSpace)
}
object Parboiled2Json {
  val WhiteSpaceChar: org.parboiled2.CharPredicate = org.parboiled2.CharPredicate(" \n\r\t\f")
  val QuoteBackslash: org.parboiled2.CharPredicate = org.parboiled2.CharPredicate("\"\\")

  def parse(input: String): ParserModel.J = new Parboiled2Json(input).Json.run().get
}

object CatsParseJson {
  import cats.parse.{Numbers, Parser as P, Parser0 as P0}
  import ParserModel.*

  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val whitespaces0: P0[Unit] = whitespace.rep0.void

  val parser: P[J] = P.recursive[J] { recurse =>
    val pnull = P.string("null").as(JNull: J)
    val bool = P.string("true").as(JBool(true): J).orElse(P.string("false").as(JBool(false): J))
    val justStr = cats.parse.strings.Json.delimited.parser
    val str = justStr.map(s => JStr(s): J)
    val num = Numbers.jsonNumber.map(s => JNum(s.toDouble): J)
    val listSep: P[Unit] = P.char(',').soft.surroundedBy(whitespaces0).void
    def rep0[A](pa: P[A]): P0[List[A]] = pa.repSep0(listSep).surroundedBy(whitespaces0)
    val list = rep0(recurse).with1.between(P.char('['), P.char(']')).map(vs => JArr(vs): J)
    val kv: P[(String, J)] = justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)
    val obj = rep0(kv).with1.between(P.char('{'), P.char('}')).map(vs => JObj(vs): J)
    P.oneOf(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  val file: P[J] = parser.between(whitespaces0, whitespaces0 ~ P.end)

  def parse(input: String): J = file.parseAll(input).fold(e => throw new Exception(e.toString), identity)
}

object ParsleyJson {
  import parsley.Parsley
  import parsley.Parsley.{atomic, many}
  import parsley.character.{char, item, satisfy, string, stringOfSome, whitespaces}
  import parsley.combinator.sepBy
  import ParserModel.*

  private def lexeme[A](p: Parsley[A]): Parsley[A] = p <* whitespaces
  private def tok(c: Char): Parsley[Char] = lexeme(char(c))

  private val chunk: Parsley[String] =
    stringOfSome(c => c != '"' && c != '\\') <|> (char('\\') *> item).map(c => "\\" + c)
  private val rawString: Parsley[String] =
    lexeme(char('"') *> many(chunk) <* char('"')).map(parts => unescape(parts.mkString))
  private val digits: Parsley[String] = stringOfSome(c => c >= '0' && c <= '9')
  private val number: Parsley[J] = lexeme(
    (parsley.combinator.option(char('-')) <~> digits <~> parsley.combinator.option(char('.') *> digits) <~>
      parsley.combinator.option(
        satisfy(c => c == 'e' || c == 'E') *> parsley.combinator.option(satisfy(c => c == '+' || c == '-')) <~> digits
      ))
      .map { case (((sign, int), frac), exp) =>
        val text = sign.fold("")(_.toString) + int + frac.fold("")("." + _) +
          exp.fold("") { case (s, d) => "e" + s.fold("")(_.toString) + d }
        JNum(text.toDouble): J
      }
  )
  private lazy val value: Parsley[J] =
    (rawString.map(s => JStr(s): J) <|> number <|> array <|> obj <|>
      lexeme(atomic(string("true"))).as(JBool(true): J) <|> lexeme(atomic(string("false"))).as(JBool(false): J) <|>
      lexeme(string("null")).as(JNull: J))
  private lazy val array: Parsley[J] = (tok('[') *> sepBy(value, tok(',')) <* tok(']')).map(items => JArr(items): J)
  private lazy val member: Parsley[(String, J)] = (rawString <* tok(':')) <~> value
  private lazy val obj: Parsley[J] = (tok('{') *> sepBy(member, tok(',')) <* tok('}')).map(fields => JObj(fields): J)

  val json: Parsley[J] = whitespaces *> value <* parsley.combinator.eof

  def parse(input: String): J = json.parse(input).get
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ParserJsonBenchmark {
  import ParserModel.*

  @Setup def check(): Unit = {
    val expected = KindlingsParsers.generated.parse(input)
    val results = List(
      "interpreted" -> KindlingsParsers.interpreted.parse(input),
      "reader" -> KindlingsParsers.generated.parse(new java.io.StringReader(input), 8192),
      "fastparse" -> FastparseJson.parse(input),
      "parboiled2" -> Parboiled2Json.parse(input),
      "cats-parse" -> CatsParseJson.parse(input),
      "parsley" -> ParsleyJson.parse(input)
    )
    results.foreach { case (name, result) =>
      if (result != expected) throw new IllegalStateException(s"$name produced a different AST")
    }
  }

  @Benchmark def kindlingsGenerated: J = KindlingsParsers.generated.parse(input)
  @Benchmark def kindlingsInterpreted: J = KindlingsParsers.interpreted.parse(input)
  @Benchmark def kindlingsGeneratedReader: J = KindlingsParsers.generated.parse(new java.io.StringReader(input), 8192)
  @Benchmark def fastparse: J = FastparseJson.parse(input)
  @Benchmark def parboiled2: J = Parboiled2Json.parse(input)
  @Benchmark def catsParse: J = CatsParseJson.parse(input)
  @Benchmark def parsley: J = ParsleyJson.parse(input)
  @Benchmark def circeJawn: io.circe.Json = io.circe.parser.parse(input).fold(throw _, identity)
}
