package hearth.kindlings.parser

import hearth.MacroSuite

final class JsonSpec extends MacroSuite {

  import JsonSpec.*
  import Json.*

  group("JSON grammar (Either[String, *] engine)") {

    test("parses nested documents with sepBy, groups and literal values") {
      json.parse("""{"a": [1, 2.5, -3e2], "b": {"c": null, "d": [true, false]}, "e": "x\"y", "f": []}""") ==> Right(
        JObj(
          List(
            "a" -> JArr(List(JNum(1), JNum(2.5), JNum(-300))),
            "b" -> JObj(List("c" -> JNull, "d" -> JArr(List(JBool(true), JBool(false))))),
            "e" -> JStr("x\\\"y"),
            "f" -> JArr(Nil)
          )
        )
      )
    }

    test("keyword literals win over longer-or-equal regex matches only when they are the longest match") {
      json.parse("[true, null]") ==> Right(JArr(List(JBool(true), JNull)))
    }

    test("syntax errors are lifted into the error channel") {
      val result = json.parse("""{"a": 1,}""")
      assert(result.isLeft, result)
      assert(result.left.exists(_.contains("expected string")), result)
    }

    test("an effectful action returning Left stops the parse") {
      jsonNoNegatives.parse("[1, 2]") ==> Right(JArr(List(JNum(1), JNum(2))))
      jsonNoNegatives.parse("[1, -2, 3]") ==> Left("negative number: -2")
    }

    test("effectful alternatives can be grouped inside helpers (user guide example)") {
      numbers.parse("1, 2, 3") ==> Right(List(1, 2, 3))
      numbers.parse("1, -2") ==> Left("negative: -2")
    }

    test("very large arrays are parsed without deep recursion") {
      val n = 50000
      json.parse((1 to n).mkString("[", ",", "]")).map {
        case JArr(items) => items.size
        case _           => -1
      } ==> Right(n)
    }
  }
}
object JsonSpec {

  sealed trait Json
  object Json {
    case object JNull extends Json
    final case class JBool(value: Boolean) extends Json
    final case class JNum(value: Double) extends Json
    final case class JStr(value: String) extends Json
    final case class JArr(items: List[Json]) extends Json
    final case class JObj(fields: List[(String, Json)]) extends Json
  }
  import Json.*

  type Result[A] = Either[String, A]

  val json: Parser[Result, Json] = Grammar.grammar[Json, Result] { g =>
    import g.*
    val value = nonTerminal[Json]
    val member = nonTerminal[(String, Json)]
    val string = terminal("\"([^\"\\\\]|\\\\.)*\"").map(s => s.substring(1, s.length - 1))
    val number = terminal("-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?").map(_.toDouble)
    skip("[ \\t\\r\\n]+")
    value ::= (
      all("{", sepBy(member, ","), "}").pure((_, members, _) => JObj(members)) ||
        all("[", sepBy(value, ","), "]").pure((_, items, _) => JArr(items)) ||
        all(string).pure(s => JStr(s)) ||
        all(number).pure(n => JNum(n)) ||
        all("true" || "false").pure(b => JBool(b == "true")) ||
        all("null").pure(_ => JNull)
    )
    member ::= all(string, ":", value).pure((k, _, v) => k -> v)
    value
  }

  val numbers: Parser[Result, List[Int]] = Grammar.grammar[List[Int], Result] { g =>
    import g.*
    val list = nonTerminal[List[Int]]
    val num = terminal("-?[0-9]+").map(_.toInt)
    skip(" +")
    list ::= all(sepBy1(all(num)(n => if (n < 0) Left(s"negative: $n") else Right(n)), ",")).pure(ns => ns)
    list
  }

  val jsonNoNegatives: Parser[Result, Json] = Grammar.grammar[Json, Result] { g =>
    import g.*
    val value = nonTerminal[Json]
    val number = terminal("-?[0-9]+").map(_.toDouble)
    skip("[ ]+")
    value ::= (
      all("[", sepBy(value, ","), "]").pure((_, items, _) => JArr(items)) ||
        all(number)(n => if (n < 0) Left(s"negative number: ${n.toInt}") else Right(JNum(n)))
    )
    value
  }
}
