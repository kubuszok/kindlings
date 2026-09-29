package hearth.kindlings.parser

import hearth.MacroSuite

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

final class EnginesSpec extends MacroSuite {

  import EnginesSpec.*

  group("built-in engines") {

    test("Id: effectful actions return values directly, errors are thrown") {
      val log = ListBuffer.empty[String]
      sumId(log).parse("1 2 3") ==> 6
      log.toList ==> List("1", "2", "3")
      intercept[ParseError](sumId(log).parse("1 x"))
    }

    test("Option: an action returning None stops the parse; syntax errors are None") {
      sumOption.parse("1 2 3") ==> Some(6)
      sumOption.parse("1 0 3") ==> None
      sumOption.parse("1 +") ==> None
    }

    test("Try: an action returning Failure stops the parse; syntax errors are Failure(ParseError)") {
      sumTry.parse("4 5") ==> Success(9)
      sumTry.parse("4 13") match {
        case Failure(e) => e.getMessage ==> "unlucky 13"
        case other      => fail(s"unexpected $other")
      }
      sumTry.parse("4 ?") match {
        case Failure(_: ParseError) => ()
        case other                  => fail(s"unexpected $other")
      }
    }

    test("Future: effects run in order, one continuation per effectful action") {
      implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
      val log = ListBuffer.empty[String]
      val parser = sumFuture(log)
      parser.parse("1 2 3").map { result =>
        result ==> 6
        log.toList ==> List("1", "2", "3")
      }
    }

    test("Future: syntax errors fail the future") {
      implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
      sumFuture(ListBuffer.empty).parse("1 ,").transform {
        case Failure(_: ParseError) => Success(())
        case other                  => Failure(new AssertionError(s"unexpected $other"))
      }
    }
  }

  group("literal values and empty alternatives") {

    test("literal alternatives are singleton values; \"\" is the empty alternative") {
      suffixes.parse("Smith Jr.") ==> ("Smith", "Jr.")
      suffixes.parse("Smith Sr.") ==> ("Smith", "Sr.")
      suffixes.parse("Smith") ==> ("Smith", "")
    }

    test("keywords win over identifiers of the same length, longer identifiers win over keywords") {
      keywords.parse("if iffy then x") ==> List("KW:if", "ID:iffy", "KW:then", "ID:x")
    }

    test("opt and rep") {
      optRep.parse("a b b b") ==> (Some("a"), 3)
      optRep.parse("b") ==> (None, 1)
      optRep.parse("") ==> (None, 0)
    }
  }
}
object EnginesSpec {

  def sumId(log: ListBuffer[String]): Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val sum = nonTerminal[Int]
    val num = terminal("[0-9]+")
    skip(" +")
    sum ::= (
      all(num) { n => log += n; n.toInt } ||
        all(sum, num) { (s, n) => log += n; s + n.toInt }
    )
    sum
  }

  val sumOption: Parser[Option, Int] = Grammar.grammar[Int, Option] { g =>
    import g.*
    val sum = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    sum ::= (
      all(num)(n => if (n == 0) None else Some(n)) ||
        all(sum, num)((s, n) => if (n == 0) None else Some(s + n))
    )
    sum
  }

  val sumTry: Parser[Try, Int] = Grammar.grammar[Int, Try] { g =>
    import g.*
    val sum = nonTerminal[Int]
    val num = terminal("[0-9]+").map(_.toInt)
    skip(" +")
    sum ::= (
      all(num).pure(n => n) ||
        all(sum, num)((s, n) => if (n == 13) Failure(new Exception("unlucky 13")) else Success(s + n))
    )
    sum
  }

  def sumFuture(log: ListBuffer[String])(implicit ec: scala.concurrent.ExecutionContext): Parser[Future, Int] =
    Grammar.grammar[Int, Future] { g =>
      import g.*
      val sum = nonTerminal[Int]
      val num = terminal("[0-9]+").map(_.toInt)
      skip(" +")
      sum ::= (
        all(num)(n => Future { log += n.toString; n }) ||
          all(sum, num)((s, n) => Future { log += n.toString; s + n })
      )
      sum
    }

  val suffixes: Parser[Id, (String, String)] = Grammar.grammar[(String, String), Id] { g =>
    import g.*
    val name = nonTerminal[(String, String)]
    val suffix = nonTerminal[String]
    val word = terminal("[A-Z][a-z]+")
    skip(" +")
    suffix ::= "Sr." || "Jr." || ""
    name ::= all(word, suffix).pure((w, s) => w -> s)
    name
  }

  val keywords: Parser[Id, List[String]] = Grammar.grammar[List[String], Id] { g =>
    import g.*
    val tokens = nonTerminal[List[String]]
    val token = nonTerminal[String]
    val ident = terminal("[a-z]+")
    skip(" +")
    token ::= (
      all("if" || "then").pure(k => s"KW:$k") ||
        all(ident).pure(i => s"ID:$i")
    )
    tokens ::= all(rep1(token)).pure(ts => ts)
    tokens
  }

  val optRep: Parser[Id, (Option[String], Int)] = Grammar.grammar[(Option[String], Int), Id] { g =>
    import g.*
    val start = nonTerminal[(Option[String], Int)]
    skip(" +")
    start ::= all(opt("a"), rep("b")).pure((a, bs) => a -> bs.size)
    start
  }
}
