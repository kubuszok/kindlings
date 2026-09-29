package hearth.kindlings.parser
package catseffect

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import hearth.MacroSuite

import java.io.StringReader

final class CatsEffectEngineSpec extends MacroSuite {

  import CatsEffectEngineSpec.*

  group("Cats Effect engine") {

    test("effectful actions are sequenced in parse order") {
      (for {
        log <- Ref.of[IO, List[Int]](Nil)
        sum <- summing(log).parse("1 2 3 4")
        logged <- log.get
      } yield {
        sum ==> 10
        logged.reverse ==> List(1, 2, 3, 4)
      }).unsafeToFuture()
    }

    test("running the same F[R] twice parses twice (fresh machine per run)") {
      (for {
        log <- Ref.of[IO, List[Int]](Nil)
        io = summing(log).parse("5 6")
        first <- io
        second <- io
        logged <- log.get
      } yield {
        (first, second) ==> ((11, 11))
        logged.size ==> 4
      }).unsafeToFuture()
    }

    test("syntax errors are raised in F") {
      Ref
        .of[IO, List[Int]](Nil)
        .flatMap(log => summing(log).parse("1 +").attempt)
        .map {
          case Left(e: ParseError) => e.endOfInput ==> false
          case other               => fail(s"unexpected $other")
        }
        .unsafeToFuture()
    }

    test("readers are read with blocking, a tiny budget still parses") {
      val input = (1 to 5000).mkString(" ")
      Ref
        .of[IO, List[Int]](Nil)
        .flatMap { log =>
          implicit val engine: ParserEngine[IO] = CatsEffectEngine.async[IO](budget = 3)
          val parser = summingWith(log)
          parser.parse(new StringReader(input), bufferSize = 5)
        }
        .map(_ ==> (1 to 5000).sum)
        .unsafeToFuture()
    }
  }
}
object CatsEffectEngineSpec {

  def summing(log: Ref[IO, List[Int]]): Parser[IO, Int] = summingWith(log)

  def summingWith(log: Ref[IO, List[Int]])(implicit engine: ParserEngine[IO]): Parser[IO, Int] =
    Grammar.grammar[Int, IO] { g =>
      import g.*
      val sum = nonTerminal[Int]
      val num = terminal("[0-9]+").map(_.toInt)
      skip(" +")
      sum ::= (
        all(num)(n => log.update(n :: _).as(n)) ||
          all(sum, num)((s, n) => log.update(n :: _).as(s + n))
      )
      sum
    }
}
