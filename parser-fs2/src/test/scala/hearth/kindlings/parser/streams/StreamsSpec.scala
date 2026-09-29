package hearth.kindlings.parser
package streams

import _root_.fs2.{Chunk, Stream}
import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import hearth.MacroSuite
import hearth.kindlings.parser.catseffect.*

final class StreamsSpec extends MacroSuite {

  import StreamsSpec.*

  group("fs2 pipes") {

    test("tokens split across many tiny chunks") {
      val text = (1 to 3000).map(i => s"k$i = $i").mkString("\n")
      Stream
        .emits(text.grouped(3).toSeq)
        .covary[IO]
        .through(assignments.pipeIn[IO])
        .compile
        .lastOrError
        .map(_ ==> (1 to 3000).sum)
        .unsafeToFuture()
    }

    test("UTF-8 bytes with multi-byte chars split across chunks") {
      val bytes = "zażółć = 1\ngęślą = 2".getBytes("UTF-8").toIndexedSeq
      Stream
        .chunk(Chunk.from(bytes))
        .covary[IO]
        .chunkLimit(1)
        .unchunks
        .through(assignments.bytePipeIn[IO])
        .compile
        .lastOrError
        .map(_ ==> 3)
        .unsafeToFuture()
    }

    test("a large generated stream is parsed with bounded buffers") {
      val count = 100000
      Stream
        .range(1, count + 1)
        .map(i => s"a$i = ${i % 10}\n")
        .covary[IO]
        .through(assignments.pipeIn[IO])
        .compile
        .lastOrError
        .map(_ ==> (1 to count).map(_ % 10).sum)
        .unsafeToFuture()
    }

    test("effectful actions are evaluated in the stream effect, in order") {
      (for {
        log <- Ref.of[IO, List[Int]](Nil)
        result <- Stream("1 2", " 3 ", "4").covary[IO].through(summing(log).pipe).compile.lastOrError
        logged <- log.get
      } yield {
        result ==> 10
        logged.reverse ==> List(1, 2, 3, 4)
      }).unsafeToFuture()
    }

    test("syntax errors are raised in the stream; incomplete input is flagged") {
      Stream("k1 = ")
        .covary[IO]
        .through(assignments.pipeIn[IO])
        .compile
        .lastOrError
        .attempt
        .map {
          case Left(e: ParseError) => e.endOfInput ==> true
          case other               => fail(s"unexpected $other")
        }
        .unsafeToFuture()
    }

    test("running the same stream twice parses twice") {
      val stream = Stream("k = 1\n", "j = 2").covary[IO].through(assignments.pipeIn[IO])
      (for {
        first <- stream.compile.lastOrError
        second <- stream.compile.lastOrError
      } yield (first, second) ==> ((3, 3))).unsafeToFuture()
    }
  }
}
object StreamsSpec {

  val assignments: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
    import g.*
    val program = nonTerminal[Int]
    val assignment = nonTerminal[Int]
    val name = terminal("[a-zżółćęśąźń][a-z0-9żółćęśąźń]*")
    val number = terminal("[0-9]+").map(_.toInt)
    skip("[ \\t\\r\\n]+")
    assignment ::= all(name, "=", number).pure((_, _, n) => n)
    program ::= all(rep1(assignment)).pure(_.sum)
    program
  }

  def summing(log: Ref[IO, List[Int]]): Parser[IO, Int] = Grammar.grammar[Int, IO] { g =>
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
