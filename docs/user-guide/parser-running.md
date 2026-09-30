# Running parsers

## Effects and engines

A grammar is compiled for one effect `F`: `Grammar.grammar[Result, F]` produces a `Parser[F, Result]` whose
`parse(input)` returns `F[Result]`. Actions are type-checked against it: `all(...) { ... }` must return `F[R]`, while
`all(...).pure { ... }` returns `R` and never touches `F`. The parser runs pure actions, lexing and all its own work
inline, and hands a value to the effect only when an effectful action produced one.

The engine that runs a parse is found implicitly as a `ParserEngine[F]`. Built-in engines:

| `F` | Effectful action | Syntax error |
|---|---|---|
| `Id` | returns the value | throws `ParseError` |
| `Option` | `None` stops the parse | `None` |
| `Try` | `Failure` stops the parse | `Failure(ParseError)` |
| `Either[E, *]` | `Left` stops the parse | `Left(...)` via a `ParseErrorLift[E]`: built in for `ParseError`, `Throwable` and `String` (the message) |
| `Future` | one continuation per effectful action; effects run in parse order (needs an implicit `ExecutionContext`) | failed future |

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

type Result[A] = Either[String, A]

val numbers: Parser[Result, List[Int]] = Grammar.grammar[List[Int], Result] { g =>
  import g._
  val list = nonTerminal[List[Int]]
  val num = terminal("-?[0-9]+").map(_.toInt)
  skip(" +")
  list ::= all(sepBy1(all(num)(n => if (n < 0) Left(s"negative: $n") else Right(n)), ",")).pure(ns => ns)
  list
}

println(numbers.parse("1, 2, 3"))
println(numbers.parse("1, -2, 3"))
println(numbers.parse("1, 2,"))
// expected output:
// Right(List(1, 2, 3))
// Left(negative: -2)
// Left(Unexpected token at 1:6: found end of input, expected num)
```

Other runtimes plug in by providing a `ParserEngine[F]`, like the Cats Effect module below. Collections and value types
whose smart constructor can reject values (see [Writing grammars](parser-grammars.md#choosing-the-collection)) also
need an `ErrorChannel[F]`, which the built-in effects other than `Id`, and Cats Effect `F`s, provide.

### Cats Effect

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-cats-effect" % "{{ kindlings_version() }}"
    ```

`import hearth.kindlings.parser.catseffect._` provides an engine (and an error channel) for every `Async[F]` or
`Sync[F]`:

```scala
//> using dep com.kubuszok::kindlings-parser-cats-effect:{{ kindlings_version() }}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import hearth.kindlings.parser._
import hearth.kindlings.parser.catseffect._

val parser: Parser[IO, Int] = Grammar.grammar[Int, IO] { g =>
  import g._
  val sum = nonTerminal[Int]
  val num = terminal("[0-9]+").map(_.toInt)
  skip(" +")
  sum ::= (
    all(num)(n => IO.println(s"read $n").as(n)) ||
      all(sum, num)((s, n) => IO.println(s"read $n").as(s + n))
  )
  sum
}

println(parser.parse("1 2 3").unsafeRunSync())
// expected output:
// read 1
// read 2
// read 3
// 6
```

- pure actions, lexing and parsing run inside one `delay` segment; each effectful action costs exactly one `flatMap`,
  whose continuation runs the next pure segment;
- every `budget` steps (default `CatsEffectEngine.DefaultBudget`) the engine `cede`s so that long parses do not starve
  other fibers; pick another budget with `implicit val engine = CatsEffectEngine.async[IO](budget = ...)`;
- `Reader`/`InputStream` inputs are read with `Sync.blocking`;
- the parse starts when the `F[R]` runs: running the same `parser.parse(input)` value twice parses twice.

## Inputs

- `parser.parse(input: String)` reads the string in place (and is the input of the recursive-descent fast path);
- `parser.parse(reader: java.io.Reader, bufferSize = Parser.DefaultBufferSize)` and
  `parser.parse(stream: java.io.InputStream)` (UTF-8) read the input in chunks: text that has been parsed is discarded as
  parsing progresses, so memory stays bounded by the buffer and the longest token, whatever the input size. Positions
  are `Long`s. Readers and streams are never closed by the parser.

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val count: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
  import g._
  val words = nonTerminal[Int]
  skip("[ \\n]+")
  words ::= all(rep(terminal("[a-z]+"))).pure(_.size)
  words
}

val text = "lorem ipsum dolor\n" * 10000
println(count.parse(new java.io.StringReader(text), bufferSize = 64))
// expected output:
// 30000
```

### fs2 streams

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-fs2" % "{{ kindlings_version() }}"
    ```

`import hearth.kindlings.parser.streams._` adds pipes that parse a whole stream as one input:

| Pipe | Parser | Stream |
|---|---|---|
| `parser.pipe` | `Parser[F, R]` (effectful actions run in `F`) | `Stream[F, String]` |
| `parser.bytePipe` | `Parser[F, R]` | `Stream[F, Byte]` (UTF-8) |
| `parser.pipeIn[G]` | `Parser[Id, R]` (pure grammars) | `Stream[G, String]`, any `G` |
| `parser.bytePipeIn[G]` | `Parser[Id, R]` | `Stream[G, Byte]` (UTF-8) |

```scala
//> using dep com.kubuszok::kindlings-parser-fs2:{{ kindlings_version() }}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import hearth.kindlings.parser._
import hearth.kindlings.parser.streams._

val sum: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
  import g._
  val sum = nonTerminal[Int]
  skip("[ \\n]+")
  sum ::= all(rep(terminal("[0-9]+").map(_.toInt))).pure(_.sum)
  sum
}

val chunks = Stream("1 2 ", "3", "4 5\n", "6").covary[IO] // "34" is one token split across chunks
println(chunks.through(sum.pipeIn[IO]).compile.lastOrError.unsafeRunSync()) // 1 + 2 + 34 + 5 + 6
// expected output:
// 48
```

Chunks are fed to the parser as it needs them and already-parsed text is discarded, so streams of any size are parsed
with bounded memory (plus whatever your actions build). Multi-byte UTF-8 characters split across chunks are handled.

### Push API (integrations and REPLs)

`parser.pushMachine()` returns a low-level machine fed with `machine.feed(chunk)` / `machine.endOfInput()` and driven
with `machine.run(budget)`, which returns `Machine.NeedInput`, `Machine.Effect` (run `machine.pendingEffect`, then
`machine.resume(result)`), `Machine.Yield` (the budget was spent), `Machine.Done` (`machine.result`) or
`Machine.Error` (`machine.error`). The engines and the fs2 pipes are built on it.

## Syntax errors

`ParseError` (an `Exception`) carries where the error is (`offset`, `line`, `column`), what was `found` and which
tokens were `expected`:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val sums: Parser[scala.util.Try, Int] = Grammar.grammar[Int, scala.util.Try] { g =>
  import g._
  val sum = nonTerminal[Int]
  val num = terminal("[0-9]+").map(_.toInt)
  skip(" +")
  sum ::= all(sepBy1(num, "+")).pure(_.sum)
  sum
}

sums.parse("1 + 2 + + 3").failed.foreach { case e: ParseError =>
  println(e.getMessage)
  println((e.offset, e.line, e.column, e.found, e.expected, e.endOfInput))
}
sums.parse("1 + 2 +").failed.foreach { case e: ParseError => println(e.endOfInput) }
// expected output:
// Unexpected token at 1:9: found "+", expected num
// (8,1,9,"+",List(num),false)
// true
```

`endOfInput` is `true` when the input ended while the parser still expected more: the input is a valid prefix. A REPL
can use it to ask for another line instead of reporting an error. Values rejected by a collection's smart constructor
are reported as `ParseError`s too.
