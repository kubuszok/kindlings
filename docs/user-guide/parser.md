# Parser (yacc-style grammars)

Write a grammar in BNF-like syntax directly in Scala, with typed semantic actions; a macro turns it into an
**LALR(1) parser at compile time**:

- grammar problems (shift/reduce and reduce/reduce conflicts, non-terminals without productions, terminals matching the
  empty string, unsupported regex features, ...) are **compile errors pointing at the offending production**;
- the lexer (a DFA built from all terminals) and the LR tables are computed during compilation and embedded in the
  generated code, together with the code of your actions and `.map` conversions, inlined into one `switch` (values that
  an action ignores, e.g. `_` parameters, are never converted);
- parsing runs on an explicit, heap-allocated stack: **nesting depth is not limited by the JVM thread stack**;
- semantic actions can be pure or return the effect `F` of your choice (`Id`, `Option`, `Try`, `Either[E, *]`,
  `Future`, or any runtime providing a `ParserEngine[F]`).

It works on Scala 2.13 and Scala 3, on the JVM, Scala.js and Scala Native.

!!! warning "Early stage"

    This module is new. The grammar syntax is settled, but engines, input sources and code generation will
    evolve. See `docs/research/parser-library-research.md` in the repository for the design.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}
    ```

## Quick start

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val calc: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
  import g._
  val expr = nonTerminal[Int]
  val num = terminal("[0-9]+").map(_.toInt)
  skip("[ \\t]+")
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

println(calc.parse("1 + 2 * (3 - 1)"))
// expected output:
// 5
```

`Grammar.grammar[Result, F] { g => import g._; ... }` defines the grammar; the block's last expression is the start
symbol. `Result` is the type produced by the start symbol and `F` is the effect of actions and of the whole parse:
`Parser[F, Result].parse(input): F[Result]`.

## Grammar syntax

Declarations come first, productions after them (the compiler enforces that order).

| Construct | Meaning |
|---|---|
| `val expr = nonTerminal[A]` | a non-terminal producing `A` |
| `val num = terminal("[0-9]+")` | a terminal (token) defined by a regular expression; its value is the matched `String` |
| `.map(f)` on a terminal | converts the matched text, e.g. `terminal("[0-9]+").map(_.toInt)` |
| `"+"` inline | a literal terminal; its value is the literal itself |
| `"[a-z]+".r` inline | an inline regex terminal |
| `nt ::= alternatives` | adds productions to `nt` (can be used several times) |
| `all(s1, ..., sN) { (v1, ..., vN) => F[R] }` | an alternative with an **effectful** action |
| `all(s1, ..., sN).pure { (v1, ..., vN) => R }` | an alternative with a **pure** action |
| `a \|\| b` | combines alternatives |
| a single symbol or literal as an alternative | passes its value through (`suffix ::= "Sr." \|\| "Jr." \|\| ""`) |
| `""` as a whole alternative | the empty alternative (its value is `""`) |
| `"+" \|\| "-"` used as a symbol | an inline group (an anonymous helper non-terminal) |
| `opt(s)`, `rep(s)`, `rep1(s)` | `Option[A]`, `List[A]` (zero or more), `List[A]` (one or more) |
| `sepBy(s, sep)`, `sepBy1(s, sep)` | `List[A]` of `s` separated by `sep` |
| `rep(s).as[C]` (also `rep1`, `sepBy`, `sepBy1`) | the repetition collected into `C` instead of a `List` (see below) |
| `left(...)`, `right(...)`, `nonassoc(...)` | operator precedence levels, lowest first (yacc's `%left`, ...) |
| `all(...).prec(op)` | the production takes `op`'s precedence (yacc's `%prec`) |
| `skip("[ \\t\\n]+")` | text skipped between tokens (whitespace, comments) |

Lexing uses the longest match; when a literal and a regex match the same text (a keyword and an identifier), the literal
wins.

### Choosing the collection of a repetition

Repetitions produce a `List` by default. `.as[C]` collects them into any collection that Hearth's `IsCollection`
standard extension supports: Scala collections (`Vector`, `Set`, `ArrayBuffer`, ...), arrays, Java collections (JVM
only), and collections from providers on the classpath, e.g. cats `NonEmptyList`, `NonEmptyVector` or `Chain` with
`kindlings-cats-integration`:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}
import hearth.kindlings.parser._
import scala.collection.immutable.ArraySeq

val numbers: Parser[Id, ArraySeq[Int]] = Grammar.grammar[ArraySeq[Int], Id] { g =>
  import g._
  val list = nonTerminal[ArraySeq[Int]]
  val num = terminal("[0-9]+").map(_.toInt)
  skip(" +")
  list ::= all("[", sepBy(num, ",").as[ArraySeq[Int]], "]").pure((_, ns, _) => ns)
  list
}
println(numbers.parse("[1, 2, 3]"))
// expected output:
// ArraySeq(1, 2, 3)
```

The macro checks that the collection is supported and that the repeated values fit its element type (maps are
rejected). The generated code creates the collection's own `Builder` when the repetition starts, appends each value as
it is parsed and calls `result()` once, so no intermediate `List` is built. The builder's `Factory` is evaluated once
per parser. `.as[C]` requires `Grammar.grammar`: `Grammar.interpreted` always builds `List`s.

A collection with a smart constructor (cats `NonEmptyList`) can reject the values, e.g. an empty `rep`. The rejection is
reported as a `ParseError` ("Invalid cats.data.NonEmptyList[scala.Int]: ...") through the effect's error channel, so
such collections compile only in an `F` that has an `ErrorChannel[F]`: `Option`, `Try`, `Either[E, *]` (with a
`ParseErrorLift[E]`), `Future`, and cats-effect `F`s with `import hearth.kindlings.parser.catseffect._`. In `Id` they
are a compile error, unless you explicitly opt into throwing the rejection as a `ParseError`:

```scala
import hearth.kindlings.parser.ErrorChannel.throwing._
```

Prefer `rep1`/`sepBy1` for non-empty collections: the grammar then guarantees at least one value.

### Writing alternatives on several lines

Scala 2.13 does not continue an expression on a line starting with `||`, so put several alternatives in parentheses and
end each line with `||` (this layout works on both Scala 2.13 and 3):

```scala
expr ::= (
  all(expr, "+", expr).pure((a, _, b) => a + b) ||
    all(num).pure(n => n)
)
```

Sequences are limited to 22 symbols (a Scala 2 function arity limit); split longer ones into helper non-terminals.

## Effects

Actions are type-checked against the grammar's effect: `all(...) { ... }` must return `F[R]`, `all(...).pure { ... }`
returns `R` and never touches `F`. Only effectful actions involve the effect: the parser runs pure actions, lexing and
all shifts/reductions inline, and hands a value to the engine only when an effectful action produced one.

| `F` | Effectful action | Syntax error |
|---|---|---|
| `Id` | returns the value | throws `ParseError` |
| `Option` | `None` stops the parse | `None` |
| `Try` | `Failure` stops the parse | `Failure(ParseError)` |
| `Either[E, *]` | `Left` stops the parse | `Left(...)` via `ParseErrorLift[E]` (`ParseError`, `Throwable`, `String`) |
| `Future` | one continuation per effectful action; effects run in parse order | failed future |

```scala
type Result[A] = Either[String, A]

val numbers: Parser[Result, List[Int]] = Grammar.grammar[List[Int], Result] { g =>
  import g._
  val list = nonTerminal[List[Int]]
  val num = terminal("-?[0-9]+").map(_.toInt)
  skip(" +")
  list ::= all(sepBy1(all(num)(n => if (n < 0) Left(s"negative: $n") else Right(n)), ",")).pure(ns => ns)
  list
}
```

Other runtimes plug in by providing a `ParserEngine[F]` from their own modules.

### Cats Effect

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-cats-effect" % "{{ kindlings_version() }}"
    ```

`import hearth.kindlings.parser.catseffect._` provides an engine for every `Async[F]` (or `Sync[F]`):

```scala
import cats.effect.IO
import hearth.kindlings.parser._
import hearth.kindlings.parser.catseffect._

val parser: Parser[IO, Int] = Grammar.grammar[Int, IO] { g =>
  import g._
  val sum = nonTerminal[Int]
  val num = terminal("[0-9]+").map(_.toInt)
  skip(" +")
  sum ::= (
    all(num)(n => IO.println(n).as(n)) ||
      all(sum, num)((s, n) => IO.println(n).as(s + n))
  )
  sum
}
```

- pure actions, lexing and all shifts/reductions run inside one `delay` segment; each effectful action costs exactly
  one `flatMap`, whose continuation runs the next pure segment;
- every `budget` steps (default `CatsEffectEngine.DefaultBudget`) the engine `cede`s so long parses do not starve other
  fibers; pick another budget with `CatsEffectEngine.async[IO](budget = ...)`;
- `Reader`/`InputStream` inputs are read with `Sync.blocking`;
- the machine is allocated when the `F[R]` runs: running the same `parser.parse(input)` value twice parses twice.

## Compile-time diagnostics

Conflicts are reported at the production that causes them, together with the LR state (abridged):

```
shift/reduce conflict on "+": after `e ::= e "+" e` the parser can either reduce by it or shift "+".
  Resolve it by declaring the precedence/associativity of "+" (left/right/nonassoc) and of this production,
  or by restructuring the grammar.
  LR state 5:
      e ::= e "+" e .
      e ::= e . "+" e
```

Other checks: non-terminals without productions, non-terminals that cannot derive any finite input, unreachable
non-terminals (warning), terminals matching the empty string, invalid or unsupported regular expressions (anchors,
back-references, look-arounds and lazy quantifiers cannot be compiled into a lexer DFA), non-literal patterns.

## Inputs

- `parser.parse(input: String)` reads the string in place;
- `parser.parse(reader: java.io.Reader, bufferSize = ...)` and `parser.parse(stream: java.io.InputStream)` (UTF-8)
  read the input in chunks: text that has been parsed is discarded as parsing progresses, so memory stays bounded by
  the buffer and the longest token, whatever the input size. Positions are `Long`s.

Readers are never closed by the parser.

### fs2 streams

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-fs2" % "{{ kindlings_version() }}"
    ```

`import hearth.kindlings.parser.streams._` adds pipes that parse a whole stream as one input:

```scala
import hearth.kindlings.parser.streams._

bytes.through(parser.bytePipe).compile.lastOrError    // Parser[IO, R]: effectful actions run in IO
chunks.through(parser.pipe).compile.lastOrError       // Stream[IO, String]
chunks.through(pureParser.pipeIn[IO])                 // Parser[Id, R] in any stream effect
```

Chunks are fed to the parser as it needs them and already-parsed text is discarded, so streams of any size are parsed
with bounded memory (plus whatever your actions build). Multi-byte UTF-8 characters split across chunks are handled.

### Push API (integrations and REPLs)

`parser.pushMachine()` returns a low-level machine fed with `machine.feed(chunk)` / `machine.endOfInput()` and driven
with `machine.run(budget)`; see its scaladoc for the protocol. The fs2 pipes and the engines are built on it.

## Syntax errors

`ParseError` carries the offset, line and column, what was found and which tokens were expected:

```
Unexpected token at 1:5: found "*", expected one of: "(", num
```

`ParseError.endOfInput` is `true` when the input ended while the parser still expected more: the input is a valid
prefix. A REPL can use it to ask for another line instead of reporting an error.

## Generated vs interpreted actions

`Grammar.grammar` generates the code of the actions, of every reduction (with its stack effects compiled in) and, for
`String` inputs, of the lexer (the token automaton becomes code, unless it is very large). `Grammar.interpreted` (same syntax) instead evaluates the grammar
block once at run time and calls the actions as function values: it is slower and exists as a fallback and as a
benchmark baseline. Because generated actions are moved out of the grammar block, they may use anything in scope
except the grammar's own symbols (non-terminals, terminals and the DSL): that is reported as a compile error. `.map`
functions and pure actions may be skipped when their value is unused, so they should be free of side effects.

## Current limitations

- The grammar block must be static: no conditionals or loops around declarations and productions, no helper methods.
- Terminal patterns must be literals (they are compiled at compile time).
