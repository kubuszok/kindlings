# Parser (yacc-style grammars)

Write a grammar in BNF-like syntax directly in Scala, with typed semantic actions, and a macro turns it into a parser
**at compile time**:

- grammar problems (shift/reduce and reduce/reduce conflicts, non-terminals without productions, terminals matching the
  empty string, unsupported regex features, ...) are **compile errors pointing at the offending production**;
- the lexer (a DFA built from all terminals) and the LALR(1) tables are computed during compilation; your actions,
  token conversions and collection building are **generated code**, not function values called at run time;
- grammars that are **LL(1)** - most data formats - are detected and also get a generated **recursive-descent parser**,
  which parses JSON at ~90% of the speed of jawn, the hand-written parser behind circe (on Scala 3);
- parsing never overflows the JVM stack: the LALR(1) machine keeps its stack on the heap and handles any nesting depth;
- actions can be pure or return the effect of your choice: `Id`, `Option`, `Try`, `Either[E, *]`, `Future`, any Cats
  Effect `F`, fs2 streams, or any runtime providing a `ParserEngine[F]`;
- repetitions are collected into any collection (standard, Java, cats `NonEmptyList`/`Chain`, ...) and value type
  (`AnyVal` wrappers, refined and Iron types) that Hearth's standard extensions understand.

It works on Scala 2.13 and Scala 3, on the JVM, Scala.js and Scala Native.

!!! warning "Early stage"

    This module is new. The grammar syntax is settled, but engines, input sources and code generation will
    evolve. See `docs/research/parser-library-research.md` in the repository for the design.

The documentation is split into:

- this page: installation and a first look;
- [Writing grammars](parser-grammars.md): the grammar syntax, tokens and their conversions, actions, repetitions and
  collections, precedence, diagnostics;
- [Running parsers](parser-running.md): effects and engines (including Cats Effect), inputs (strings, readers, streams,
  pushed chunks) and syntax errors;
- [Parsers, flags and performance](parser-performance.md): which parser runs when, the grammar flags, what makes a
  grammar LL(1), and how to write fast grammars.

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser" % "{{ kindlings_version() }}"
    // optional:
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-cats-effect" % "{{ kindlings_version() }}" // Parser[IO, R] etc.
    libraryDependencies += "com.kubuszok" %%% "kindlings-parser-fs2" % "{{ kindlings_version() }}"         // fs2 pipes
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}
    ```

## Quick start

A calculator: a left-recursive grammar with operator precedence, evaluated while it is parsed.

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

`Grammar.grammar[Result, F] { g => import g._; ... }` defines the grammar: terminals and non-terminals are declared
first, productions (`nt ::= alternatives`) come after them, and the block's last expression is the start symbol.
`Result` is the type produced by the start symbol and `F` is the effect of actions and of the whole parse:
`Parser[F, Result].parse(input): F[Result]`. With `F = Id` the result is the value itself and syntax errors are thrown.

A JSON-like data format, whose grammar is LL(1), so it is parsed by the generated recursive-descent parser:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

sealed trait Value
final case class Num(value: Double) extends Value
final case class Str(value: String) extends Value
final case class Arr(items: Vector[Value]) extends Value
final case class Obj(fields: Map[String, Value]) extends Value

type Result[A] = Either[String, A]

val values: Parser[Result, Value] = Grammar.grammar[Value, Result] { g =>
  import g._
  val value = nonTerminal[Value]
  val field = nonTerminal[(String, Value)]
  val string = terminal("\"[^\"]*\"").mapSlice((input, start, end) => input.substring(start + 1, end - 1))
  val number = terminal("-?[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double)
  skip("[ \\t\\r\\n]+")
  value ::= (
    all(number).pure(n => Num(n): Value) ||
      all(string).pure(s => Str(s): Value) ||
      all("[", sepBy(value, ",").as[Vector[Value]], "]").pure((_, items, _) => Arr(items): Value) ||
      all("{", sepBy(field, ","), "}").pure((_, fields, _) => Obj(fields.toMap): Value)
  )
  field ::= all(string, ":", value).pure((k, _, v) => k -> v)
  value
}

println(values.parse("""{"xs": [1, 2.5, "three"]}"""))
println(values.parse("""{"xs": [1, 2.5,]}"""))
// expected output:
// Right(Obj(Map(xs -> Arr(Vector(Num(1.0), Num(2.5), Str(three))))))
// Left(Unexpected token at 1:16: found "]", expected one of: "[", "{", number, string)
```
