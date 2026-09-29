# Parser (yacc-style grammars)

Write a grammar in BNF-like syntax directly in Scala, with typed semantic actions; a macro turns it into an
**LALR(1) parser at compile time**:

- grammar problems (shift/reduce and reduce/reduce conflicts, non-terminals without productions, terminals matching the
  empty string, unsupported regex features, ...) are **compile errors pointing at the offending production**;
- the lexer (a DFA built from all terminals) and the LR tables are computed during compilation and embedded in the
  generated code;
- parsing runs on an explicit, heap-allocated stack: **nesting depth is not limited by the JVM thread stack**;
- semantic actions can be pure or return the effect `F` of your choice (`Id`, `Option`, `Try`, `Either[E, *]`,
  `Future`, or any runtime providing a `ParserEngine[F]`).

It works on Scala 2.13 and Scala 3, on the JVM, Scala.js and Scala Native.

!!! warning "Early stage"

    This module is new. The grammar syntax is settled, but engines, input sources (currently `String` only) and
    code generation will evolve. See `docs/research/parser-library-research.md` in the repository for the design.

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
| `left(...)`, `right(...)`, `nonassoc(...)` | operator precedence levels, lowest first (yacc's `%left`, ...) |
| `all(...).prec(op)` | the production takes `op`'s precedence (yacc's `%prec`) |
| `skip("[ \\t\\n]+")` | text skipped between tokens (whitespace, comments) |

Lexing uses the longest match; when a literal and a regex match the same text (a keyword and an identifier), the literal
wins.

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

Other runtimes (cats-effect, ZIO, ...) plug in by providing a `ParserEngine[F]` from their own modules.

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

## Syntax errors

`ParseError` carries the offset, line and column, what was found and which tokens were expected:

```
Unexpected token at 1:5: found "*", expected one of: "(", num
```

## Current limitations

- Input is a `String` (streams and chunked input are planned).
- The grammar block must be static: no conditionals or loops around declarations and productions, no helper methods.
- Terminal patterns must be literals (they are compiled at compile time).
