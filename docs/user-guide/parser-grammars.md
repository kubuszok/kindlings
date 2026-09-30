# Writing grammars

A grammar block declares terminals and non-terminals first and productions after them (the compiler enforces that
order); its last expression is the start symbol:

```scala
Grammar.grammar[Result, F] { g =>
  import g._
  // declarations: val x = nonTerminal[A], val t = terminal("regex"), skip("regex"), precedence, flags
  // productions:  x ::= alternative || alternative || ...
  startSymbol
}
```

## Syntax

| Construct | Meaning |
|---|---|
| `val expr = nonTerminal[A]` | a non-terminal producing `A` |
| `val num = terminal("[0-9]+")` | a terminal (token) defined by a regular expression; its value is the matched `String` |
| `.map(f)` on a terminal | converts the matched text, e.g. `terminal("[0-9]+").map(_.toInt)` |
| `.mapSlice((input, start, end) => ...)` on a terminal | converts the token from the input and its bounds, without copying it first |
| `"+"` inline | a literal terminal; its value is the literal itself |
| `"[a-z]+".r` inline | an inline regex terminal |
| `skip("[ \\t\\n]+")` | text skipped between tokens (whitespace, comments); can be used several times |
| `nt ::= alternatives` | adds productions to `nt` (can be used several times) |
| `all(s1, ..., sN).pure { (v1, ..., vN) => R }` | an alternative with a **pure** action |
| `all(s1, ..., sN) { (v1, ..., vN) => F[R] }` | an alternative with an **effectful** action |
| `a \|\| b` | combines alternatives |
| a single symbol or literal as an alternative | passes its value through (`suffix ::= "Sr." \|\| "Jr." \|\| ""`) |
| `""` as a whole alternative | the empty alternative (its value is `""`) |
| `"+" \|\| "-"` used as a symbol | an inline group (an anonymous helper non-terminal) |
| `opt(s)` | `Option[A]` |
| `rep(s)`, `rep1(s)` | zero or more, one or more `s`: a `List[A]` |
| `sepBy(s, sep)`, `sepBy1(s, sep)` | zero or more, one or more `s` separated by `sep`: a `List[A]` |
| `.as[C]` on `rep`/`rep1`/`sepBy`/`sepBy1` | the repetition collected into `C` instead of a `List` |
| `left(...)`, `right(...)`, `nonassoc(...)` | operator precedence levels, lowest first (yacc's `%left`, ...) |
| `all(...).prec(op)` | the production takes `op`'s precedence (yacc's `%prec`) |
| `enable(flag)`, `disable(flag)` | compile-time options, see [Parsers, flags and performance](parser-performance.md) |

Sequences are limited to 22 symbols (a Scala 2 function arity limit); split longer ones into helper non-terminals.

## Tokens

Terminals are regular expressions in a DFA-compatible subset of Java regex syntax: literals and escapes, `.`,
character classes (`[a-z]`, `[^"\\]`, `\d`, `\w`, `\s`), groups, alternatives and the `?`, `*`, `+` and `{n,m}`
quantifiers. Anchors, back-references, look-arounds, lazy quantifiers and flags cannot be compiled into a lexer and are
reported as compile errors, as are patterns that match the empty string.

All terminals of a grammar are compiled into one lexer, which reads the **longest match**. When a literal and a regex
match the same text, the literal wins, so keywords can overlap identifiers:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val words: Parser[Id, List[String]] = Grammar.grammar[List[String], Id] { g =>
  import g._
  val words = nonTerminal[List[String]]
  val word = nonTerminal[String]
  val ident = terminal("[a-z]+")
  skip(" +")
  skip("#[^\\n]*") // comments
  words ::= all(rep(word)).pure(ws => ws)
  word ::= all("if").pure(_ => "KEYWORD") || all(ident).pure(id => s"id:$id")
  words
}

println(words.parse("if iffy # a comment"))
// expected output:
// List(KEYWORD, id:iffy)
```

### Converting tokens: `map`, `mapSlice` and `Numbers`

A terminal's value is a copy of the matched text; `.map` converts it (several `.map`s chain). When the conversion copies
again (dropping the quotes of a string literal) or scans the text (looking for escapes), `.mapSlice` saves the first
copy: it gets a text containing the token and the token's bounds in it (`input.substring(start, end)` is the token).
With a `String` input that text is the whole input, so the function must stay within `[start, end)`.
`Slices.indexOf` looks for a char within bounds (`String.indexOf` would scan the rest of the input), and `Numbers.int`,
`Numbers.long` and `Numbers.double` parse numbers in place, without creating a `String`:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

def unquote(input: String, start: Int, end: Int): String =
  if (Slices.indexOf(input, '\\', start + 1, end - 1) < 0) input.substring(start + 1, end - 1)
  else StringContext.processEscapes(input.substring(start + 1, end - 1))

val pairs: Parser[Id, List[(String, Double)]] = Grammar.grammar[List[(String, Double)], Id] { g =>
  import g._
  val pairs = nonTerminal[List[(String, Double)]]
  val pair = nonTerminal[(String, Double)]
  val string = terminal("\"([^\"\\\\]|\\\\.)*\"").mapSlice(unquote).map(_.toUpperCase)
  val number = terminal("-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?").mapSlice(Numbers.double)
  skip("[ \\n]+")
  pairs ::= all(sepBy(pair, ",")).pure(ps => ps)
  pair ::= all(string, "=", number).pure((k, _, v) => k -> v)
  pairs
}

println(pairs.parse("\"pi\" = 3.14159, \"tab\\tbed\" = -1e3"))
// expected output:
// List((PI,3.14159), (TAB	BED,-1000.0))
```

`mapSlice` must be the first conversion of a `terminal(...)`. It runs when the token is read, even if its value ends up
unused. The terminal's pattern may not be used by another terminal declaration (a compile error), because the
conversion is attached to the token itself.

## Actions

`all(...).pure { ... }` computes the alternative's value from its symbols' values; `all(...) { ... }` returns it in the
grammar's effect `F` (see [Running parsers](parser-running.md)). Parameters an action ignores (`_`) are never
converted: their `.map`s do not run, and literal values are never even copied.

Actions are moved out of the grammar block into generated code, so they may use anything in scope except the grammar's
own symbols (non-terminals, terminals and the DSL), which is reported as a compile error. `.map` functions and pure
actions may be skipped when their value is unused and may run twice when the fast path hands an invalid input over to
the LALR(1) parser (see [Parsers, flags and performance](parser-performance.md#the-recursive-descent-fast-path)), so
they should be free of side effects.

Non-terminals declared with a primitive type (`nonTerminal[Int]`, `nonTerminal[Double]`, ...) keep their values
unboxed between reductions.

## Optional and repeated symbols

`opt`, `rep`, `rep1`, `sepBy` and `sepBy1` take any symbol: a terminal, a non-terminal, a literal or an inline group.

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val calls: Parser[Id, List[String]] = Grammar.grammar[List[String], Id] { g =>
  import g._
  val calls = nonTerminal[List[String]]
  val call = nonTerminal[String]
  val ident = terminal("[a-z]+")
  skip(" +")
  calls ::= all(rep1(call)).pure(cs => cs)
  call ::= all(opt("await"), ident, "(", sepBy(ident, ","), ")").pure { (await, f, _, args, _) =>
    await.fold("")(_ => "await ") + f + args.mkString("[", ", ", "]")
  }
  calls
}

println(calls.parse("f(a, b) await g() h(x)"))
// expected output:
// List(f[a, b], await g[], h[x])
```

### Choosing the collection

Repetitions produce a `List` by default. `.as[C]` collects them into any type that Hearth's standard extensions
understand:

- **collections** (`IsCollection`): Scala collections (`Vector`, `Set`, `ArraySeq`, `ArrayBuffer`, ...), arrays, Java
  collections (JVM only), and collections from providers on the classpath, e.g. cats `NonEmptyList`, `NonEmptyVector`,
  `NonEmptyChain` or `Chain` with [kindlings-cats-integration](cats-integration.md);
- **value types wrapping a collection** (`IsValueType`): `AnyVal` wrappers, and refined or Iron types with
  [kindlings-refined-integration](refined-integration.md) or [kindlings-iron-integration](iron-integration.md).

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._
import scala.collection.immutable.ArraySeq

final case class Tags(values: Set[String]) extends AnyVal

val numbers: Parser[Id, ArraySeq[Int]] = Grammar.grammar[ArraySeq[Int], Id] { g =>
  import g._
  val list = nonTerminal[ArraySeq[Int]]
  skip(" +")
  list ::= all("[", sepBy(terminal("[0-9]+").map(_.toInt), ",").as[ArraySeq[Int]], "]").pure((_, ns, _) => ns)
  list
}
val tags: Parser[Id, Tags] = Grammar.grammar[Tags, Id] { g =>
  import g._
  val tags = nonTerminal[Tags]
  skip(" +")
  tags ::= all(rep(terminal("#[a-z]+")).as[Tags]).pure(t => t)
  tags
}

println(numbers.parse("[1, 2, 3]"))
println(tags.parse("#scala #parser #scala"))
// expected output:
// ArraySeq(1, 2, 3)
// Tags(Set(#scala, #parser))
```

The macro checks that the type is supported and that the repeated values fit its element type; maps (and value types
wrapping them) are rejected with an explanation: collect pairs and convert them in the action. The generated code
creates the collection's own `Builder` when the repetition starts, appends each value as it is parsed and calls
`result()` once, so no intermediate `List` is built. The builder's `Factory` is evaluated once per parser. `.as[C]`
requires `Grammar.grammar` (`Grammar.interpreted` always builds `List`s).

A collection or value type with a smart constructor (cats `NonEmptyList`, `List[Int] Refined NonEmpty`,
`List[Int] :| MinLength[2]`) can reject the values. The rejection is reported as a `ParseError`
(`Invalid cats.data.NonEmptyList[scala.Int]: ...`) through the effect's error channel, so such types compile only in
an `F` that has an `ErrorChannel[F]`: `Option`, `Try`, `Either[E, *]` (with a `ParseErrorLift[E]`), `Future`, and Cats
Effect `F`s with `import hearth.kindlings.parser.catseffect._`. In `Id` they are a compile error, unless you explicitly
opt into throwing the rejection as a `ParseError` with `enable(ThrowingInRuntime)`:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}
//> using dep com.kubuszok::kindlings-cats-integration:{{ kindlings_version() }}

import cats.data.NonEmptyList
import hearth.kindlings.parser._

type Result[A] = Either[String, A]

val numbers: Parser[Result, NonEmptyList[Int]] = Grammar.grammar[NonEmptyList[Int], Result] { g =>
  import g._
  val list = nonTerminal[NonEmptyList[Int]]
  skip(" +")
  list ::= all(rep(terminal("[0-9]+").map(_.toInt)).as[NonEmptyList[Int]]).pure(ns => ns)
  list
}

println(numbers.parse("1 2 3"))
println(numbers.parse("").isLeft)
// expected output:
// Right(NonEmptyList(1, 2, 3))
// true
```

Prefer `rep1`/`sepBy1` for non-empty collections: the grammar then guarantees at least one value.

## Precedence

`left`, `right` and `nonassoc` declare the precedence and associativity of operators, lowest first, as yacc's
`%left`/`%right`/`%nonassoc`. A production takes the precedence of its last terminal that has one, or of the operator
given with `.prec(op)` (yacc's `%prec`), e.g. for a unary minus that binds tighter than binary operators:

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val calc: Parser[Id, Int] = Grammar.grammar[Int, Id] { g =>
  import g._
  val expr = nonTerminal[Int]
  val num = terminal("[0-9]+").map(_.toInt)
  skip(" +")
  left("-")
  right("^")
  nonassoc("NEG")
  expr ::= (
    all(expr, "-", expr).pure((a, _, b) => a - b) ||
      all(expr, "^", expr).pure((a, _, b) => math.pow(a.toDouble, b.toDouble).toInt) ||
      all("-", expr).prec("NEG").pure((_, e) => -e) ||
      all(num).pure(n => n)
  )
  expr
}

println(calc.parse("10 - 2 - 3"))
println(calc.parse("2 ^ 3 ^ 2"))
println(calc.parse("-2 ^ 2"))
// expected output:
// 5
// 512
// 4
```

Precedence only settles choices for the LALR(1) parser: grammars with precedence declarations do not get the
recursive-descent fast path. An LL(1) grammar spells precedence out in its rules, one non-terminal per level, with
repetitions for the operators (see [Parsers, flags and performance](parser-performance.md#writing-fast-grammars)).

## Writing alternatives on several lines

Scala 2.13 does not continue an expression on a line starting with `||`, so put several alternatives in parentheses and
end each line with `||` (this layout works on both Scala 2.13 and 3):

```scala
expr ::= (
  all(expr, "+", expr).pure((a, _, b) => a + b) ||
    all(num).pure(n => n)
)
```

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
non-terminals (warning), terminals matching the empty string, invalid or unsupported regular expressions, non-literal
patterns, a `mapSlice` pattern shared with another terminal, unsupported `.as[C]` types, collections with smart
constructors in effects without an error channel, and conflicting flags. With `enable(RequireLL1)`, a grammar that is
not LL(1) fails with a plain-language explanation of why (see
[Parsers, flags and performance](parser-performance.md#why-a-grammar-is-not-ll1)).

## Generated vs interpreted grammars

`Grammar.grammar` generates the code of the actions, of every reduction (with its stack effects compiled in), of the
recursive-descent parser of LL(1) grammars and, for `String` inputs, of the lexer (the token automaton becomes code,
unless it is very large). `Grammar.interpreted` (same syntax, without `.as[C]`) instead evaluates the grammar block
once at run time and calls the actions as function values: it is slower and exists as a fallback and as a benchmark
baseline.

## Current limitations

- The grammar block must be static: no conditionals or loops around declarations and productions, no helper methods.
- Terminal patterns must be literals (they are compiled at compile time).
- Grammars cannot be composed yet: a grammar is one block.
