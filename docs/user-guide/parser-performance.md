# Parsers, flags and performance

## How a grammar is parsed

Every grammar is compiled into LALR(1) tables, which handle left recursion, operator precedence and most programming
language grammars. The **LALR(1) machine** that runs them keeps its stack on the heap (any nesting depth), can stop
and resume (effectful actions, streamed and pushed input, step budgets) and reports syntax errors. Its reductions and,
for `String` inputs, its lexer are generated code.

### The recursive-descent fast path

When a grammar is **LL(1)** - it can be parsed top down by looking at the next token only, which is true of most data
formats: JSON, configuration files, protocols - it also gets a generated **recursive-descent parser**, detected
automatically. It is the fast path for `String` inputs:

- every rule used at several places is a method returning its value, and the other rules are inlined: there is no
  value or state stack, and repetitions fill their collection's builder in a loop;
- choices are made on the next character where only one token of the grammar can start with it; punctuation and
  keywords are compared with the input instead of being lexed, and every other token with a unique first character
  has a scanner of its own;
- whitespace is skipped with a table lookup per char.

It does not report errors itself. On a syntax error, a value rejected by a collection's smart constructor, an exception
thrown by an action, or nesting deeper than 1000 levels, it gives up and the LALR(1) machine parses the input again,
reporting the error with its usual messages (or parsing the deep input on the heap). Both parsers run the same actions
in the same order and give the same values, but on those inputs the actions that ran before the problem run a second
time: keep actions free of side effects.

The fast path is used when:

- the grammar is LL(1), has no effectful actions and no precedence declarations (`left`/`right`/`nonassoc` settle
  choices only for the LALR(1) parser), and `RequireLALR` is not enabled;
- the input is a `String` (`Reader`/`InputStream`/pushed input use the machine);
- the engine runs the parse to completion (`Id`, `Option`, `Try`, `Either`, `Future`); the Cats Effect engine and the
  fs2 pipes run the machine with a step budget so that long parses yield to other fibers.

With `enable(RequireLL1)`, a grammar that is not LL(1) fails to compile with an explanation, and the machine that the
recursive-descent parser falls back to is a generated top-down one, whose error messages list only the tokens valid in
the current context (the LALR(1) machine can also list tokens valid in other contexts).

## Grammar flags

Compile-time options are set inside the grammar block, before the declarations:

```scala
Grammar.grammar[Json, Id] { g =>
  import g._
  enable(RequireLL1)
  // declarations and productions
}
```

| Flag | Effect |
|---|---|
| `RequireLL1` | the grammar must be LL(1) - otherwise compilation fails with an explanation of every problem (like `@tailrec` for tail calls) - and errors on `String` inputs are reported by a top-down machine |
| `RequireLALR` | use only the LALR(1) machine, without the recursive-descent fast path of LL(1) grammars |
| `ThrowingInRuntime` | allow collections and value types whose smart constructor can reject values in effects without an error channel (`Id`); the rejection is thrown as a `ParseError` |

`disable(flag)` states that a flag is off (flags are off by default); setting a flag both ways, or requiring both parsers,
is a compile error.

### Why a grammar is not LL(1)

With `enable(RequireLL1)`, a grammar that is not LL(1) fails with one error listing every reason, where it happens and
how to fix it, in plain words. For example:

```scala
expr ::= all(expr, "+", term).pure((a, _, b) => a + b) || all(term).pure(t => t)
```

```
`enable(RequireLL1)`: this grammar is not LL(1), i.e. a top-down parser that looks at one token ahead cannot parse it:
  1. `expr` can start with itself: `expr ::= expr "+" term` (Calc.scala:12:34). To read `expr`, a top-down parser
     would first have to read `expr` again, forever, before looking at any input (this is called left recursion).
     Describe the repetition with `rep`/`sepBy` instead - e.g. `list ::= all(item, rep(all(",", item)))` rather than
     `list ::= all(list, ",", item)` - or keep LALR (remove `enable(RequireLL1)`), which handles left recursion.
```

The reasons it reports:

- **a rule that starts with itself**, directly or through other rules (with the path, e.g. `a -> b -> a`): write the
  repetition with `rep`/`sepBy`;
- **alternatives that can start with the same token**: move the common beginning out, e.g.
  `all(x, "a") || all(x, "b")` becomes `all(x, "a" || "b")`;
- **an empty alternative** (`""`, `opt`, an empty `rep`) whose next token can also start another alternative;
- **an optional part, a repetition or a list separator** whose next token could also be what comes after it (e.g.
  `all(rep("x"), "x")`): the parser cannot tell whether it continues;
- **a repetition of something that can match nothing**.

`RequireLL1` is also a way to make sure a grammar keeps the fast path as it evolves.

## Writing fast grammars

- **Prefer LL(1) where the language allows it**: write precedence as one rule per level with repetitions for the
  operators instead of precedence declarations, and lists with `rep`/`sepBy` instead of left recursion (see below).
- **Convert tokens in place**: `.mapSlice` with `Numbers.int`/`long`/`double` for numbers, and slicing the delimiters
  off string literals, instead of `.map` on a copy of the text.
- **Keep effects out of hot rules.** Effectful actions disable the fast path; a pure grammar in `Either`/`Try` still
  reports syntax errors through the effect.
- **Declare primitive non-terminals** (`nonTerminal[Int]`, ...): their values stay unboxed between reductions.
- **Collect directly** into the collection you need with `.as[C]` instead of converting a `List` in the action.

### An LL(1) grammar for expressions

Precedence written as one rule per level, with repetitions for the operators (and `RequireLL1` to keep it LL(1)):

```scala
//> using dep com.kubuszok::kindlings-parser:{{ kindlings_version() }}

import hearth.kindlings.parser._

val calc: Parser[Id, Double] = Grammar.grammar[Double, Id] { g =>
  import g._
  enable(RequireLL1)
  val sum = nonTerminal[Double]
  val sumOp = nonTerminal[Double => Double]
  val product = nonTerminal[Double]
  val productOp = nonTerminal[Double => Double]
  val factor = nonTerminal[Double]
  val num = terminal("[0-9]+(\\.[0-9]+)?").mapSlice(Numbers.double)
  skip(" +")
  sum ::= all(product, rep(sumOp)).pure((a, ops) => ops.foldLeft(a)((acc, op) => op(acc)))
  sumOp ::= all("+", product).pure((_, b) => (a: Double) => a + b) ||
    all("-", product).pure((_, b) => (a: Double) => a - b)
  product ::= all(factor, rep(productOp)).pure((a, ops) => ops.foldLeft(a)((acc, op) => op(acc)))
  productOp ::= all("*", factor).pure((_, b) => (a: Double) => a * b) ||
    all("/", factor).pure((_, b) => (a: Double) => a / b)
  factor ::= all("(", sum, ")").pure((_, e, _) => e) || all(num).pure(n => n)
  sum
}

println(calc.parse("1 + 2 * (3 - 1) / 4"))
// expected output:
// 2.0
```

### Benchmarks

JSON (~1 MB, records with nested objects, arrays, strings, numbers), building the same AST, same session (JMH,
3 forks × 8 iterations; details in `docs/research/parser-vs-jawn.md` in the repository):

| Parser | Scala 3 ops/s | Scala 2.13 ops/s |
|---|---|---|
| jawn (hand-written, behind circe) | 224.5 ± 17.1 | 192.6 ± 8.2 |
| kindlings-parser, recursive descent, `mapSlice(Numbers.double)` | 199.0 ± 16.1 | 155.3 ± 8.5 |
| kindlings-parser, recursive descent, `.map(_.toDouble)` | 172.9 ± 17.7 | 142.8 ± 7.3 |
| kindlings-parser, LALR(1) machine (`RequireLALR`) | 108.5 ± 4.8 | 85.1 ± 3.8 |

On arithmetic (every value a `Double`), the LL(1) grammar above runs at 70.6 ± 6.1 ops/s, the precedence-based LALR(1)
grammar with the same number parsing at 63.5 ± 6.4, and fastparse at 20.0 ± 1.6 (Scala 3). Comparisons with
fastparse, parboiled2, cats-parse and Parsley are in `docs/research/parser-benchmarks.md`.
