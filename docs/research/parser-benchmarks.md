# kindlings-parser benchmarks

> **Benchmark**: `benchmarks/.../ParserBenchmark.scala` (`ParserJsonBenchmark`). The input is ~1 MB of JSON: 4000
> records with nested objects, arrays, numbers, strings, booleans and nulls. Each parser turns it into the same AST
> (`ParserModel.J`); `@Setup` checks that every parser produces an identical AST.
> **Configuration**: 2 forks, 4 warmup + 6 measurement iterations of 2 s; JDK 21; a shared 4-core cloud container, so
> absolute numbers are approximate and the ratios are what matters. Raw JMH JSON is in
> [benchmark-runs/2026-09-29-parser](benchmark-runs/2026-09-29-parser/).
> **Versions**: fastparse 3.1.1, parboiled2 2.5.1, cats-parse 1.1.0, Parsley 4.6.2, circe-parser (jawn) 0.14.16.

ops/s, higher is better:

| Parser | Scala 2.13 | Scala 3 |
|---|---|---|
| circe / jawn (hand-written JSON parser, reference) | 248 ± 17 | 280 ± 20 |
| jawn with a facade building the same AST (added later, separate run: see below) | 247 ± 31 | 250 ± 37 |
| **kindlings-parser, generated** (`Grammar.grammar`) | **92 ± 5** | **112 ± 5** |
| kindlings-parser, generated, input from a `Reader` (8 KB buffer) | 97 ± 8 | 98 ± 11 |
| kindlings-parser, interpreted (`Grammar.interpreted`) | 78 ± 5 | 98 ± 4 |
| parboiled2 (macro-generated PEG) | 90 ± 9 | 91 ± 8 |
| fastparse (inline macros) | 86 ± 7 | 88 ± 6 |
| cats-parse | 44 ± 4 | 40 ± 3 |
| Parsley 4 | 17 ± 1 | 16 ± 2 |

The jawn-with-facade row comes from a later session, in which kindlings-parser generated measured 84 ± 6 (2.13) and
94 ± 19 (3): the machine was slower that day, so compare it with that run's kindlings numbers (a ~2.6-2.9x gap), not with
the table above. [parser-vs-jawn.md](parser-vs-jawn.md) analyses where that gap comes from.

## Reading the results

- **Generated kindlings-parser grammars are the fastest of the grammar libraries measured**: on par with parboiled2
  and fastparse on 2.13, ~20-25% ahead of them on Scala 3. It is ~2x cats-parse and ~6x Parsley. Unlike parboiled2 and
  fastparse, the parse stack is on the heap (no `StackOverflowError` on deep nesting), and the same grammar also
  parses streams with bounded memory. The `Reader` variant copies chunks into a buffer and releases parsed text, yet is
  as fast as `String` input.
- **Hand-written jawn is still ~2.5x faster.** The gap is the price of a general LALR(1) machine: table-driven lexing,
  a value stack of boxed values, list builders for `sepBy`, and one substring per string/number token.
- **Generated vs interpreted actions**: +15-20%. After the lexer fast paths, action dispatch is not the main cost.
  The profile (JMH `-prof stack`, before the self-loop scan) split the time as lexer ~48%, reductions ~25%, the LR loop
  ~18%, `Double` parsing ~5%, and the generated actions themselves ~2%.
- The Parsley JSON parser is a straightforward grammar written for this benchmark, not a tuned one.

## Optimizations applied so far

1. Lexer specialised for `String` inputs: direct `charAt`, ASCII transition table in locals, no virtual `Input` calls.
   This took the generated JSON parser from 64 to 87 ops/s on 2.13.
2. Literal tokens push their constant instead of a substring of the input.
3. Runs of chars that keep the DFA in the same state (string bodies, whitespace, digits) are scanned in one tight loop.
4. Generated actions and `.map` chains are inlined into a `switch`, and unused values are not converted.

5. Generated reductions (static length/lhs/action, constant gotos) and a generated `String` lexer (the DFA as code,
   first-char `switch`, inlined self-loop scans): +42% (2.13) / +46% (3) on generated grammars in a same-session
   run, 84 → 119 and 94 → 138 ops/s, jawn at 235 / 269. See [parser-vs-jawn.md](parser-vs-jawn.md) § 5.

## Next candidates, by expected gain

See [parser-vs-jawn.md](parser-vs-jawn.md) § 4 for the measurements behind this order.

A fused driver loop (stacks and lookahead in locals, the lexer inlined into the loop) was tried and gave nothing: see
[parser-vs-jawn.md](parser-vs-jawn.md) § 5.

1. **Cheaper token text**: slices of a capture group, and lexer facts such as "no escapes" (~11%).
2. **Typed value slots** for primitive-valued symbols, to avoid boxing `Double`/`Int` on the value stack. (Repetitions
   already feed the target collection's own builder: see `.as[C]`.)
4. **Per-runtime generated drivers** (§5.11 of the research doc). These need an effect-heavy benchmark first (e.g.
   JSON with an `IO` action per element); the benchmark above is pure.
