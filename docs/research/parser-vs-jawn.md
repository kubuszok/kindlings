# Why jawn parses JSON ~2.8x faster than kindlings-parser

> **Question**: what makes jawn (the hand-written parser behind circe-parser) so much faster than a generated
> kindlings-parser grammar on the JSON benchmark, and which of those advantages a grammar-based parser can adopt.
> **Setup**: `ParserJsonBenchmark` (`benchmarks/.../ParserBenchmark.scala`), ~1 MB of JSON; JDK 21 on a shared
> 4-core container (absolute numbers drift between days, ratios are stable). Raw JMH JSON:
> [benchmark-runs/2026-09-29-parser/jawn-vs-kindlings-*.json](benchmark-runs/2026-09-29-parser/).

## 1. A like-for-like baseline first

The existing `circeJawn` benchmark builds circe's own `Json`, not our `ParserModel.J` AST, so it was not a fair
comparison. The new `jawn` benchmark (`JawnJson`) drives jawn's parser with a custom `Facade.NoIndexFacade[J]` that
builds exactly the AST every other parser builds (the `@Setup` check verifies it):

| Parser (same AST) | Scala 2.13 ops/s | Scala 3 ops/s |
|---|---|---|
| jawn + `J` facade | 247 ± 31 | 250 ± 37 |
| kindlings-parser, generated | 84 ± 6 | 94 ± 19 |

(2 forks × 6 iterations. An A/B run of the commit before the repetition-collection change gave 85.6 ± 4.3 on 2.13 in the
same session, so today's lower kindlings numbers compared with `parser-benchmarks.md` are the machine, not a regression.)

jawn stays ~2.6-2.9x ahead even building our AST, so the gap is in parsing, not in circe's `Json` representation.

## 2. Not allocation

`-prof gc` (Scala 3, per parse): jawn allocates **9.1 MB**, kindlings **11.9 MB** (circe-jawn with circe's `Json`:
9.6 MB). Much of that is shared: token strings, `Double`s, the AST and `List`s. 30% more garbage does not explain a
2.8x difference: the gap is CPU time.

## 3. Where the time goes (JFR execution sampling)

`-prof "jfr:configName=profile;debugNonSafePoints=true"`, `-f 1`, Scala 3; `jfr view hot-methods`. Percentages of
samples, converted to time per parse (kindlings ≈ 10.3 ms, jawn ≈ 3.5 ms per parse in that run).

The input has **192 000 tokens** (60 000 strings with 699 000 chars, 16 000 numbers). The JSON grammar makes ≈ 0.9
reductions per token (per record: 13 `value` reductions, 11 `member`s, 15 `sepBy` appends, 3 `sepBy` passes for 48
tokens). kindlings spends **≈ 54 ns per token**, jawn **≈ 19 ns**.

| Bucket | kindlings (share → ms) | jawn (share → ms) |
|---|---|---|
| Scanning characters into tokens | `lexString` 29.1%, `String.charAt`/`coder`/`length`/`checkIndex` 5.9%, `Tables.*` accessors 2.8%, `lex` 0.9% → **~39% ≈ 4.0 ms** | `parseStringSimple` 8.5%, `parseNum` 5.2%, `charAt`/`checkIndex`/`at` 12.6% → **~26% ≈ 0.9 ms** |
| Parser control (what to do with a token) | `Machine.run` 14.3%, `reduce` 8.8%, `push` 8.4%, `GeneratedGrammar.userAction` 2.0%, `prodKind` 0.5% → **~34% ≈ 3.5 ms** | `rparse` 6.3% → **~6% ≈ 0.2 ms** |
| Token text copies and rescans | `copyOfRangeByte` 4.7%, `StringLatin1.indexOf` 3.9%, `String.<init>`/`newString`/`substring` 2.6% → **~11% ≈ 1.1 ms** | `newString` 2.4%, `copyOfRangeByte` 2.0% → **~4% ≈ 0.15 ms** |
| Actions / facade, lists | generated `action` 5.5%, `ListBuffer` 3.5% → **~9% ≈ 0.9 ms** | facade `add`, `List`/`::`, `releaseFence`, boxing → **~40% ≈ 1.4 ms** (attribution of inlined AST building is fuzzy) |
| `toDouble` | `trim` + `FloatingDecimal` ≈ 4% → 0.4 ms | ≈ 18% → 0.6 ms |

Sampling attributes inlined callees to their callers, so each bucket is approximate (±5 points). The picture is
nevertheless unambiguous: **the value-building work is about the same in both (~2-2.5 ms), and the whole gap is the
parsing machinery**:

1. **Parser control, ~3.3 ms of the ~6.8 ms gap.** jawn's "parser" is a `switch` on the first character of a value
   inside a loop with an explicit context stack: one branch per token, no tables. kindlings interprets LALR tables:
   per shift it reads `action(state * tokenCount + lookahead)` and pushes a state and a boxed value (with a growth
   check). Per reduction (≈ 170 000 of them) it reads `prodLen`, `prodLhs`, `prodKind`, dispatches through
   `CompiledGrammar.userAction` → `GeneratedReductions.action` (a `switch` over all productions), nulls the popped value
   slots, and reads `goto(state * nonTerminalCount + lhs)`. That is ≈ 10 ns per LR step, spent on the equivalent of
   jawn's single branch.
2. **Scanning, ~3.1 ms.** Both read the `String` with `charAt`, but jawn uses a hand-written loop per token kind:
   `parseStringSimple` loops until `"` or `\` and nothing else; `parseNum` walks the digits with branches; whitespace
   is skipped inline before dispatch. kindlings runs a generic longest-match DFA: an `ascii(state * 128 + c)` lookup
   per char (the self-loop scan still does `selfLoop(row + c)` per char), an accept-table check per transition, and a
   restart of the whole token loop for every whitespace run (whitespace is a skipped token). It also writes the `Long`
   `pos`/`tokenStart`/`tokenEnd` fields per token.
3. **Token text, ~1 ms.** Our JSON grammar copies each string twice: `input.slice` for the token, then
   `.substring(1, n - 1)` in `.map` to drop the quotes. `unescape` then rescans it with `indexOf('\\')`. jawn copies once
   and already knows while scanning whether an escape occurred (`parseStringSimple` falls back to `parseStringComplex`
   only on `\`).

## 4. What a grammar-based parser can take from this

Ranked by the time at stake. Everything below keeps the design: grammars stay declarative, LALR analysis and
diagnostics are unchanged, and the stack stays on the heap.

1. **A generated driver instead of table interpretation (targets ~3.3 ms).** Generate per-grammar code for the LR loop
   (the "direct-coded" LR of §5.3 of the research doc; bison/lemon-style table interpretation is what we do now):
   - reduction cases that know their length, left-hand side and kind statically: no `prodLen`/`prodLhs`/`prodKind`
     reads, no `userAction` → `action` double dispatch, no per-slot nulling (only the slot that is overwritten matters;
     stale slots above `sp` die with the machine);
   - **default reductions**: in states whose only action is one reduction (most `value ::= ...` states in JSON),
     reduce without consulting the lookahead;
   - **unit-reduction chains** folded at compile time (a reduction immediately followed by a pass/goto that is always
     the same).
   A state-machine `switch` per state (or per state group) replaces two table reads and a multiply per step.
2. **A generated lexer (targets ~3 ms).** Compile the DFA to code with a `switch` on the first char, like jawn:
   - self-loop states become tight `while` loops over `charAt` with the loop condition inlined (`c != '"' && c != '\\'`)
     instead of `selfLoop` table reads;
   - skipped tokens (whitespace) are consumed inline before the token switch, without restarting the token loop;
   - accept checks happen only in states that can accept, and `Int` positions are used for `String` inputs.
3. **Cheaper token text (targets ~1 ms).** Let terminals slice a capture instead of the whole match (e.g.
   `terminal("\"(...)\"").group(1)` or a `.mapSlice((text, start, end) => ...)` converter), and let the lexer report
   facts it already knows ("no `\` in this string") so `unescape` does not rescan.
4. **Lower in value**: typed value slots for primitives (a `Double` per number is boxed today) and a cheaper builder
   for small `List`s. Both are small next to the three above.

Expected outcome: items 1 + 2 alone remove most of the ~6.4 ms of machinery overhead. A generated parser at ~4-5 ms per
parse would be within ~1.3-1.5x of jawn, the remaining gap being the generality of LALR (a state stack, the lookahead
token) and the per-token value stack.

## 5. Implemented: generated reductions and a generated lexer

Items 1 and 2 are implemented (`CodegenPlan` builds compiler-independent plans, the per-Scala-version bridges emit
them):

- **Generated reductions.** Every production, built-ins included (pass, constants, `opt`, collection building), gets a
  `case` in a generated `reduce(p, machine)`. The length, left-hand side and action are compiled in. When the left-hand
  side's goto column has a single target (true of most productions), the target state is a constant (`reducedTo`), and
  popped slots are no longer nulled. The per-production tables (`prodLen`/`prodLhs`/`prodKind`) and the
  `userAction` → `action` double dispatch are gone from the generated path. On Scala 2, action and converter lambdas
  are now inlined too (arguments bound to fresh vals, unused parameters dropped) instead of called through
  `FunctionN.apply`, matching Scala 3's `betaReduce`.
- **Generated `String` lexer.** The DFA becomes code. States with a single predecessor are inlined into it, so only
  the start state and join states are `case`s of a `state` loop. The start state is a `match` on the first char (a
  `tableswitch`, like jawn's value dispatch). Self-loops (string bodies, digits, whitespace) are `while` loops with the
  membership test inlined, using the set or its complement (`c != '"' && c != '\'`). Accepting is a constant, and a
  state without transitions (`{`, a closing quote) finishes the token without reading another char. Skipped tokens
  loop back without leaving the method, and positions are `Int`s. DFAs over 256 states or ~6000 bytes of estimated
  bytecode keep the table lexer, because HotSpot does not JIT-compile methods over 8000 bytes. `Reader`/push inputs
  keep the table lexer as well, and `LexerSpec` checks that both lexers agree, including on errors and on 500
  generated inputs.

Results (2 forks × 6 iterations, same session for all rows):

| Parser | Scala 2.13 ops/s | Scala 3 ops/s |
|---|---|---|
| jawn + `J` facade | 235 ± 19 | 269 ± 20 |
| **kindlings-parser, generated** | **119 ± 11** (was 84 ± 6) | **138 ± 7** (was 94 ± 19) |
| kindlings-parser, generated, `Reader` input (table lexer) | 97 ± 7 | 102 ± 8 |
| kindlings-parser, interpreted | 70 ± 7 | 94 ± 12 |

That is +42% / +46%, and the gap to jawn went from ~2.8x to ~2x. A lexer-only measurement before the change (a
temporary token-counting benchmark) put table lexing at 3.9 ms per parse, as long as jawn's whole parse. JFR after
the change: the generated lexer is ~30% of the time (~2 ms, down from ~4), the LR loop (`Machine.run`, `push`,
`reduce`) ~38%, and token text plus value building (the grammar's second `substring`, `unescape`'s rescan, `toDouble`,
list builders) ~25%.

### Tried and reverted: a fused driver loop

Commit "experiment: fused generated String driver ..." (reverted by the next commit, kept in history) generated
`runString(machine, budget)`: the whole parse loop over locals (stacks, stack pointer, lookahead, token positions) with
the lexer inlined, writing the state back only when the machine stops (done, error, effect, budget), and a
`reduce(p, states, values, top, goto, m)` working on the arrays directly. It passed every test, but:

| Same session | Scala 2.13 ops/s | Scala 3 ops/s |
|---|---|---|
| generated reductions + generated lexer (kept) | 119 ± 11 | 138 ± 7 |
| + fused driver loop | 119.5 ± 7 | 137.7 ± 14 |
| + fused loop + first-char `match` widened to all ASCII (a `tableswitch`) | 100 ± 12 | 133 ± 10 (jawn 242, was 268) |

So once reductions and lexing are generated, **the loop is no longer bound by field traffic or dispatch**. Caching
the action table in a field did nothing either (138 → 140). JFR by bytecode index (every generated instruction maps
to one source line, so `jfr print --json` frames' `bytecodeIndex` against `javap -c` of the generated class) spreads
the loop's samples over the action-table read, the lexer's per-token entry and the string-body loop, with no single
hot spot. Bigger generated methods only add risk: 2.13 got slower with the widened dispatch. The one change kept from
the experiment is numbering the lexer's join states densely, so the per-token `state` match is a `tableswitch`.

What remains is the data, not the control:

1. **Cheaper token text** (item 3 above). Token copies (`String.<init>`, `copyOfRange`, `substring`) are ~12% of the
   samples, the grammar's `unescape` rescan (`indexOf`) ~7%. A terminal that yields a slice of its match (e.g. the
   string without its quotes), and a lexer that reports whether it saw an escape, would remove one copy and the
   rescan: ~1 ms of the ~7 ms per parse.
2. **Value building** (`toDouble`, list builders, tuples) is the same work jawn's facade does and stays.

## 5. How to reproduce

```bash
sbt --client 'benchmarks/Jmh/run -f 2 -wi 4 -i 6 -r 2 -w 2 .*ParserJsonBenchmark.(jawn|kindlingsGenerated)$'
sbt --client 'benchmarks/Jmh/run -f 1 -wi 4 -i 5 -r 2 -w 2 -prof gc .*ParserJsonBenchmark.(kindlingsGenerated|jawn|circeJawn)$'
sbt --client 'benchmarks/Jmh/run -f 1 -wi 4 -i 5 -r 2 -w 2 -prof "jfr:dir=/tmp/jfr;configName=profile;debugNonSafePoints=true;stackDepth=64" .*ParserJsonBenchmark.(kindlingsGenerated|jawn)$'
jfr view --width 200 hot-methods /tmp/jfr/*kindlingsGenerated*/profile.jfr
```
