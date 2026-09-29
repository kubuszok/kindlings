# Parser library for Kindlings: prior-art research

Status: **RESEARCH** (2026-09-29; revised the same day for composition (R6), the input memory model (R7) and the yacc-style BNF front end (§5.8)). No implementation yet. This document compares the existing Scala
parsing libraries and the relevant non-Scala prior art against the requirements below, and proposes a
direction plus the decisions that need to be made before a prototype.

How the facts were gathered:

- **Scala libraries:** read from source (shallow clones of each repository), with versions taken from
  git tags and Maven metadata.
- **Algorithms and papers:** checked against the papers, manuals and repositories cited in
  [§9 Sources](#9-sources).

Nothing was benchmarked or run. The stack-safety claims come from reading the code. Unverified claims
are marked **[unverified]**.

## Implementation status (milestone 1, 2026-09-29)

The `parser` module (`kindlings-parser`) implements the yacc-style front end of §5.8 on Scala 2.13 and 3, JVM, Scala.js
and Scala Native (all tests green on all six). User guide: `docs/user-guide/parser.md`.

**Implemented**
- The `grammar[Result, F] { g => import g._; ... }` DSL with in-block `nonTerminal`/`terminal` declarations,
  `all(...) { ... }` (effectful) and `.pure { ... }`, `||`, inline literals (singleton values, `""` as the empty
  alternative) and inline `"...".r` regexes, `.map` on terminals, inline groups, `opt`/`rep`/`rep1`/`sepBy`/`sepBy1`,
  `left`/`right`/`nonassoc` + `.prec`, `skip`.
- A macro that extracts the grammar (raw compiler trees per Scala version, see
  `docs/research/hearth-gap-destructured-local-vals.md`), then shared, compiler-independent analysis:
  - a regex subset parser and a lexer DFA with longest match and literal-over-regex priority;
  - LALR(1) tables with yacc-style precedence;
  - diagnostics reported at the offending production: shift/reduce and reduce/reduce conflicts with the LR state,
    non-terminals without productions, unproductive and unreachable non-terminals, empty-matching and unsupported
    patterns, non-literal patterns.
- A resumable, table-driven LR machine on heap arrays (100k-deep nesting is tested) and engines for `Id`, `Option`,
  `Try`, `Either[E, *]` (with `ParseErrorLift`) and `Future`. Other runtimes plug in with an implicit `ParserEngine[F]`.

**Deliberately simplified in milestone 1, compared with the design above**
- The macro emits the encoded tables plus the user's own lambda. At run time, the block is evaluated once against a
  recording `Dsl` to collect the action functions, with a structural fingerprint check against the compiled grammar.
  Actions are therefore called through `FunctionN` values, not spliced into reduce cases (§5.8, step 4).
- The engine is table-driven. There are no direct-coded states yet, no per-runtime codegen providers (§5.11); engines
  are run-time `ParserEngine[F]` instances.
- Dead-value elimination (§5.8, round 4), tracing (§5.4), error recovery, helper methods in grammar blocks, push-style
  inputs, and cross-grammar composition (§5.6) are not implemented yet.

**Milestone 2 (same day)**
- Inputs: `String` read in place, `Reader`/`InputStream` read in chunks with already-parsed text discarded on refill
  (§5.7's "until committed" strategy: token values are copied on shift), `Long` positions.
- The machine never performs I/O: `NeedInput` makes the engine refill, and `run(budget)` returns `Yield`.
- Engines receive a machine factory, so lazy effects parse afresh on every run.
- `ParseError.endOfInput` for REPL incomplete-input detection.
- `kindlings-parser-cats-effect`: an `Async`/`Sync` engine (one fused `flatMap` per effectful action, `cede` every
  budget, `blocking` reads).
- Push input (`Parser.pushMachine`: `feed`/`endOfInput`) and `kindlings-parser-fs2`: `pipe`/`bytePipe` (`Pull`-based,
  one `Pull.eval` per effectful action, fresh machine per stream run, bounded buffers).

## 0. Requirements (as stated) and how they are read here

| # | Requirement | What it implies technically |
|---|---|---|
| R1 | User-chosen effect `F[_]`, with a type class `Something[F]` that the parser uses to produce `F[Program]`. `F` can be hard-coded or passed in. | Semantic actions are abstracted over `F`. The macro must specialise when `F` is known (e.g. `Id`), because a per-token `flatMap` is far too slow for R2. |
| R2 | Minimal overhead, so a 4 GB JSON is parseable. Stack safe, using a mutable heap stack rather than the JVM thread stack. | `Long` offsets, streaming input with a bounded buffer, and no unbounded backtracking. An explicit `Array`-backed state/value stack. No allocation per token on the hot path. A mode that does not build a full AST, since a 4 GB AST does not fit in a normal heap. |
| R3 | Both compilers and REPLs; input from `String` and `InputStream`. | A push/resumable parser (feed chunk → `NeedInput` / `Done` / `Error`). Exact "incomplete input" detection. Error recovery is desirable. |
| R4 | Built-in debugging: runtime tracing, plus compile-time detection of unintended loops, ambiguities and shift/reduce conflicts. | The **whole grammar must be visible to a macro as data**. Nullable/FIRST/FOLLOW analysis, left-recursion and nullable-loop checks, LL or LR conflict reporting with counterexamples, and PEG-shadowing checks. The tracer is compiled in only when requested. |
| R5 | Easy to write algol-like, indentation-based, Markdown/markup and data-language grammars. | Precedence/Pratt support, a lexer layer, a layout (INDENT/DEDENT) scanner hook, and line-oriented/stateful escape hatches for CommonMark-class languages. |
| R6 | Parsers can be **combined** (separate vals, files, libraries, parametrised rules) **without losing** compile-time and runtime debugging; possibly via phantom types. | Every rule must carry a machine-readable summary of itself that survives separate *and* incremental compilation, plus source metadata. Global checks re-run where the grammar is closed. See §5.6. |
| R7 | No re-allocation of input: results reference consumed input as **views/ranges**; for streams the parsed prefix must become GC-able; `String`s may use a different strategy; a per-input "visitor" controls memory behaviour. | A strategy type class per input kind with `Long` spans, mark/commit/low-water-mark eviction, and an explicit span-validity contract. The macro specialises the parser per strategy. See §5.7. |

## 1. TL;DR

- **No existing Scala library meets R2 + R4 together, and none meets R1 at all.**
  - The fast ones (parboiled2, fastparse) recurse on the JVM stack, analyse nothing statically, and
    use `Int` offsets.
  - The stack-safe ones (Parsley's VM, atto's trampolines, zio-parser's `StackSafe` backend) are
    `String`-only and do their checks at run time.
  - The only one with real grammar analysis (Scallion, LL(1) conflicts) is Scala 3 only and works at
    run time.
  - None abstracts semantic actions over a user `F[_]`.
- The strongest foundation in the literature is **grammar-as-data, analysed and compiled at compile
  time into a deterministic automaton with an explicit stack**. Concretely:
  - Krishnaswami & Yallop's *typed context-free expressions* (PLDI'19), which enforce LL(1) and rule
    out left recursion by typing.
  - flap's *lexer–parser fusion* (PLDI'23).
  - Parsley-Haskell's *staged selective combinators* (ICFP'20).
  - Menhir/Bison-style *conflict explanations and counterexamples* for diagnostics.

  A Hearth macro can play the role that MetaOCaml / Typed Template Haskell / LMS play in those papers.
- **Revised direction (see §5.8): "yacc embedded in macros".**
  - The primary front end is a BNF DSL inside `grammar(start) { … }`:
    `nonTerminal ::= (sym, sym, …) ==> { action }`.
  - A macro turns it into an **LR(1) automaton (IELR/LALR)**. The automaton is inlined as a `while`
    loop with an explicit stack, with semantic actions spliced into the reduce cases, pure or in a user
    `F[_]`.
  - No Scala library does this today. The prototypes of the syntax type-check on 2.13.18 and 3.8.3
    (§5.8).
  - The recommendation below is kept for history. LL(1) and combinators remain a secondary front end
    and back end.
- **Original recommended shape (superseded as the primary path):**
  - **Core:** LL(1) with Pratt/precedence operators, plus a layout scanner and stateful escape
    hatches.
  - **Back end:** generated `while` loops over `Array[Int]` stacks, exposed as a push-style `step`
    function driven by `F`.
  - **Optional second back end:** LR(1) (IELR/Pager), for grammars that are not LL(1). This is also
    the only setting where "shift/reduce conflict" diagnostics mean anything; in LL the equivalent is
    a FIRST/FOLLOW conflict.
  - **Not in core:** general algorithms (GLR/GLL/Earley/packrat). They break the streaming and
    bounded-memory guarantees.
- **The main design constraint is not parsing theory but macro visibility.** A Scala 2 macro cannot
  see the body of another `val`/`def`, and Scala 3 can only with `-Yretain-trees`. Global analysis
  therefore needs the grammar either passed to a single macro call, or serialised per rule into
  something a later macro can read. See §5.1.
- **Combining parsers while keeping diagnostics (R6) is feasible with a phantom *literal type*.**
  - Each `rule { … }` macro gives its `val` an inferred type such as
    `Rule[A] { type Meta = "<hash:local IR + summary + source position>" }`.
  - Literal types survive separate compilation on 2.13 and 3, and Zinc hashes them into the API, so
    changing a rule recompiles its dependents.
  - Nullability goes into the type proper (`Rule0`/`Rule`, cats-parse style). FIRST/FOLLOW sets should
    *not* be computed by implicits or match types; the macro computes them and stores them in the
    string.
  - Local checks run at each rule definition. Global checks re-run in a final `compile`/link macro.
    See §5.6.
- **Input without re-allocation (R7) is a solved problem in pieces, not in any one parser library.**
  - jsoniter-scala's buffer compaction to a mark, generalised to "the lowest of all live marks", gives
    bounded-memory streams.
  - Spans are `(Long, Long)` ranges, never host substrings. `String.substring` copies on HotSpot but
    *shares* on Scala Native and V8, so it can silently pin a whole input.
  - A per-input `InputStrategy` type class (the "visitor") decides span validity and materialisation.
    The macro specialises the generated code per strategy. See §5.7.

## 2. Existing Scala libraries

### 2.1 Comparison against the requirements

| Library (latest) | 2.13/3/JS/Native | Execution model | Deep nesting | Input | Speed | Static checks | Runtime debug | `F[_]` |
|---|---|---|---|---|---|---|---|---|
| **Parsley** 4.6.2 (Dec 2025); 5.0.0-M19 (Feb 2026) | ✓/✓/✓/✓ | Lazy combinator graph → optimiser → `Array[Instr]` for a stack VM, compiled **at run time** on first use | **Safe**: heap `CallStack`, handler and operand stacks | `String` only, `Int` offset; `parseFile` does `mkString` | Good when hot; cold-start compile; `flatMap` recompiles at run time | Runtime only: `many(pure x)` throws `NonProductiveIterationException`; laziness bugs. **No** left-recursion check (external Scalafix linter "parsley-garnish") | Best in class: `.debug`, breakpoints, `.profile`, `parsley-debug` tree, Dill GUI over HTTP | ✗ (pure; `ErrorBuilder[Err]` type class) |
| **parboiled2** 2.5.1 (Oct 2023) | ✓/✓/✓/Native 0.4 only | Macro deconstructs the rule DSL into an OpTree and renders imperative code; each rule rendered twice (fast and traced) | JVM stack | `String`/`Array[Char]`/`Array[Byte]`, in memory, `Int` | **Fastest** combinator-style | Typed HList value stack only; no loop or left-recursion detection (README warns `zeroOrMore(!',')` hangs) | Re-parses up to 4 extra phases on failure to build errors; "Grammar Debugging: TODO" | ✗ (mutable value stack; no rollback of side effects) |
| **fastparse** 3.1.1 (Jun 2024) | ✓/✓/✓/✓ | `inline`/macro operators over a mutable `ParsingRun`; parsers are opaque methods | **JVM stack**, documented as "Stack-Limited" with `-Xss` as the fix | `String`, `Iterator[String]`, `Reader`/`InputStream`. The buffer is dropped only after **cuts**; `.!` capture pins it; `index: Int` | ≈ parboiled2 | None (opaque) | `.log`, instrumentation hooks, `.traced` (re-parse, **impossible on streams**) | ✗ |
| **cats-parse** 1.1.0 (Dec 2024) | ✓/✓/✓/✓ | Optimised sealed ADT, `parseMut` interpreter | JVM stack (`Defer` calls through) | `String` | Slightly behind fastparse | **Type-level**: `Parser` (must consume) vs `Parser0`; `rep` only on `Parser`, so empty loops don't type-check | `Expectation`s, `withContext` | ✗ (cats instances) |
| **scala-parser-combinators** 2.5.0 (Sep 2026) | ✓/✓/✓/✓ | Closures `Input => ParseResult` | JVM stack | `Reader` / lazy `PagedSeq` | ~100× slower than fastparse | None | `log` | ✗ |
| **atto** 0.9.5 (2021, dormant) | ✓/3.0/✓/✗ | CPS + `Eval` trampoline | Safe | Incremental `feed`, but accumulates the whole `String` | Slow (allocation) | None | Context stack | ✗ |
| **zio-parser** 0.1.11 | ✓/✓/✓/✓ | ADT with two interpreters: `Recursive` (default) and `StackSafe` (explicit heap stack) | Selectable | `String`, `Chunk[In]` (tokens) | [unverified] | None | Parser/printer structure dumps | ✗ (typed errors; invertible `Syntax`) |
| **Scallion** (EPFL) | 3.7 only | LL(1) parsing with derivatives on a zipper (PLDI'20, Coq-verified) | Heap zipper [unverified] | `Iterator[Token]` (streaming) + silex lexer | [unverified] | **First-class LL(1) conflicts**: `NullableConflict`, `FirstConflict`, `FollowConflict` (at run time) | Resumable: returns the residual parser on an unexpected token | ✗ |
| **gll-combinators** / **Meerkat** | 2.x, dormant | GLL / memoised CPS | Heap | `String` | Cubic worst case | – | – | ✗ |
| **ANTLR 4** (Java target) | JVM | ALL(*), recursive descent | JVM stack | Streams (with bounded lookahead) | Good | Static analysis moved to parse time by design | Diagnostic listener, `DefaultErrorStrategy` recovery | ✗ |
| **tree-sitter** (jtreesitter, JDK 23+) | JVM via FFM | Incremental GLR, C | Heap | Bytes | Very good | Grammar-generation conflicts | Error recovery, incremental | ✗, not a Scala DSL |

### 2.2 Library notes

Only what matters for the design is listed here.

**Parsley**
- The only fast JVM library whose parsing does not recurse on the JVM stack. It uses seven heap stacks
  and a `@tailrec` loop (`internal/machine/Context.scala`, `stacks/CallStack.scala`).
- Costs of the model:
  - The grammar is compiled at program run time, so checks happen on first use rather than at
    `scalac` time.
  - Recursion must be knot-tied with `lazy val`.
  - `flatMap` builds a new instruction array while parsing.
- Things worth taking:
  - `precedence` with heterogeneous typed levels (`Ops`/`SOps`).
  - The configurable `token.Lexer`.
  - `ErrorBuilder[Err]`, a type class for the error representation.
  - The debugger UX: `DebugTree`, breakpoints, a remote GUI, and auto-naming (`@debuggable` in 5.0).
- It is the proof that "deep embedding + explicit-stack machine" is fast enough on the JVM. A macro
  removes the interpretive layer and the cold start.

**parboiled2**
- Closest in spirit to a macro-generated parser.
- Two ideas worth taking:
  1. Render every rule **twice**, a fast path and an instrumented path, and on failure *re-run* in
     instrumented mode. The happy path pays nothing for error reporting.
  2. A typed value stack in place of tuple allocation.
- Its weaknesses are exactly what R2 and R4 ask for:
  - JVM recursion.
  - In-memory `Int`-indexed input.
  - No analysis.
  - Two separate macro code bases (Scala 2 and Scala 3); the Scala 3 port took years.

  Hearth cross-quotes are the answer to that last point.

**fastparse**
- The only Scala library with real streaming (`ReaderParserInput`, `InputStream` via geny).
- Memory is bounded only by **cuts**. The docs report peak buffers of 1.4–3.6 % of input for
  ScalaParse and PythonParse. Captures pin the buffer. Tracing requires re-parsing, which is impossible
  on a stream: `checkTraceable()` throws. These are exactly the traps a streaming design must avoid.
- Indentation grammars are done with `flatMap` and an indent parameter (pythonparse).
- Ammonite used fastparse's failure-at-EOF to detect incomplete REPL input.

**cats-parse**
- Its `Parser` vs `Parser0` split is a cheap, type-level way to make `many(ε)` unrepresentable. A macro
  can generalise this with a real nullability analysis.
- It optimises the ADT through smart constructors: character-set merging, a radix tree for
  `oneOf(strings)`, and `void` elimination. These are compile-time rewrites for us.

**scala-parser-combinators**
- `PackratParsers` implements Warth-style seed growing and so supports left recursion. It is the only
  mainstream Scala option that does.

**Scallion**
- The closest existing thing to the static-analysis half of R4, but it runs at program run time and is
  Scala 3 only.
- Its conflict ADT and resumable zipper are both worth copying.

**Benchmark provenance warning**
- The widely quoted cats-parse README table (2021) benchmarks the **http4s `parsley` 1.5.0-M3 fork**,
  not j-mie6 Parsley 4/5.
- The fastparse numbers are also old.
- A new library should publish its own JMH numbers against current versions.

## 3. Algorithm families (beyond Scala)

| Family | Time | Explicit stack | Static diagnostics | Bounded-memory streaming | Verdict |
|---|---|---|---|---|---|
| LL(1)/LL(k) predictive | O(n) | Natural (a predictive automaton *is* a stack machine) | Excellent: FIRST/FIRST, FIRST/FOLLOW, left recursion, nullable loops, all cheap and decidable | Yes (k tokens) | **Core candidate**; needs left-factoring and `chain`/Pratt for operators |
| LL(*)/ALL(*) (ANTLR4) | ~linear in practice, O(n⁴) worst | Lookahead ATN simulation | Deliberately none at compile time | Unbounded lookahead | Not a fit for R4 |
| PEG / packrat | O(n) with memo, exponential without | Via a backtrack stack (LPeg-style VM) | Ford well-formedness (left recursion, nullable repetition) is decidable; ordered-choice *shadowing* only approximable (Redziejowski's LL(1)-in-PEG checks) | Packrat memo is O(n × rules), impossible for 4 GB. Bounded only with cuts; auto-inserting cuts on FIRST-disjoint choices (Mizushima et al.) gives "mostly constant space" | Useful as a *local* escape hatch, not the core |
| LR(1)/LALR/IELR/Pager | O(n) | Inherent | **Best in class**: shift/reduce and reduce/reduce with Menhir explanations; Bison counterexamples (Isradisaikul & Myers); error-state enumeration (Pottier) | Yes; push parsers are trivial (Bison `yypush_parse`, Menhir checkpoints) | **Optional second back end**; needed if "shift/reduce" diagnostics are literally wanted |
| GLR (tree-sitter, Lezer) | O(n) deterministic, O(n³) worst | Graph-structured stack | Ambiguity only at run time | Forks can hold unbounded input | IDE-grade recovery; not for R2 |
| GLL, Earley/Marpa | O(n³) worst | Heap | Run time | No | Out of scope for core |
| Pratt / precedence climbing | O(n) | Operator stack | Precedence-table consistency | Yes | **Built-in** for algol-like expressions |
| Derivatives on zippers (Scallion, Darragh & Adams) | Linear for LL(1) | The zipper is a heap continuation stack | LL(1) conflicts | Push-style by nature | Validates the LL(1) + resumable design |

**Staged/compiled combinators: what transfers to a Hearth macro**

1. **Krishnaswami & Yallop, PLDI'19 (ocaml-asp).**
   - Grammars are typed context-free expressions. Each expression has the type
     `{nullable, FIRST, FLAST}`.
   - Sequencing requires separability; alternation requires disjoint FIRST sets. Guarded `μ` excludes
     left recursion.
   - Well-typed grammars parse in linear time with one token of lookahead and no backtracking.
   - **For us:** the typing judgement *is* the compile-time diagnostic engine. Failures become
     `scalac` errors pointing at the offending sub-expression.
2. **flap, PLDI'23.**
   - The lexer (regex derivatives → DFA) and parser (typed CFEs) are written separately, then *fused*
     into one automaton. There is no token stream, which gives a large speedup.
   - **For us:** this is how R2 is met for JSON-class grammars. It branches on bytes directly, as
     jsoniter-scala does by hand (see `docs/research/jsoniter-codegen-techniques.md`).
3. **Staged selective combinators, ICFP'20 (parsley-haskell).**
   - Monadic `flatMap` is dropped in favour of selective `branch`/`select`, so the grammar is a
     *static* tree. It is optimised by algebraic laws, lowered to an explicit-stack abstract machine,
     and then staged.
   - **For us:** no dynamic `flatMap` in the DSL. Data-dependent choice goes through a finite
     `select`.
4. **Jonnalagedda et al., OOPSLA'14 (Scala LMS).**
   - A direct Scala precedent: staging removes combinator overhead, and a "recognise-only" fast path
     applies where results are discarded.
5. **Design Patterns for Parser Combinators (Willis & Wu).**
   - Covers the lexeme discipline, `chain`/`precedence`, and position-carrying smart constructors.
   - **For us:** these should be primitives the macro recognises.

## 4. Specific requirement areas

### 4.1 Compile-time diagnostics (R4)

This is what can realistically be reported at `scalac` time, with source positions of the DSL
sub-expressions:

| Diagnostic | Technique | Prior art |
|---|---|---|
| Left recursion (direct and indirect), with the cycle path | Fixpoint over "can derive `A…` without consuming" | Ford'04, asp typing, parsley-garnish |
| Nullable under repetition (`many(opt(x))`) with an ε-witness | Nullability fixpoint | Ford'04, cats-parse `Parser0`, Parsley runtime check |
| FIRST/FIRST, FIRST/FOLLOW conflicts, with the conflicting tokens and two example prefixes | LL(1) sets | Scallion conflicts, asp |
| Shift/reduce and reduce/reduce (LR back end only) | LR(1) automaton; Menhir-style "conflict string" and two derivations; optional Bison-style unifying counterexample | Menhir `--explain`, Bison `-Wcounterexamples` (the manual calls it costly: time-box it) |
| PEG shadowing (`"a" \| "ab"`) in ordered-choice mode | FIRST overlap (approximate); exact for regular alternatives via DFA inclusion | Redziejowski (PEG Explorer, LL(1p)) |
| Unreachable and unproductive rules | Classic fixpoints | – |
| Precedence-table inconsistencies | Table check | – |
| Ambiguity (undecidable in general) | Bounded-length exhaustive search under a time budget, producing a unifying counterexample | AMBER, Schmitz (conservative), Bison CEX |
| Error-message coverage | Enumerate error states, each with a minimal input reaching it | Menhir `--list-errors`, `.messages`, Pottier CC'16 |

Expensive analyses fit the existing `DerivationTimeout` / `-Xmacro-settings:<ns>.…` conventions.

### 4.2 Indentation (R5)

**Options:**
- **Python model (recommended default).** A pluggable *layout scanner* keeps an indent stack and
  bracket depth, and emits INDENT/DEDENT/NEWLINE. The grammar stays context-free and analysable, and
  memory is O(1), so it streams. tree-sitter does the same with external scanners.
- **Haskell `parse-error(t)` rule.** Needs parser→lexer feedback: "is the virtual close in the expected
  set?". Both LL and LR automata know their expected set per state, so a narrow hook is deterministic.
  Marpa's "Ruby Slippers" is the same idea.
- **Adams (POPL'13, Haskell'14).** Indentation relations annotate grammar symbols. It is elegant, but
  complicates the static analysis. A later extension at most.
- **YAML.** Practical parsers use a hand-written scanner with an indent stack. It should use the same
  hook.

### 4.3 Markdown / markup (R5)

CommonMark is **specified as an algorithm, not a grammar**:

- **Block phase:** line by line, against a stack of open containers, including lazy continuation.
- **Inline phase:** runs per leaf block and depends on document-global link definitions. Emphasis is
  resolved with a delimiter stack, the rule of 3, and an `openers_bottom` table to stay linear.
- cmark, pulldown-cmark and flexmark all hand-write this.

A general library can host it only through **escape hatches the macro inlines but does not analyse**:

- a line-oriented block driver with user block parsers (`tryStart`/`tryContinue`/`close`);
- sub-parsers run on collected spans, with shared context available through `F`;
- a delimiter-stack primitive;
- char-class/regex matchers with one character of look-behind;
- a column register.

CommonMark should be a **reference application**, not a promise that the core DSL checks it
statically. Simpler markups (AsciiDoc-lite, wiki markup, TOML, INI) fit the core with the layout
scanner.

### 4.4 Streaming, 4 GB, REPLs (R2, R3)

**Offsets**
- `Long` offsets everywhere, or a chunk-relative `Int` plus a `Long` base.
- Every surveyed Scala library is capped at 2^31 characters.

**Buffer**
- With deterministic LL(1)/LR(1) there is no backtracking. The buffer is the current chunk plus the
  lexer's longest-match window.
- Local PEG backtracking must declare a bound; the analyser warns when a window is unbounded.
- Captures should copy out, not pin.

**Output modes** (the same grammar, compiled differently)
- *recognise* (actions erased);
- *event/fold* (SAX-like, constant memory);
- *build* (AST).

A 4 GB JSON is realistic only in the first two modes. The `build` mode is limited by the heap, not the
parser.

**Push parsing**
- The generated parser is `step(state, chunk): Step`, where
  `Step = NeedInput | Done(result) | Error(state, expected)`.
- `NeedInput` at EOF on a viable non-accepting state is an **exact** "incomplete input" answer for a
  REPL.
- That is better than the heuristics in Python's `codeop` and the Scala 3 REPL (scala/scala3#27172,
  scala/scala3#5183).
- Precedents: Menhir's incremental API (persistent checkpoints, which gives cheap snapshots for REPL
  history) and Bison's push parser.

**Error recovery**
- *LR:* CPCT+ ("Don't Panic!", ECOOP'20) repairs 98.4 % of broken Java files with minimal-cost
  insert/delete sequences, and needs only the LR table.
- *LL:* ANTLR-style single-token insertion/deletion plus FOLLOW-set resync.

**Incremental reparsing**
- tree-sitter/Lezer-style is out of scope for v1.
- Keeping node positions relative, and storing the automaton state per node, would leave the door
  open.

## 5. Design implications for a Kindlings module

### 5.1 How the macro gets to see the grammar (the key constraint)

Global analysis (FIRST/FOLLOW, left recursion, conflicts) needs **all rules at once**, but:

- **Scala 2:** a macro sees only its own arguments. It cannot read the body of another `val`/`def`,
  even in the same file.
- **Scala 3:** `Symbol.tree` works across definitions only for the current compilation run, or with
  `-Yretain-trees`. This is not portable to Scala 2, and is fragile.

That is why fastparse and parboiled2 are per-rule and analyse nothing globally. The options:

- **A. Whole grammar inside one macro call.**
  - Shape: `val json = Grammar.compile[F] { g => val value: Rule[Json] = …; val obj = …; value }`.
  - Recursion is by name between local `val`s, which the macro resolves.
  - It is the simplest and works identically on 2.13 and 3 with Hearth's `DestructuredExpr`, as the
    `optics` DSL does.
  - Downside: no cross-file grammar modularity.
- **B. Per-rule macros that emit a serialised IR, plus a final `compile` macro.**
  - Each `rule { … }` expands to a value whose *type* carries a serialised grammar IR in a string
    literal type.
  - `Grammar.compile(rootRule)` collects and links the IR of every rule it reaches, including rules
    from other compilation units or libraries.
  - This allows reusable grammar libraries, such as a standard `Json` or `Expr` module.
  - §5.6 covers which carriers work and why: literal types do; annotations, companions and
    `Symbol.tree` don't.
- **C. Grammar as an ADT value, analysed at run time** (the Parsley model).
  - Portable, but loses compile-time diagnostics (R4). Rejected as the primary path, but useful as a
    test oracle and REPL-time fallback.

**Recommendation (revised after R6):** make **B** the primary mechanism, using the literal-type
carrier from §5.6. Keep **A** as a `grammar { … }` block for mutually recursive rules, which need to be
tied in one expansion anyway. Both produce the same carrier, so a block's rules compose with
standalone ones.

### 5.2 The effect `F[_]` and its type class (R1)

"Parsing effect" can mean two different things, and they should be separate type classes:

1. **Action effect.** How semantic actions build `F[Program]`.
   - Minimal needs: `pure`, `map2`/`product` (applicative is enough for a static grammar; no
     monadic bind is needed), and `raiseError` for semantic errors.
   - Optionally `delay`, for side-effecting actions such as symbol-table updates in a compiler.
   - If actions only need to be applicative, `F` could even be a *free applicative* the user
     interprets. That is worth exploring as the "figure out the operations during development" route.
2. **Driver effect.** How input arrives.
   - Needs `tailRecM` (stack-safe looping) and a way to pull the next chunk:
     `InputStream`, `fs2.Stream`, or a REPL line reader.
   - This is where `IO` lives.

**Performance rule:**
- When `F` is statically `Id`, or the type class instance is a known "pure" one, the macro must emit
  the plain `while` loop with no `F` wrapping.
- Effectful actions are applied per reduction, or batched per chunk, never per character.
- This follows the existing kindlings pattern of specialising generated code at compile time (the
  `semiEval` techniques in `kindlings-runtime-perf`).

"User can hard-code `F` or provide it as an input":
- `Grammar.compile[IO] { … }` gives a parser fixed to `IO`.
- `Grammar.compile { … }` gives an `F`-polymorphic parser
  (`def parse[F[_]: ParserEffect](in: Input): F[A]`).
- The polymorphic variant pays for dictionary passing unless inlined.

### 5.3 Engine

**Front end:** a deep-embedded, shared 2.13/3 DSL.
- Built from: `token`/char-class/regex, `~`, `|`, `many`/`sepBy`, `opt`, named recursive rules,
  `select`/`branch`, `precedence`/`chain`, `cut`, `layout`, `map` with actions.
- No dynamic `flatMap`. The DSL markers are `@compileTimeOnly`, following the `optics` recipe in
  `hearth-expr-parsing-dsl`.

**IR:** typed context-free expressions plus a regex/DFA lexer IR. Optimisations:
- char-set merging;
- radix tree for keywords;
- dropping unused results;
- **lexer fusion** where the lexer is regular.

**Analysis:** see §4.1. Errors abort compilation, warnings are configurable, and everything is
time-boxed.

**Back end A (default): LL(1)**
- A single state loop: `Int` state, `Array[Int]` return stack, and value slots.
- Typed `var`s where possible; otherwise an `Array[AnyRef]` value stack, as in the jsoniter perf
  work.
- Stack safe by construction.
- **Watch the JVM 64 KB method limit.** Large grammars must be split into several methods, or use
  table encoding (ANTLR serialises its ATN into strings).

**Back end B (opt-in, later): LR(1) via IELR or Pager**
- For grammars that are not LL(1): C-like languages, and anything the user will not left-factor.
- Brings shift/reduce diagnostics, precedence declarations, and CPCT+ recovery.
- Policy: when the LL(1) check fails, report the LL conflict *and* whether the grammar is LR(1).

**Escape hatches (inlined, not analysed):**
- `StatefulScanner[S]` (layout, heredocs, YAML indents), with read access to the expected-token set;
- line/block drivers and a delimiter stack for markup;
- bounded local backtracking.

### 5.4 Runtime debugging (R4)

- **Tracing** is compiled in only when requested, so production code carries nothing. It is enabled
  either by an import marker like the existing `LogDerivation`/`AllowDerivation` markers, or by
  `-Xmacro-settings:<ns>.trace=true`.
- The tracer is a user-pluggable type class that receives events:
  `(rule, state, offset: Long, token, action, stack depth)`.
- Front-ends: a text logger, a JSON/HTTP exporter (Parsley Dill-style GUI), and breakpoints.
- **Errors, the parboiled2 way.** The fast path tracks only "furthest failure + expected set", cheap
  and always on. For `String`/in-memory input, a failure triggers an instrumented re-run for rich
  traces. Streams get only the cheap report, unless the user opts in to a bounded replay buffer.
- **Error message type class**, the Parsley `ErrorBuilder` way.

### 5.5 Coverage of grammar styles (R5)

| Style | Needs | In core? |
|---|---|---|
| Algol-like (C/Pascal/Scala-lite) | Lexer with keywords, `precedence`, LL(1) with left factoring; LR back end for the hard cases | ✓ (+ back end B) |
| Indentation (Python/Haskell/YAML-lite) | Layout scanner (INDENT/DEDENT), expected-set feedback hook | ✓ (scanner hook) |
| Data (JSON/CSV/TOML/…) | Fused lexer, recognise/event/build modes, streaming | ✓ (primary perf target) |
| Markdown/CommonMark | Block driver, delimiter stack, sub-parsers on spans | Escape hatch; reference app |
| Wiki/INI-style markups | Layout scanner + core | ✓ |

### 5.6 Composing parsers without losing diagnostics (R6)

**Why phantom types, and which kind**

A `val json = rule { … }` def macro controls exactly one thing about its `val` that survives separate
compilation: the **inferred type**. The carriers compare as follows:

| Carrier | Survives separate compilation (2.13 / 3) | Zinc invalidates dependents on change | Producible by a def macro | Verdict |
|---|---|---|---|---|
| **String-literal type**, e.g. `Rule[A] { type Meta = "…" }` (ConstantType) | ✓ / ✓ (pickled / TASTy) | ✓: Zinc's `ExtractAPI` keeps constant types for vals on both compilers | ✓ (whitebox / `transparent inline`) | **Primary** |
| Annotation with literal args | ✓ (only `StaticAnnotation` on 2.13) / ✓ | ✓ | ✗: needs a macro annotation; `MacroAnnotation` is still `@experimental` on Scala 3 | Opt-in at most |
| Generated companion or `final val` constant | ✓ | ✓ | ✗ on Scala 3 (a def macro can't add public definitions) | Only with a source generator |
| Scala 3 `inline def` body | – / ✓ | ✓ (Zinc hashes inline bodies) | n/a | Scala 3 only; recursive rules loop the inliner |
| Other vals' bodies via `Symbol.tree` / `-Yretain-trees` | ✗ / fragile | **✗: bodies are not in the API hash, so results go stale under incremental compilation** | – | Reject |

**Type-level FIRST sets: not at real size.**
- On 2.13 there are no union types, so sets would be HLists resolved by implicit search. Error
  messages degrade to "could not find implicit `Disjoint[…]`", and compile times explode.
- On Scala 3, match types get stuck on abstract types inside generic rules.
- Unicode character classes have thousands of members.
- Recursive rules need a type-level fixpoint that match types can't express cleanly.
- Even the dependently typed prior art keeps only small indices in types. Danielsson's *Total Parser
  Combinators* (ICFP 2010) indexes parsers by the results they return on empty input (i.e.
  nullability), not by FIRST sets.

**Recommended split:**
1. **In types (checked by `scalac` directly, for good error messages):**
   - the result type `A`;
   - nullability via the class split `Rule0[+A]` / `Rule[+A] <: Rule0[A]`, so `rep`/`sepBy` on a
     nullable rule is an ordinary type error, as in cats-parse.
2. **In the literal carrier:** a versioned, compact string containing:
   - a **Merkle hash**: the local IR plus the hashes of referenced rules, so a change to a leaf
     changes every ancestor's type and Zinc recompiles the chain;
   - the **local IR** of this rule only (no transitive closure, to avoid quadratic size), with
     `Ref(symbolPath, hash)` for other rules, `Param(i)` holes for templates, and `Mu`/`Var` for local
     recursion;
   - the precomputed **summary** `{nullable, FIRST, FLAST, productive, hasFreeRefs}`, in
     Krishnaswami–Yallop style, so leaf rules need not be re-analysed at link time;
   - **`RuleMeta`**: the rule name (from the enclosing owner, like parboiled2's `rule`) and a
     *source-root-relative* file, line and column. Relative paths keep API hashes reproducible across
     machines and CI caches.

**Where analysis runs**
- **At `rule { … }` (local):**
  - typing errors;
  - K&Y separability and disjointness checks on every closed sub-term;
  - nullable repetition;
  - left recursion inside the rule and its `Mu` binders;
  - PEG shadowing inside the rule;
  - template termination.

  Errors point at the exact sub-expression.
- **At `Parser.compile(entry)` (global link):**
  - follow `Ref`s by reading the referenced symbols' types;
  - verify the stored hashes; a mismatch means a stale classpath, so report it and don't guess;
  - instantiate templates;
  - compute FIRST/FOLLOW over the closed grammar;
  - report cross-module left recursion, LL(1)/LR conflicts, ambiguity search results, keyword and
    token clashes, and opaque-node warnings.

  This step is unavoidable: FOLLOW sets are global, so two individually LL(1) modules can conflict
  once linked.
- The link expansion must **mention every rule symbol it read** (in the generated metadata table). That
  gives Zinc a recorded dependency on symbols reached only through `Ref`s inside strings. Merkle
  hashes are the second safety net.

**Recursion and ascriptions**
- A recursive `lazy val` needs a type ascription, and an ascription erases the carrier. Offer instead:
  - `rule.fix("expr") { self => … }`, an explicit μ like cats-parse's `recursive`;
  - a `grammar { … }` block for mutually recursive rules (option A);
  - templates parameterised by the missing rule.
- An ascribed rule degrades to an opaque reference, with a warning that tells the user how to fix it.

**Parametrised rules** (`sepBy(p, sep)`, `between`, `precedence(atom)(ops…)`)
- **Templates:** a user `def sepBy[A](p: Rule[A], …) = rule { … }` stores a template IR with `Param`
  holes in its result type. Each use site substitutes the arguments' carriers and analyses *that
  instantiation*. This is Menhir's approach: parameterised nonterminals expanded at compile time, with
  a termination check on growing arguments. It is also the Parsley-Haskell/asp approach, where
  combinator functions run at staging time.
- **Built-in templates** (`precedence`, `chain`, `layout`) are IR nodes. Their conflicts are reported
  in operator and level terms.
- **Opaque rules** (data-dependent parsing, hand-written scanners) have a declared or unknown summary
  (FIRST = any). The link step warns and names them, and users can declare `first`/`nullable` to
  restore the checks.

**Modular grammars** (reusing and extending a `Json` or `Expr` module)
- **Rats!** (Grimm, PLDI 2006): grammar modules can be parameterised by other modules and can add,
  remove or override **labelled alternatives**. The lesson: give alternatives stable labels (their
  `RuleMeta` name) so downstream modules can extend them deterministically.
- **Copper/Silver** (Schwerdfeger & Van Wyk, PLDI 2009): each extension is checked against the host
  alone, which guarantees that all passing extensions compose conflict-free. It achieves this by
  restricting extensions to start with a unique *marking terminal*. The LL analogue is
  `host.extend(alt)`, which requires `alt`'s FIRST set to be disjoint from the host's alternatives and
  checks this at the extension's definition. The full link-time check still runs.
- **Menhir** (`%public`, `%inline`) and **ANTLR** `import` both analyse only the joined grammar. That
  confirms the link macro as the place for the global analysis.

**Runtime debugging survives composition**
- The link macro emits one static `Array[RuleMeta]`. Generated states and instructions carry only
  `Int` ids, so debug metadata costs nothing on the hot path.
- Traces and errors print `rule @ file:line`, plus the template instantiation chain ("in `sepBy`
  instantiated at Foo.scala:12, defined at Combinators.scala:40"). This holds even when rules come
  from other libraries. It generalises parboiled2's `RuleTrace.Named` and Parsley's `@debuggable`
  name registry, without needing macro annotations.

**To verify before committing** (spike 1 in §7):
- the maximum literal-type length on 2.13 and 3, including JS and Native;
- that `transparent inline` refinement types stay as the inferred type of public vals;
- that a leaf-rule edit recompiles the link site through the Merkle chain on both compilers;
- how the carrier prints in type-mismatch errors (keep it in a `type Meta` member, not a visible type
  argument).

### 5.7 Input, views and memory (R7)

**Facts that shape the design**
- `String.substring` **copies** on HotSpot. It has done so since 7u6, precisely to stop small
  substrings pinning huge parents.
- It **shares** the backing store on Scala Native (its `String` still has `offset`/`count`) and on V8
  for results of 13 chars or more (`SlicedString`).
- So "use `substring` as a view" is wrong in both directions. It costs O(n) on the JVM and can leak on
  JS/Native.
- **Spans must therefore be the library's own `(start: Long, end: Long)` over a strategy-owned store**,
  and materialisation must be an explicit, strategy-controlled step.
- A generic `CharSequence.charAt` in the hot loop becomes megamorphic (itable dispatch, no inlining)
  once three or more input classes reach it. Macro specialisation per input type removes this.

**Prior art for "the parsed prefix becomes GC-able"**

| System | Mechanism | Lesson |
|---|---|---|
| jsoniter-scala `JsonReader.loadMore` | One `Array[Byte]`. On refill, compact to `min(mark, pos)`. `Long totalRead` for positions. Bounded by `maxBufSize`. | **Low-water-mark eviction with one mark**. Generalise to many. |
| fastparse `UberBuffer` + `dropBuffer` | Ring buffer, dropped only after a cut, and never while a capture or lookahead is open | Cuts are what advance the mark. The grammar (or the macro, for LL(1)-disjoint choices) must supply them. |
| Jackson `getTextCharacters`, SAX, Go `Scanner.Bytes`, simdjson `string_view` | Zero-copy view valid **until the next step** | Ideal for event/visitor consumers; wrong for ASTs that outlive the step |
| Jackson `NonBlockingJsonParser`, jawn `AsyncParser`, attoparsec `Partial` | `feedInput(chunk)` returns `NOT_AVAILABLE`/`Partial`. Partial tokens are saved internally, so the caller's chunk is released. | Push API; with an explicit parser stack the continuation is just the stack plus the state |
| Netty `ByteBuf`, Rust `bytes`, Okio `Segment` | Refcounted or shared slices. Okio copies spans under 1 KiB instead of sharing, so tiny spans don't pin 8 KiB segments. | Pinning granularity is the chunk. Use a copy-small, share-large heuristic. |
| fs2 `Chunk` slices, V8 `SlicedString`, attoparsec `ByteString` | GC-based sharing | Safe but pins unpredictably. attoparsec retains the whole input until `Done`: the failure mode to avoid. |
| JDK 22 FFM `MemorySegment` via `FileChannel.map(…, Arena)` | `Long` offsets, zero-copy `asSlice`, deterministic unmap on `arena.close()` | The mmap strategy (older JDKs need windows of 2 GB `MappedByteBuffer`s) |

**Span-validity contracts.** The strategy declares one, and the macro reads it at compile time:
1. **Forever:** in-memory `String`/`Array`, and mmap while its arena is open. Spans can go straight
   into results.
2. **Until committed:** chunked streams. A span is valid while `start >= lowWaterMark`. Before the
   mark that protects a span is released, the generated code either materialises it (copy on capture)
   or hands it to the user callback for immediate use.
3. **Until the next step:** event mode. The zero-copy callback style, like Jackson and SAX.

**Low-water-mark eviction with an explicit stack**
- Backtrack points and open captures are LIFO. They mirror the parser's own explicit stack, so the
  low-water mark is simply the bottom-most live mark: O(1), no heap.
- Cuts, and in LL(1) mode every committed prediction, advance it.
- User-held spans are the only non-LIFO pins. Don't track them; make them materialise at the contract
  boundary. That keeps memory bounded and avoids the attoparsec retention failure.

**Sketch of the strategy ("visitor") type class**

This is a strawman for the spike, not an API proposal.

```scala
trait InputStrategy[I] {
  type Unit                 // Byte or Char: selects the byte- or char-level automaton at compile time
  type State                // mutable buffer/cursor state, allocated once per parse
  def open(input: I): State

  // hot path: must be final/inlinable; the macro may instead splice codegen snippets (see below)
  def ensure(s: State, pos: Long, n: Int): Boolean   // make [pos, pos+n) addressable; false = EOF
  def unitAt(s: State, pos: Long): Int

  // memory control
  def mark(s: State, pos: Long): Int                 // LIFO, mirrors the parser's backtrack stack
  def release(s: State, mark: Int): Unit
  def commit(s: State, pos: Long): Unit              // cut: nothing before pos is revisited
  def lowWaterMark(s: State): Long

  // spans
  def validity: SpanValidity                         // Forever | UntilCommitted | UntilNextStep (compile-time constant)
  def materialize(s: State, start: Long, end: Long): String
  def regionEquals(s: State, start: Long, end: Long, lit: String): Boolean  // allocation-free keyword match
  def lineColumn(s: State, offset: Long): (Long, Int) // lazy; error path only
}
```

**Three reference strategies**
- **`String` / `CharSequence` in memory:**
  - `Int` cursor (the macro emits `Int` when the strategy's max length fits), `charAt` monomorphic.
  - Marks are no-ops, validity is **Forever**, spans are zero-copy.
  - `materialize` is `substring`, only on demand.
  - Optionally a `StringView extends CharSequence` for users who want a view object.
- **Chunked `InputStream` / `Reader` / pushed chunks:**
  - The jsoniter scheme generalised to a mark stack: one growable array, `base: Long`, and compaction
    to the low-water mark on refill.
  - Alternatively a segment ring that drops whole segments, which avoids a memmove for long marked
    regions.
  - Validity is **UntilCommitted**. There is a configurable max window with a clear "token too long"
    error.
  - UTF-8 multi-byte sequences crossing a refill need up to 3 bytes of carry.
  - A `feed(chunk)` push variant for Scala.js, fs2 and REPLs.
- **Memory-mapped file:**
  - JDK 22+ `MemorySegment` with `Long` offsets; older JDKs use overlapping `MappedByteBuffer`
    windows; Native uses `mmap` `Ptr[Byte]`; JS falls back to chunked.
  - Validity is **Forever** within the arena scope. The OS page cache, not the GC heap, holds the data.

**How the macro uses it.** The instance is resolved statically at the `compile`/`parse` site, so the
generated parser:
- is emitted against the concrete strategy: monomorphic calls, or better, the strategy supplies Hearth
  `Expr` snippets for `load`, `ensure` and `refill`, so the loop contains raw `buf(i)`;
- keeps the slow refill path in a separate non-inlined method, as jsoniter's `loadMoreOrError` does;
- inserts `materialize` before `release` only when `validity != Forever`;
- drops mark bookkeeping entirely for `Forever` strategies;
- picks the byte or char automaton from `Unit`.

The cost is one copy of the generated code per (grammar, strategy) pair actually used.

**Line and column.** Track neither on the hot path. Keep a per-chunk `linesBefore: Long` for evicted
chunks, and compute line and column lazily by scanning back to `\n` (as fastparse's lazy
`lineNumberLookup` does) only for errors and traces.

### 5.8 BNF / yacc-style front end ("yacc embedded in macros")

The user-facing shape, after two rounds of syntax probes (details below):

```scala
// the single surface, identical on 2.13 and 3 (decided 2026-09-29)
val parser = grammar[PostalAddress, IO] { g => import g._
  // phantom declarations: exist only to type-check the wiring; the macro erases them (and `g`)
  val postalAddress = nonTerminal[PostalAddress]
  val name          = nonTerminal[String]
  val optSuffix     = nonTerminal[String]
  val personal      = terminal("[A-Z][a-z]+")
  val zip           = terminal("[0-9]{5}").map(_.toInt)
  val EOL           = terminal("\n").map(_ => ())

  // `all(...)` is one sequence, `||` separates alternatives.
  // `{ ... }` must return F[A] (checked by the compiler); `.pure { ... }` is the pure fast path.
  postalAddress ::= all(name, zip) { (n, z) => IO(PostalAddress(n, z)) }
  name ::= (
       all(personal, "-", personal, optSuffix, EOL).pure { (a, _, b, s, _) => a + b + s }
    || all(personal, name).pure                         { (p, n) => p + n }
  )
  optSuffix ::= "Sr." || "Jr." || ""   // literals are singleton values; "" is the empty alternative

  postalAddress // start symbol = block result
}
```

The first draft used bare tuples, `(a, b, c) { … } | (d, e) { … }`, with symbols declared outside the
block. Probe round 1 below shows why that was replaced.

**Why this is the strongest position.**
- The Scala ecosystem has combinator libraries (Parsley, fastparse, cats-parse) and one old,
  external-file LALR tool (ScalaBison, which drives bison). To my knowledge, nothing embeds a yacc
  grammar in Scala source and compiles it at `scalac` time into an inlined, effect-polymorphic parser.
  [unverified that no such library exists.]
- Outside Scala, the closest relatives generate code *from a separate grammar file*: Menhir (`.mly`),
  LALRPOP (`.lalrpop` plus a build script), Happy, and grmtools (`.y`).
- A Hearth macro gives the same power in-source, cross-compiled 2.13/3, with IDE navigation from every
  symbol to its definition.
- BNF with productions is also the most analysable form. Nonterminals are explicit, every alternative
  has a source position, and actions fire at reductions. That is exactly LR's model and the setting
  where shift/reduce and reduce/reduce diagnostics (R4) exist.

**Syntax probes.** Plain marker types, no macro, compiled with scalac 2.13.18 and 3.8.3.

| # | Question | 2.13.18 | 3.8.3 | Consequence |
|---|---|---|---|---|
| 1 | Does `::=` bind looser than `\|`? | ✓ | ✓ | `::=` ends in `=`, so it has assignment-operator (lowest) precedence and is left-associative. `a ::= b \| c` is `a ::= (b \| c)`. |
| 2 | `(a, b, c) { (x, y, z) => … }` with lambda parameter types inferred from the symbols | ✓ via an `implicit class` on `(Sym[A], Sym[B], Sym[C])` with `apply` | ✓ same mechanism | Works, but see #3 |
| 3 | Error quality for a wrong action type or arity with juxtaposition `(…) { … }` | Good ("found Int, required String") | **Misleading**: Scala 3 tuples already have `apply(n: Int)`, so the error reports "missing parameter type … expected Int" | Use an explicit connector such as **`==>`**, which gives clean errors on both. Allow juxtaposition optionally at most. The grammar macro can't repair this, because the block is typed before the macro runs. |
| 4 | Alternatives with a **leading** `\|` on a new line | **✗** `not found: value \|`, even with `-Xsource:3` | ✓ | Cross-compiled grammars put `\|` at the **end** of the line, or use one statement per alternative (e.g. `name \|= (…) ==> {…}`, which is also an assignment operator) |
| 5 | Terminal regex visible in the *type* without a macro, across separate compilation | ✓ `def terminal[Re <: String with Singleton](re: Re): Terminal[Re, String]` infers `Terminal["[0-9]{5}", String]`; `.map` keeps `Re`; a mismatch is rejected from another compilation run | ✓ same (use `&` instead of `with`) | Terminals need **no macro** and are a free carrier (§5.6). `"…".r` must **not** be used, because `Regex` loses the literal. |
| 6 | Non-terminals declared outside the block | ✓ | ✓ | `val name = nonTerminal[String]` has no body. The grammar macro keys on the `val`'s symbol, takes its name for traces, and recursion and forward references are free. This removes the "ascription erases the carrier" problem of §5.6 for BNF grammars. |

**Probe round 2: the `all(…) { … } || all(…) { … }` form with in-block declarations** (both compilers):

| # | Question | 2.13.18 | 3.8.3 | Consequence |
|---|---|---|---|---|
| 7 | Overloaded `all` per arity, action in a second parameter list: `def all[A, B, R](a: Sym[A], b: Sym[B])(f: (A, B) => R): Alt[R]` | ✓ | ✓ | Overload resolution happens on the first list (arity), then lambda parameter types are inferred. A builder variant (`all(a, b)` returns `All2[A, B]` with `apply`) behaves identically and leaves room for modifiers such as `.prec(times)` before the action. |
| 8 | Wrong action result type | "found Int, required String", pointing at the expression | "Found: (z : Int) Required: String" | Clean on both, unlike juxtaposed tuples (#3) |
| 9 | Wrong action arity | "missing parameter type … required: (String, Int) => String" | "Wrong number of parameters, expected: 2" | Clean on both |
| 10 | Leading `\|\|` on a new line | ✗ (statement ends at the newline) | ✓ | Same as #4. |
| 11 | Leading `\|\|` **inside parentheses**, `name ::= ( … \|\| … )`, or varargs `oneOf(all(…), all(…))` | ✓ | ✓ | Newlines inside parentheses don't end statements, so the parenthesised layout is the documented cross-compiling style. |
| 12 | `nonTerminal`/`terminal` declared as **local `val`s inside the `grammar { … }` block**, self-recursive productions, block result as start symbol | ✓ (no unused warnings with `-Wunused:locals`) | ✓ (none with `-Wunused:all`) | The macro sees every declaration, the regex literal and the `.map` lambda **in the tree**. No type carrier is needed for in-block symbols (§5.6 carriers matter only for symbols shared across grammars), and the phantom values are erased from the output. |
| 13 | Productions using a symbol declared *later* in the block | ✗ "forward reference … extends over definition" | ✗ same | Rule: **declarations first, then productions**. The compiler enforces it with a clear message, so the macro needs no extra check. |

**Probe round 3: inline literals and regexes** (both compilers):

| # | Question | 2.13.18 | 3.8.3 | Consequence |
|---|---|---|---|---|
| 14 | Inline string literals and `"…".r` regexes as `all(…)` arguments, via implicit conversions `String => Sym[Unit]` and `Regex => Terminal[…, String]`, including `"-?[0-9]+".r.map(_.toInt)`, mixed with non-terminals, arities up to 6 | ✓ | ✓ | For example `all("(", "-?[0-9]+".r.map(_.toInt), ",", …, ")", label) { (_, x, _, y, _, l) => … }`. Lambda parameter types come out as `Unit`/`Int`/`String` as expected. |
| 15 | Wrong action type with an inline regex (`all("[0-9]+".r) { s => s }` for an `Int` non-terminal) | "found String, required Int" | "Found: (s : String) Required: Int" | Clean on both |

What the macro does with inline terminals:
- It matches the conversion calls in the typed tree (`litSym("(")`, `reSym(augmentString("…").r)`,
  optionally followed by `.map(f)`) and extracts the literal.
- A **non-literal** argument (`val s = …; all(s.r)`) type-checks, but the macro rejects it with a
  positioned error: "terminal patterns must be literals".
- The `Regex` object is never built at runtime. The pattern is parsed by the macro into its
  DFA-compatible subset (§5.8 step 2).
- Identical literals or patterns used in several places become **one** lexer token.
- On equal-length matches, string literals take priority over regexes, the usual lex/ANTLR rule, so
  `"origin"` beats `[a-z]+`. Context-aware scanning means a keyword only competes where the LR state
  can accept it.
- Diagnostics name inline terminals by their text: `expected "(" or "origin"`, or
  `/[a-z]+/ at Grammar.scala:31`.
- Superseded by round 4: string literals are **singleton values** by default (their value is the
  literal itself), not `Unit`.

**Probe round 4: literals as singleton values** (both compilers).

The motivating rule is `<opt-suffix-part> ::= "Sr." | "Jr." | ""`. The probes use
`implicit def litSym[S <: String with Singleton](s: S): Sym[S]` and
`litAlt[S <: String with Singleton](s: S): Alt[S]`, with a covariant `Alt[+A]` whose method is
`||[B >: A]`.

| # | Question | 2.13.18 | 3.8.3 | Consequence |
|---|---|---|---|---|
| 16 | `optSuffix ::= "Sr." \|\| "Jr." \|\| ""` for `nonTerminal[String]`: bare literals as whole alternatives, `""` as ε | ✓ | ✓ | BNF-literal style works with no `all`/action |
| 17 | The same into `nonTerminal["Sr." \| "Jr."]` | n/a (no unions) | ✓ | Scala 3 keeps precise literal unions when the non-terminal asks for them |
| 18 | `n ::= "Sr." \|\| "Jr."` for `nonTerminal[Int]` | "found String("Jr."), required Alt[Int]" | "Found: Alt[String] Required: Alt[Int]" | Clean errors |
| 19 | A bare regex as a whole alternative, `first ::= "[A-Z][a-z]+".r` | ✓ only with its own `Regex => Alt[String]` conversion | same | Implicit conversions don't chain, so every "symbol as a whole alternative" form needs a direct conversion |
| 20 | Inside `all("Sr.") { s => … }`, is `s` typed `"Sr."`? | ✗, widened to `String` | ✗, widened to `String` | Type inference widens the singleton when instantiating `all`'s type parameter. This only affects the *static* type; the macro still knows the value is the constant `"Sr."`. |

**Value synthesis and dead-value elimination** (macro design; not yet prototyped)

1. **Literal values are free.**
   - A matched literal's value is the literal constant. The generated reduce code pushes or substitutes
     the constant (a constant-pool `String` on the JVM). It never reads the value back from the input
     or allocates.
   - For `"Sr." || "Jr." || ""`, each alternative's reduce case yields its own constant.
2. **Unused parameters are not synthesised.**
   - For every action lambda, the macro checks which parameters are `_` or never referenced in the
     body.
   - For those it emits no span materialisation (§5.7), does not run the terminal's `.map`, and pushes
     nothing onto the value stack.
   - The lambda is rewritten to bind only the live parameters, then beta-reduced into the reduce case.
     For example, `all(first, " ", optSuffix) { (f, _, s) => Name(f, s) }` never materialises `" "`.
3. **Liveness propagates through non-terminals (whole-grammar fixpoint).**
   - A non-terminal's value is *live* iff it is the start symbol, or some production binds it to a
     parameter that is live.
   - A dead non-terminal's productions compile **without actions and without materialising their
     terminals**, i.e. recognise-only, and this propagates to everything below them.
   - The "recognise mode" of §4.4 then falls out per subtree instead of being a global switch. For
     example, a JSON grammar whose action ignores some fields' values skips building those subtrees
     entirely, which is the cheap path for 4 GB inputs.
4. **Constant folding.** A live parameter whose symbol can only produce constants (a literal, or a
   non-terminal whose alternatives are all literals) is passed as the matched constant. That is an
   `Int` alternative index plus a constant table, with no value-stack traffic for strings.

**Semantics this assumes**
- Pure actions and `.map` functions may be skipped, so they must be side-effect free. That is the
  same contract as lazy evaluation.
- Actions returning `F[B]` (§5.8 effects) are **never** dropped, because their effect is observable.
- An explicit escape hatch (e.g. `all(…).always { … }`) forces evaluation for rare impure pure-typed
  actions.
- Diagnostics can report the analysis in debug builds ("non-terminal `comment` value is never used:
  compiled as recognise-only"). That also helps catch actions that accidentally discard values.

**Implementation notes to verify in the spike**
- Hearth's lambda destructuring (`DestructuredExpr.extractLambda` or equivalent) and a traversal to
  find parameter references.
- Moving the lambda body into generated code requires owner fixing on Scala 3 (`changeOwner`). Check
  what Hearth's `Expr` splicing already does before relying on it.

**Notes on in-block phantoms**
- The phantom constructors (`nonTerminal`, `terminal`, `all`, `::=`, `||`) can be `@compileTimeOnly`
  for in-block use, which guarantees none survives into runtime code.
- Symbols shared *between* grammars (declared outside, §5.6) need non-`compileTimeOnly` variants.
  Those keep their regex in the type via the `Singleton` bound (#5).
- #5 needs `String with Singleton`, because 2.13 has no `&` even with `-Xsource:3`. In shared sources
  Scala 3 then emits a deprecation warning, so that signature belongs in version-specific sources.
- Unit-valued symbols (`"-"`, `EOL`) remain positional `_` parameters.

Probe sources are not committed. The findings are summarised here, and spike 6 in §7 re-creates them
properly as tests.

**Remaining syntax decisions**
- **Unit-valued symbols** (`EOL`, `"+"`) appear as lambda parameters, which users write as `_`. That
  mirrors Menhir's `$1 … $3` and LALRPOP's `<a:Expr> "+" <b:Term>`.
  - Dropping them automatically would need type-level tuple filtering *before* the lambda is typed.
    Scala 3 match types can do that, but Scala 2 would need an overload per arity × mask.
  - Recommendation: keep `_` in v1.
- **Start symbol:** write `grammar(postalAddress)` rather than `grammar[PostalAddress]`, which is
  ambiguous when two non-terminals share a type.
- **Precedence and associativity declarations** (yacc `%left`/`%right`/`%nonassoc`, `%prec`) go inside
  the block as statements, e.g. `left(plus, minus); left(times, div)` and `(…) ==> {…} prec times`.
- **EBNF sugar** (`opt(x)`, `rep(x)`, `rep1(x)`, `sepBy(x, comma)`) desugars into generated helper
  non-terminals, as in Menhir's standard library (`list(X)`, `separated_list(sep, X)`). It is built
  left-recursive, so the LR stack stays flat on long lists.
- **Arity:** 2.13 needs generated overloads up to 22. Scala 3 can use a single generic-tuple signature.

**What the macro does.** This is a non-derivation module following the `di`/`mock`/`optics` recipe.
1. **Walk the typed block** (`DestructuredExpr`):
   - collect the local phantom declarations, the `::=` statements, `all`/`||` alternatives, precedence
     declarations and actions;
   - resolve each symbol reference to a non-terminal (by local symbol) or a terminal (regex literal and
     `.map` lambda read from its in-block declaration, a string literal inline, or, for shared
     external terminals, the literal type argument);
   - read opaque or combinator-built rules through their §5.6 carriers.
2. **Build the lexer:**
   - parse the regexes itself, using a restricted, DFA-compatible syntax; back-references and
     look-around are rejected at compile time with the terminal's position;
   - build one Unicode-aware DFA over bytes or chars (per `InputStrategy`, §5.7);
   - use **context-aware scanning** (Copper): in each LR state, only terminals valid there are
     candidates. That resolves keyword/identifier clashes, which also matters when composing grammars.
3. **Build the LR(1) automaton:** IELR(1), or LALR(1) as a fast option. Report conflicts Menhir-style:
   the conflict token, the two items, and each production's `file:line` (the `::=` call site). Optional
   Bison-style counterexamples are time-boxed by `DerivationTimeout`. Also report unreachable and
   unproductive non-terminals and unused precedence declarations.
4. **Generate code:**
   - a direct-coded or table-driven state machine in a `while` loop, with `Array[Int]` state stack and
     value stacks (split into primitive and reference stacks to avoid boxing `Int`/`Long`/`Double`
     results);
   - no JVM recursion, so it is stack safe;
   - tables encoded as string literals when big, to respect the 64 KB method limit;
   - **action lambdas beta-reduced and spliced into their reduce case**, so there is no `Function`
     allocation per reduction.
5. **Push interface:** the same machine is exposed as `step(chunk)` returning `NeedInput | Done | Error`.
   This gives exact REPL incompleteness via LR's viable-prefix property, with `InputStream`/`String`
   drivers on top.
6. **Debugging:**
   - a static `RuleMeta` table (non-terminal, alternative index, `file:line`);
   - traces of shift, reduce and goto events when tracing is compiled in (§5.4);
   - error messages listing expected terminals by their `val` names;
   - optionally a Menhir `--list-errors`-style coverage report of error states that have no custom
     message.

**Effect typing: `grammar[Result, F] { … }`**

`F` is declared once, at the grammar. Every action's result is then **checked by the compiler** to be
`F[A]`, where `A` is the type of the non-terminal on the left of `::=`. For this to work, `F` must be
known *while* each action lambda is typed, so that the expected type is `F[R]`. Three ways to get it
there were probed (round 5, both compilers):

| Design | Surface | 2.13.18 | 3.8.3 | Findings |
|---|---|---|---|---|
| **D1: explicit DSL parameter** | `grammar[Addr, IO] { g => import g._; … }`. `g: Dsl[IO]` provides `nonTerminal`, `terminal` and `all`. `all(…) { … }` requires `IO[R]`; `all(…).pure { … }` takes a plain `R`. | ✓ | ✓ | Forgetting `IO`: "found Addr, required IO[Addr]". Wrong inner type: "found String, required Addr". Both point at the action body. Mixed pure and effectful alternatives in one production, and `IO(Nil)` for a `List[Int]` non-terminal, type-check. The cost is the `g => import g._` line. |
| **D2: context function (Scala 3 only)** | Exactly `grammar[Addr, IO] { … }`. The body is `GDsl { type F[x] = IO[x] } ?=> NonTerminal[Addr]`, and a top-level `all(…)(using d: GDsl): All2[d.F, …]`. | n/a | ✓ | Same checks and the same mixed-alternative support. The error reads "Required: contextual$1.F[Addr]", which is accurate but prints the context parameter's path. That's worth polishing, e.g. via a named `given` or type aliasing. |
| **D3: evidence on `::=`, macro checks `F`** | Exactly `grammar[Addr, IO] { … }` on both. `::=[X](alts: Alt[X])(implicit ev: Produces[X, A])` with instances `Produces[A, A]` (pure) and `Produces[G[A], A]` (effectful). | ✓ | ✓ | Simple cases work, including a custom `@implicitNotFound` message. **But:** (1) the typer accepts *any* `G[A]`, so a wrong effect (`Other[Addr]`) is caught only by the macro; (2) **mixing pure and effectful alternatives fails**, because `\|\|` merges them into `Object` (2.13) or `IO[Nil.type] \| List[Int]` (3) before `::=` sees them; (3) making `Produces` contravariant to fix (2) makes `Produces[A, A]` vs `Produces[G[A], A]` ambiguous. **Rejected.** |

**Decision (2026-09-29): D1 is the single surface on both 2.13 and 3.**
- The same user code compiles on both compilers, with compiler-enforced `F[A]`.
- The `g =>` parameter is another phantom that the macro erases.
- D2 is not planned. It could only ever be a Scala-3-only convenience, and one syntax everywhere is
  preferred.
- Actions default to effectful (`F[R]`, as requested). `.pure { … }` marks the pure fast path
  explicitly. This works because the builder variant of `all` has room for a second method.
- For `F = Id`, both forms coincide, and the macro treats every action as pure.

**Effects in actions** (R1, now concrete). With `grammar[Result, F]`, the form of each alternative
(`{ … }` vs `.pure { … }`) tells the macro which case applies:

| Action returns | Generated reduce code |
|---|---|
| `B` (pure) | Inline, in the tight loop. No `F` involved. |
| `F[B]` with `ParserEffect[F]` | Two options, to be decided in the spike: **(a) staged:** the value stack holds `F[B]` values combined with `map2`, so the whole parse yields one `F[Program]` and no effects run during parsing; **(b) interleaved:** the machine suspends at that reduction, the driver `flatMap`s/`tailRecM`s in `F`, and it resumes. |

- Option (a) fits "build an `F[Program]`". Option (b) fits compilers that update symbol tables or
  REPLs that evaluate as they go.
- In both options, pure productions never touch `F`. That is the "inlined parser with effects" that no
  combinator library can offer, because they cannot see which actions are pure.

**LR-specific trade-offs to accept**
- Actions run only at reductions. Mid-rule actions are desugared into ε-non-terminals, as yacc does,
  and can introduce conflicts that the diagnostics will explain.
- Error messages are state-based. Mitigations: expected-terminal lists built from `val` names, Menhir
  `.messages`-style custom messages keyed by example inputs, and CPCT+ repair for recovery.
- Indentation-sensitive languages work through the layout scanner (§4.2), which feeds INDENT/DEDENT
  terminals. The expected-set hook maps naturally onto LR states.
- Markdown stays an escape hatch (§4.3). The block phase can be a hand-written driver that calls
  small LR sub-grammars for inline content.
- Combinator-style rules (§5.6) can still appear as symbols in productions. They are compiled as
  sub-parsers, or inlined when they are regular (token-like).


### 5.9 DSL limitations and workarounds (probe round 6)

This round uses a stub DSL with `all` arities 1–22 (`T1…T22`), the D1 surface, and `-feature
-deprecation -Xlint` on 2.13. It was compiled with scalac 2.13.18 and 3.8.3.

**Works on both compilers.** Everything in this snippet type-checks unchanged:

```scala
grammar[Expr, IO] { g => import g._
  val expr = nonTerminal[Expr]; val num = terminal("[0-9]+").map(_.toInt); val ident = terminal("[a-z]+")
  left("+", "-"); left("*", "/"); right("^")                                  // yacc %left / %right
  expr ::= (
       all(expr, "+" || "-", expr).pure { (l, op, r) => Bin(op, l, r) }       // inline grouping = anonymous sub-production
    || all("-", expr).prec("^").pure { (_, e) => Neg(e) }                     // %prec
    || all(ident, "(", sepBy(expr, ","), ")").pure { (f, _, as, _) => Call(f, as) }  // EBNF helper
    || all(num).pure(Num.apply)                                               // method reference instead of a lambda
    || all(opt("+"), num).pure { (_, n) => Num(n) }
  )
  def parens[A](x: Sym[A]): Sym[A] = all("(", x, ")").pure { (_, a, _) => a } // local generic helper = template
  expr ::= all(parens(expr)).pure(e => e)
  expr
}
```

**Limitations found, with workarounds**

| # | Limitation | Where | Workaround / library response |
|---|---|---|---|
| L1 | At most **22 symbols** per `all(…)`. With 23, 2.13 dumps all 22 overload signatures. | Both (the Function22 limit on 2.13; the stub stops at 22 on 3) | A varargs fallback overload `all(syms: Sym[Any]*): TooLong` whose methods are `@compileTimeOnly`. The 2.13 error then shrinks to "functions may not have more than 22 parameters, but 23 given". Scala 3 still says "Missing parameter type", so document the limit. Users split long sequences with `group(…)` or helper non-terminals, which the macro inlines, so there is no runtime cost. |
| L2 | A leading `\|\|` on a new line | 2.13 | Parenthesised alternatives `( … \|\| … )` (round 2) |
| L3 | Juxtaposed tuple actions give misleading errors | 3 | `all(…)` builders (round 2) |
| L4 | Singleton literal types widen inside `all` (`s: String`, not `"Sr."`) | Both | Static type only; the macro still constant-folds (round 4) |
| L5 | Pure and effectful alternatives can't share a `::=`-level evidence check | Both | `{ … }` vs `.pure { … }` on the builder (round 5) |
| L6 | `Singleton` bound syntax: `&` doesn't exist on 2.13, and `with` is deprecated on 3 | Both | Version-specific source for the few signatures that need it (shared terminals only) |
| L7 | Declarations must precede the productions that use them | Both | Enforced by the compiler's forward-reference check (round 2) |
| L8 | `import g._` wildcard-imports DSL names (`all`, `opt`, `rep`, `left`, …) into the block and can shadow user names | Both | Keep the DSL vocabulary small and distinctive. Users can write `g.all(…)` or rename on import. Scala's shadowing rules make any clash a compile error, not silent misbehaviour. |
| L9 | Implicit conversions from `String`/`Regex` are in scope for the whole block, including action bodies | Both | Harmless in practice: conversions only fire when a `Sym`/`Alt` is expected. The macro additionally rejects DSL values that leak into actions (L10). |

**Accepted by the typer but must be rejected by the macro, with positioned errors:**

| # | Code | Why | Macro error |
|---|---|---|---|
| L10 | An action that references a phantom (`{ x => n0.toString }`, or capturing `expr` in a closure) | Phantoms don't exist at runtime | "grammar symbols can only be used in productions, not inside actions" |
| L11 | A non-literal pattern: `terminal(pattern)`, `all(s.r)` | The lexer is built at compile time | "terminal patterns must be literals" (shared external terminals: §5.6) |
| L12 | Control flow around productions: `if (…) n ::= …`, loops, productions inside actions or lambdas | The grammar must be static | "productions must be top-level statements of the grammar block" |
| L13 | A local helper `def` that is recursive or depends on runtime values | Templates are expanded at compile time (Menhir-style) | "grammar helper `parens` must be non-recursive and take only symbols"; the termination check from §5.6 |
| L14 | An opaque action value (`val f = …; all(a).pure(f)`) | Works, but the macro can't see which parameters are used | Allowed. Dead-value elimination (round 4) is skipped for that alternative, and a debug-build note says so. |
| L15 | Unsupported regex features (backreferences, look-around) | The lexer is a DFA | "regex feature X is not supported in terminals" |
| L16 | A non-terminal declared but never defined, or defined but unreachable | Grammar hygiene | Error or warning with the declaration's position |

**Compile-time cost of type-checking the DSL.** These are cold, one-shot JVM runs of a generated
grammar mixing arities 1–6, literals and lambdas, *before* any macro work:

| Grammar | 2.13.18 | 3.8.3 |
|---|---|---|
| Stub DSL only (baseline) | 3.2 s | 4.3 s |
| 100 productions (~200 alternatives) | 5.3 s | 5.7 s |
| 300 productions (~600 alternatives, 1,500 lines) | 7.1 s | 7.3 s |

About 3–4 s per 300 productions on a cold JVM, which is modest. Warm sbt servers should be much
faster (not measured). LR construction and code generation in the macro come on top; spike 6 must
measure those.


### 5.10 Executing `F`: tail-call elimination vs a `ParserRuntime[F]`

> **See §5.11:** the engine is generated per runtime by compile-time providers. The runtime
> `ParserRuntime[F]` described here is the fallback provider; the analysis of costs and binds still holds.

**Answer to "tail-rec elimination in the macro or a `Runtime[F]`?": neither alone.**

**Recursion never reaches `F` or the JVM stack.** The LR `while (true) (state: @switch) match { … }`
loop over an `Array` state stack is the **defunctionalised form of recursive-ascent parsing**:
- the state stack is the return-continuation stack;
- the value stack holds those continuations' free variables.

References [not re-checked]: Reynolds 1972; Danvy & Nielsen, *Defunctionalization at Work*, 2001;
Pennello 1986; Roberts 1988; Sperber & Thiemann, TOPLAS 2000.

Tail-call elimination would not help anyway:
- scalac only removes *self* tail calls;
- generated per-non-terminal code is *mutually* recursive, with depth equal to the input's nesting;
- trampolining it just moves continuations onto the heap. That is exactly what fs2-data's JSON
  tokenizer does with `Pull` for nesting.

**What is left for `F`: driving a resumable machine.** The macro generates a mutable machine:

```scala
final class Machine /* generated per grammar */ {
  var states: Array[Int]; var values: Array[AnyRef]; var sp: Int      // (+ primitive value stacks, §5.8)
  var buf: Array[Byte]; var pos: Int; var lim: Int; var eof: Boolean   // per InputStrategy, §5.7
  var pending: AnyRef                    // the F[R] of an effectful action awaiting its result
  def feed(chunk: Array[Byte], off: Int, len: Int): Unit
  def endOfInput(): Unit
  def resume(v: AnyRef): Unit            // push the effect's result, take the goto
  def run(budget: Int): Int              // Signal: NeedInput | Done | Error | Effect | Yield; allocation-free
}
```

`run` executes shifts and reductions until one of these happens:
- it needs input;
- it finishes or fails;
- it has used up `budget` steps (returns `Yield`);
- it hits an **effectful** reduction: the action is called, its `F[R]` is stored in `pending`, and it
  returns `Effect`.

Pure reductions (`.pure { … }`) are inlined and never leave the loop.

**Codegen specialisation on the statically known `F`.** This is kyo-compat's "lower at the call site,
no type class dispatch" idea. kyo-compat does it with Scala 3 `inline`; here it is done by the macro,
so it works on 2.13 too.

| `F` | Generated code | `F` operations per parse |
|---|---|---|
| `Id`, or a direct-style marker (Ox / Loom / Gears) | Effectful actions are called inline like pure ones; refills are inline `is.read(buf)` | **0** |
| Either-like (`Either[E, *]`, `Try`, …) | `action(…) match { case Left(e) => fail(e); case Right(v) => push(v) }` inline | **0** |
| Suspending (`IO`, `ZIO`, `Future`, Kyo `<`, generic `Sync`/`Async`) | Signals, driven by a `ParserRuntime[F]` | **1 bind per effectful action (irreducible)**, plus 1 per `budget` slice, plus 1 per refill |

**The irreducible cost, and what can be avoided.** This refines the table above.

- **Floor: one bind per effectful action.** An action returning `F[R]` whose `R` is consumed by a later
  action forces one `flatMap`/`map2` on that value. Effects that print, write or emit must run, and
  run in order.
  - With a concrete `F` known to the macro (`grammar[R, IO]`), the bind is a direct `IO#flatMap`.
  - With an abstract `F[_]: Sync` (tagless-final code), it is a `Sync[F].flatMap` dictionary call. The
    macro can't specialise that away, but it costs the same bind count.
- **What we avoid:**
  1. **Binds for pure actions:** `.pure { … }` never touches `F`.
  2. **Binds per token or shift:** the lexer and shifts are always pure.
  3. **`IOLocal`/`Ref` stages** for parser state.
  4. **Extra resume stages.** The driver doesn't need `flatMap(pending)(v => delay { resume; run })`.
     It runs the machine's next pure segment **directly inside the continuation of that one bind**:
     `flatMap(pending)(v => { m.resume(v); m.run(budget) match { … } })`.
     - That is safe for cats-effect and ZIO: the continuation runs only when the runtime evaluates it,
       and non-fatal exceptions thrown there are captured as errors.
     - So an effectful action costs exactly **one** bind. The only other stages are one per `budget`
       slice (`cede`) and one per blocking refill.
- **Sync points instead of suspension (optimisation, not a bind reduction).** In LR, actions never
  influence parse decisions. So the machine doesn't have to *stop* at an effectful reduction.
  - It can push the unevaluated `F[R]` onto the value stack. The parent reduction then combines its
    children's `F`s (`flatMap`/`map2`, the same bind count) and the parser keeps going.
  - The accumulated `F` is only *run* at a **sync point**:
    - the end of a user-marked non-terminal, e.g. a REPL `statement` or a streamed JSON array element;
    - the end of the `budget` slice;
    - the end of input.
  - This removes machine suspend/resume round-trips. Memory is bounded by the work between sync
    points, which is why sync points are needed for 4 GB streams and REPLs. Without any, this becomes
    the "build one `F[Program]`" staged mode.
- **Multi-shot `F` breaks the mutable machine.** The resumable machine is mutable, so it assumes `F`
  calls each `flatMap` continuation **at most once**. That holds for `IO`, `ZIO`, `Future`, `Eval`,
  `Id`, `Either`, `Option` and `Try`.
  - For multi-shot monads (`List`, `LogicT`-style backtracking, `fs2.Stream` used as a monad), a
    continuation re-entered twice would corrupt shared state.
  - Such `F`s need a persistent or copy-on-resume stack (Menhir's checkpoints are persistent for this
    reason), or must be rejected.
  - The macro can reject known multi-shot `F`s. `ParserRuntime` instances declare `singleShot = true`.

**`ParserRuntime[F]`** is the only place a suspending `F` appears:

```scala
trait ParserRuntime[F[_]] {
  def delay[A](a: => A): F[A]                           // one pure segment: m.run(budget)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]       // must be stack safe (all targets below are)
  def pure[A](a: A): F[A]
  def raise[A](e: Throwable): F[A]
  def cede: F[Unit]                                     // IO.cede / ZIO.yieldNow / unit
  def readBlocking(is: java.io.InputStream, buf: Array[Byte]): F[Int]   // JVM/Native only
  def budget: Int                                       // pure steps per segment
  // optional fast path: def loop(m: Machine): F[Result] (ZIO.whileLoop, Kyo Loop)
}
```

The default driver is written once:
- `delay(m.run(budget))`, then:
  - `Effect`: `flatMap(pending)(v => { m.resume(v); <next segment inline> })`, i.e. one bind with
    no extra `delay` stage;
  - `Yield`: `cede` and again;
  - `NeedInput`: `readBlocking` and again.
- The machine itself is allocated **inside** `delay`, so every run gets a fresh one and the parser
  value stays referentially transparent. jawn-fs2 allocates its `AsyncParser` at pipe construction,
  outside `F`, which shares mutable state if the stream is re-run.

**Why not `IOLocal` + recursive `IO`** (evidence from cats-effect 3.7.1 source):
- **`IOLocal` is slow for this.** It is an immutable `Map` stored in the fiber, so every `get`/`set`
  costs an extra `IO` stage plus a map operation, and its copy-to-child semantics aren't needed. A
  mutable machine created inside `delay` is already confined to one fiber. The sequential driver orders
  the stages, even across thread hops. `FiberRef` in ZIO is the same story.
- **Per-reduction `flatMap` would dominate.** CE's own tuning doc puts a `flatMap` at about 10 ns.
  Third-party benchmarks measure about 8–13 ns and about 96 B per bind for CE and ZIO. A shift or reduce
  in the array loop should cost a few ns [to measure]. One `IO` stage per reduction would cost several
  times the parse itself.
- **One huge `delay` is unfair.** CE auto-yields every 1024 stages (cancellation checks every 512), but
  never *inside* a `delay`. Its guidance: work longer than about 10 µs is "expensive", so split it and
  `cede`. Hence `budget`, with a target of roughly 10–50 µs per segment, i.e. a few thousand steps (to
  tune). ZIO auto-yields every 10,240 ops; Kyo preempts on time slices.

**Instances**

| Instance | Scala | Notes |
|---|---|---|
| `Id` / direct style | 2.13 + 3 | Inline codegen, `budget = ∞`. For Ox, document `computeIntensive` for huge inputs, which moves CPU-bound work off the virtual-thread carriers. |
| `Either` / `Try` | 2.13 + 3 | Inline codegen |
| cats-effect generic `Sync[F]` / `Async[F]` | 2.13 + 3, JVM/JS/Native | `blocking` for reads, **not** `interruptible`: `FileInputStream` ignores interrupts (CE FAQ). `cede` needs `Async`/`GenSpawn`; with plain `Sync` it is `unit`. |
| cats-effect `IO`, specialised | 2.13 + 3 | `IO.cede`, `IO.blocking`. After `blocking`, the following pure stage usually stays on the same thread until the next yield (CE thread-model doc), so refill and parse don't ping-pong between pools. |
| ZIO, native | 2.13 + 3 | `loop` via **`ZIO.whileLoop`**, a run-loop-level while node documented as the performance constructor. Plus `yieldNow` and `attemptBlocking`. `interop-cats` only as a fallback. |
| Kyo | 3 only | `Sync.defer` + `kyo.kernel.Loop` (up to 4 loop values without tuple allocation) |
| `Future` | 2.13 + 3 | Lazy wrapper (`() => Future`), `cede` via the execution context |
| fs2 | 2.13 + 3 | Not through `ParserRuntime`. A `Pipe[F, Byte, A]` via `Pull.uncons`: per chunk, `feed` + `run`, with effectful reductions via the runtime. This is fs2-data's and jawn-fs2's shape, but with an explicit LR stack instead of `Pull`-nested continuations. |

- **Scala.js:** single-threaded, with no blocking reads. The only input APIs are in-memory and the
  push `step(chunk)`. `budget` + yield matters more there, because a long synchronous parse freezes
  the event loop.
- **Scala Native 0.5:** multithreaded, so `blocking` is meaningful.
- **Ox and Kyo instances live in Scala-3-only modules.** Ox is also JVM-21-only.

**Benchmarks that decide the parameters (spike 7)**
1. Effect density: 0 %, 1 %, 10 % and 100 % effectful reductions, on CE `IO` and ZIO. This measures
   the batching win and the cost per effect.
2. `budget` sweep (256 / 1k / 4k / 16k / ∞): throughput versus the latency of a concurrent "ping"
   fiber, plus the CE starvation checker.
3. Parser state held in a captured mutable machine vs `IOLocal`/`FiberRef` vs `Ref`.
4. Driver shape: recursive `flatMap` vs `tailRecM` vs `ZIO.whileLoop` vs Kyo `Loop`.
5. Refill: `blocking` read sizes (8 / 64 / 256 KiB) vs `delay`; the fs2 pipe vs `InputStream` +
   `blocking`.
6. Baselines: `Id` codegen vs fs2-data JSON and jawn `AsyncParser` on the same input.


### 5.11 `grammar[Result, F]` generates the whole engine in `F`: per-runtime codegen providers

**Correction to the framing of §5.10.** `grammar[Result, F]` does not produce a pure parser that is
then wrapped in `F`. It defines `F[Result]` **and the entire engine for the intermediate steps**, all
generated for `F`:
- entering the parse;
- the pure segments;
- how each effectful action's `F[R]` is continued;
- yield points;
- input acquisition (reads and refills);
- error raising;
- sync points.

The optimisation strategy is therefore chosen **at compile time, per runtime**. A runtime type class
consulted by a generic driver is only the fallback.

**Extension point.** A `ParserEngineProvider` is a Hearth `StandardMacroExtension`, discovered from the
classpath the same way `cats-integration` registers `IsEither`/collection providers (e.g.
`IsEitherProviderForValidated`) and loaded once per expansion (`ensureStandardExtensionsLoaded()`).
The core module knows nothing about cats-effect or ZIO. Adding
`kindlings-parser-cats-effect` or `kindlings-parser-zio` to the classpath adds their engines.

Compile-time interface (sketch; every hook returns Hearth `Expr`s the macro splices):

```scala
trait ParserEngineProvider {
  def mightMatch[F[_]](F: Type.Ctor1[F]): Boolean     // cheap gate (hearth#347 pattern)
  def entry[R](body: Expr[F[R]]): Expr[F[R]]          // IO.defer / ZIO.suspendSucceed / identity
  def continueWith[A, R](fa: Expr[F[A]])(k: Expr[A] => Expr[F[R]]): Expr[F[R]]
                                                      // IO#flatMap / ZIO#flatMap / Either match / direct call
  def succeed[R](r: Expr[R]): Expr[F[R]]
  def fail[R](e: Expr[ParseError]): Expr[F[R]]
  def yieldThen[R](k: Expr[F[R]]): Option[Expr[F[R]]] // IO.cede *> k / ZIO.yieldNow *> k / None (Id)
  def blockingRead(read: Expr[Int]): Option[Expr[F[Int]]] // IO.blocking / ZIO.attemptBlocking / None = direct
  def loopShape: LoopShape                            // RecursiveBind | RuntimeWhileLoop (ZIO.whileLoop) | KyoLoop | Inline
  def singleShot: Boolean; def defaultBudget: Int; def eager: Boolean
}
```

**What specialisation buys over the generic driver.** With a concrete provider, the macro doesn't need
the `Signal`/`pending: AnyRef` round-trip of §5.10. It generates the loop *inside* the engine, and each
**effectful production gets its own inlined continuation**:

```scala
// sketch of what the IO provider makes the macro emit for one effectful production
IO.defer {
  val m = new Machine(...)
  def go(): IO[Result] = {
    while (true) (m.state: @switch) match {
      // ... pure shifts/reductions inline ...
      case 42 =>                                    // reduce: addr ::= all(name, zip) { (n, z) => IO(Addr(n, z)) }
        val z = m.popInt(); val n = m.popRef[String]()
        return IO(Addr(n, z)).flatMap { r => m.pushRef(r); m.goto(7); go() }   // the one irreducible bind
      case BudgetExhausted => return IO.cede *> go()
      case NeedInput       => return IO.blocking(m.refill()).flatMap(_ => go())
    }
  }
  go()
}
```

- Every bind is a monomorphic `IO#flatMap`: no dictionary, no boxing through `pending`, no cast.
- Recursion through `flatMap` is stack safe because `IO` is.
- The action body is beta-reduced in place. Pure productions stay in the `while`.

**Providers planned**

| Provider | Matches | Engine shape |
|---|---|---|
| `IdEngine` (core) | `Id`, plus direct-style markers (Ox, Loom, Gears) | One `while` loop, effectful actions called inline, direct reads. Zero `F` operations. |
| `EagerErrorEngine` (core) | `Either[E, *]`, `Try`, `Option`, `Validated`-like via `IsEither` | Inline `match`, early return on error. Zero binds. |
| `CatsEffectIOEngine` (cats-effect module) | concrete `cats.effect.IO` | The sketch above: `IO.defer`, `IO#flatMap`, `IO.cede` per budget, `IO.blocking` (not `interruptible`). |
| `CatsEffectGenericEngine` (cats-effect module) | abstract `F[_]` with a summonable `Async[F]` / `Sync[F]` | The same shape through `F.flatMap` / `F.cede` / `F.blocking` dictionary calls. Needed for tagless-final code: the bind count is the same, the dispatch is not. |
| `ZioEngine` (zio module) | `ZIO[R, E, *]` (via aliases like `Task`/`RIO`/`IO`) | `ZIO.suspendSucceed`, `ZIO.whileLoop` as the loop node, `ZIO.yieldNow`, `ZIO.attemptBlocking`. The error channel `E` maps parse errors via a user function. |
| `KyoEngine` (Scala 3 module) | `A < S` | `Sync.defer`, `kyo.kernel.Loop`, `Async` at effects |
| `FutureEngine` (core) | `scala.concurrent.Future` | Kept lazy as `() => Future`; binds via `flatMap` on the implicit `ExecutionContext` |
| `RuntimeInstanceEngine` (core fallback) | any `F` with a user-provided `ParserRuntime[F]` value (§5.10) | The generic signal-based driver |

Selection:
1. Concrete type providers are tried first (`IO`, `ZIO`, `Kyo`, `Id`, `Either`…).
2. Then type-class providers, by summoning `Async`/`Sync`.
3. Then the runtime-instance fallback.
4. With no provider, a compile error lists the providers found on the classpath and the modules that
   would add one.

The chosen provider appears in the derivation log (`LogDerivation`-style), so users can see which engine
their grammar got.

**Consequences**
- The runtime `ParserRuntime[F]` of §5.10 is demoted to the fallback. The primary mechanism is
  compile-time providers.
- Benchmarks (spike 7) are per provider. Each provider owns its `defaultBudget`, loop shape and sync-point
  strategy (e.g. `*>`-accumulation for IO, `ZIO.whileLoop` batching for ZIO, none for `Id`).
- Parser *reuse*: `grammar[Result, F]` is typically assigned to a `val`. The engine value is immutable,
  and each `parse(input): F[Result]` allocates its machine inside `entry`, so it is referentially
  transparent and safe to run concurrently.
- Providers must declare `singleShot = true` (§5.10). The macro refuses to pair the mutable machine with
  a multi-shot `F`.

## 6. Decisions needed before prototyping

1. **Grammar class.** *Resolved (2026-09-29):* a yacc-style BNF front end with LR(1) (IELR/LALR) as the
   core (§5.8). Still open:
   - IELR(1) vs LALR(1) as the default;
   - whether an LL(1)/combinator front end ships in v1 or later;
   - effect mode: *resolved in §5.10* in favour of interleaved execution through a resumable machine
     plus `ParserRuntime[F]`, with codegen specialisation for `Id`/direct-style and Either-like `F`.
     The staged "build one `F[Program]`" mode needs no special support: it is just actions whose
     `F` is lazy (e.g. `Eval`, or a free structure);
   - the builder `all(…)` + `apply(f)` / `.pure(f)` is now effectively required by effect typing
     (round 5); it also leaves room for per-alternative modifiers (`prec`, labels);
   - ~~effect-typing surface~~ *decided:* D1, `grammar[Result, F] { g => import g._; … }`, the same
     syntax on 2.13 and 3;
   - the multi-line alternative style. Probes favour a parenthesised `( … || … )` with leading `||`,
     which works on both compilers; `oneOf(…)` is an alternative.
2. **Composition model.** Accept the literal-type carrier (§5.6) as the composition mechanism? It
   implies these user-visible rules:
   - rules must not be type-ascribed (use `rule.fix` or a `grammar { … }` block for recursion);
   - nullability shows up as `Rule0` vs `Rule`;
   - a final `Parser.compile(entry)` is where global errors appear.
3. **Semantic of `Something[F]`.** Applicative-only actions (static, analysable, fast), or should
   actions be able to influence parsing, e.g. a C typedef table that changes tokenisation? The latter
   needs a controlled parser→lexer feedback channel, not monadic `flatMap`.
4. **Output modes.** Should recognise/event/build all be first-class? A 4 GB JSON rules out `build` in
   practice.
5. **Input encoding.** Bytes (UTF-8) as the primary input with a `String` fast path, or chars first?
   Bytes are what 4 GB `InputStream` inputs are; chars are what REPLs have.
6. **Scope of v1 recovery.** Is REPL "incomplete input" plus good error messages enough, or is IDE-grade
   error recovery required?
7. **Span contract for streams.** For stream input, should captured text be materialised at commit
   (copy-on-capture: bounded memory, simple), or should large spans pin their chunk (Okio-style
   share-large: fewer copies, less predictable memory)? And should the event mode's "valid until next
   step" views be exposed to users at all?
8. **Minimum JDK for the mmap strategy.** JDK 22+ `MemorySegment` only, or also the
   `MappedByteBuffer`-window fallback?

## 7. Proposed next steps (still research/spikes)

1. **Hearth spike (carrier and visibility):**
   - parse a small multi-rule grammar block (option A) with `DestructuredExpr` on 2.13 and 3;
   - have a `rule { … }` macro infer `Rule[A] { type Meta = "…" }` and read it back from another
     module/jar on both compilers;
   - measure the maximum literal length;
   - confirm Zinc recompiles the link site after a leaf edit.
2. **Analysis spike:** implement nullable/FIRST/FOLLOW plus asp-style typing on a pure IR (no macros).
   Test it on JSON, an expression language, and a Python-ish layout grammar, and look at what the
   diagnostics look like.
3. **Codegen spike:** a hand-written version of the code the macro would emit for JSON (fused lexer,
   `while` loop, `Array[Int]` stack, `Long` offsets, chunked `InputStream`). JMH it against
   jsoniter-scala, fastparse 3.1.1, parboiled2 2.5.1, cats-parse 1.1.0 and Parsley 4.6/5.0-M on
   1 MB / 1 GB / 4 GB inputs, including 100k-deep nesting. This sets the performance target before
   any macro work.
4. **Effect spike:** sketch `Something[F]` as an applicative-plus-errors type class. Measure `Id`
   specialisation vs dictionary passing vs a free-applicative interpretation.
5. **Input spike:**
   - implement the three strategies from §5.7 by hand;
   - run the same hand-written JSON recogniser over each;
   - check with a heap profiler that a 4 GB stream stays within the max window;
   - confirm that the `String` strategy allocates nothing but results;
   - compare "specialised per strategy" against "one `CharSequence` loop" to quantify the
     megamorphic cost.
6. **BNF macro spike (§5.8):**
   - a Hearth macro that walks a `grammar(start) { … }` block on 2.13 and 3;
   - build LALR(1) for a JSON grammar and an expression grammar with `left`/`right` precedence;
   - emit the `while`-loop parser with spliced actions;
   - show a shift/reduce conflict report pointing at the two `::=` sites;
   - include the syntax probes from §5.8 as compile tests (positive, and negative with expected
     error text);
   - prototype dead-value elimination: unused lambda parameters not synthesised, and dead
     non-terminals compiled recognise-only. Measure allocation on a JSON grammar that ignores most
     values.
7. **Runtime spike (§5.10, §5.11):**
   - implement `IdEngine` and `CatsEffectIOEngine` as `ParserEngineProvider`s and compare their generated
     code against the generic signal-based driver;
   - hand-write the `Machine` for the JSON grammar with 0 / 1 / 10 / 100 % effectful reductions;
   - run the six benchmarks listed in §5.10 on JVM, JS and Native;
   - fix the default `budget` per runtime and choose the driver shape per instance.

## 8. Glossary of the less common terms

- **FLAST:** the tokens that can follow the *last* token of a non-empty word of an expression. Used for
  the separability check.
- **Selective functor:** between applicative and monad. It allows choosing among *statically known*
  branches based on a result.
- **Counterexample (unifying):** one input with two parse trees, which proves ambiguity.
- **Cut:** a commit point after which no backtracking is possible, so the input before it can be
  discarded.
- **Low-water mark:** the lowest input offset any live mark, backtrack point or open capture can still
  return to. Everything below it can be evicted.
- **Carrier:** the part of a rule's static type that encodes its serialised IR and summary for later
  macros.

## 9. Sources

**Scala libraries (read in source)**
- Parsley: https://github.com/j-mie6/parsley
  - `internal/README.md`, `machine/Context.scala`, `stacks/CallStack.scala`,
    `backend/IterativeEmbedding.scala`
  - Dill: https://github.com/j-mie6/parsley-debug-app
- parboiled2: https://github.com/sirthias/parboiled2 (`ParserMacros.scala`, `Parser.scala` `__run`
  phases, `README.rst`)
- fastparse: https://github.com/com-lihaoyi/fastparse (`ParserInput.scala`,
  `readme/FastParseInternals.scalatex` "Stack-Limited", `readme/StreamingParsing.scalatex`)
- cats-parse: https://github.com/typelevel/cats-parse (`Parser.scala`, `project/Dependencies.scala`)
- scala-parser-combinators: https://github.com/scala/scala-parser-combinators (`PackratParsers.scala`)
- atto: https://github.com/tpolecat/atto
- zio-parser: https://github.com/zio/zio-parser (`internal/stacksafe`)
- Scallion: https://github.com/epfl-lara/scallion (`Parsing.scala` conflicts)
- gll-combinators: https://github.com/djspiewak/gll-combinators
- Meerkat: https://ir.cwi.nl/pub/25145/25145.pdf
- parsley-garnish report:
  https://www.imperial.ac.uk/media/imperial-college/faculty-of-engineering/computing/public/distinguished-projects/2324-ug-projects/parsley-garnish-report-rocco-jiang-final.pdf
- Old benchmarks: https://github.com/tom91136/scala-parser-benchmarks

**Papers and manuals**
- Staged and compiled combinators:
  - Krishnaswami & Yallop, *A Typed, Algebraic Approach to Parsing*, PLDI 2019.
    https://doi.org/10.1145/3314221.3314625
  - Yallop, Xie & Krishnaswami, *flap: A Deterministic Parser with Fused Lexing*, PLDI 2023.
    https://arxiv.org/abs/2304.05276
  - Willis, Wu & Pickering, *Staged Selective Parser Combinators*, ICFP 2020.
    https://doi.org/10.1145/3409002
  - Willis & Wu, *Garnishing Parsec with Parsley*, Scala 2018.
    https://dl.acm.org/doi/10.1145/3241653.3241656
  - Willis & Wu, *Design Patterns for Parser Combinators*, Haskell 2021.
    https://dl.acm.org/doi/10.1145/3471874.3472984
  - Jonnalagedda et al., *Staged Parser Combinators for Efficient Data Processing*, OOPSLA 2014.
    https://dl.acm.org/doi/10.1145/2714064.2660241
- Derivatives:
  - Edelmann, Hamza & Kunčak, *Zippy LL(1) Parsing with Derivatives*, PLDI 2020.
    https://arxiv.org/abs/1911.12737
  - Darragh & Adams, *Parsing with Zippers*, ICFP 2020. https://doi.org/10.1145/3408990
- General and LR algorithms:
  - Parr, Harwell & Fisher, *Adaptive LL(\*)*, OOPSLA 2014.
    https://www.antlr.org/papers/allstar-techreport.pdf
  - Scott & Johnstone, *GLL Parsing*, ENTCS 2010.
  - Denny & Malloy, *IELR(1)*, SCP 2010.
- Conflict diagnostics and error recovery:
  - Isradisaikul & Myers, *Finding Counterexamples from Parsing Conflicts*, PLDI 2015.
    https://www.cs.cornell.edu/andru/papers/cupex/cupex.pdf
  - Bison manual (counterexamples, push parsers). https://www.gnu.org/software/bison/manual/
  - Menhir manual (`--explain`, incremental API, `.messages`).
    https://gallium.inria.fr/~fpottier/menhir/manual.html
  - Pottier, *Reachability and Error Diagnosis in LR(1) Parsers*, CC 2016.
  - Diekmann & Tratt, *Don't Panic!*, ECOOP 2020. https://arxiv.org/abs/1804.07133
- PEG:
  - Ford, *Parsing Expression Grammars*, POPL 2004 [not re-checked].
  - Redziejowski (PEG / LL(1p), PEG Explorer). https://mousepeg.sourceforge.net/
  - Mizushima, Maeda & Yamaguchi, *Packrat Parsers Can Handle Practical Grammars in Mostly Constant
    Space*, PASTE 2010.
- Ambiguity detection: Schmitz, *Conservative Ambiguity Detection*, ICALP 2007.
- Indentation-sensitive parsing:
  - Adams, *Principled Parsing for Indentation-Sensitive Languages*, POPL 2013.
  - Adams & Ağacan, *Indentation-Sensitive Parsing for Parsec*, Haskell 2014.
- Markup:
  - CommonMark spec, Appendix A. https://spec.commonmark.org/0.30/
  - pulldown-cmark block parsing. https://pulldown-cmark.github.io/pulldown-cmark/dev/block-parsing.html
- IDE-grade parsers:
  - tree-sitter external scanners.
    https://tree-sitter.github.io/tree-sitter/creating-parsers/4-external-scanners.html
  - Lezer. https://lezer.codemirror.net/docs/guide/
- Composition:
  - Grimm, *Better Extensibility through Modular Syntax* (Rats!), PLDI 2006.
    https://dl.acm.org/doi/10.1145/1133255.1133987
  - Schwerdfeger & Van Wyk, *Verifiable Composition of Deterministic Grammars*, PLDI 2009.
    https://dl.acm.org/doi/10.1145/1543135.1542499
  - Danielsson, *Total Parser Combinators*, ICFP 2010 [not re-checked].
- Zinc API extraction (constant types, annotations, inline bodies):
  - https://github.com/sbt/zinc/blob/develop/internal/compiler-bridge/src/main/scala/xsbt/ExtractAPI.scala
  - https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/sbt/ExtractAPI.scala
- Input buffers:
  - jsoniter-scala `JsonReader.scala` (`loadMore`, marks):
    https://github.com/plokhotnyuk/jsoniter-scala
  - jawn `AsyncParser`: https://github.com/typelevel/jawn
  - Jackson core (`NonBlockingJsonParser`, `TextBuffer`): https://github.com/FasterXML/jackson-core
  - Okio `Segment`: https://github.com/square/okio
  - fs2 `Chunk`: https://github.com/typelevel/fs2
  - Scala Native `String` (shared `substring`): https://github.com/scala-native/scala-native
  - V8 `SlicedString` retention: https://github.com/nodejs/node/issues/31891
  - JDK 7u6 `substring` change:
    https://nextmovesoftware.com/blog/2013/07/05/java-6-vs-java-7-when-implementation-matters/
- High-throughput JSON: Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDB J. 2019.
  https://arxiv.org/abs/1902.08318
