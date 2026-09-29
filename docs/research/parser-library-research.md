# Parser library for Kindlings: prior-art research

Status: **RESEARCH** (2026-09-29). No implementation yet. This document compares the existing Scala
parsing libraries and the relevant non-Scala prior art against the requirements below, and proposes a
direction plus the decisions that need to be made before a prototype.

How the facts were gathered:

- **Scala libraries:** read from source (shallow clones of each repository), with versions taken from
  git tags and Maven metadata.
- **Algorithms and papers:** checked against the papers, manuals and repositories cited in
  [§9 Sources](#9-sources).

Nothing was benchmarked or run. The stack-safety claims come from reading the code. Unverified claims
are marked **[unverified]**.

## 0. Requirements (as stated) and how they are read here

| # | Requirement | What it implies technically |
|---|---|---|
| R1 | User-chosen effect `F[_]`, with a type class `Something[F]` that the parser uses to produce `F[Program]`. `F` can be hard-coded or passed in. | Semantic actions are abstracted over `F`. The macro must specialise when `F` is known (e.g. `Id`), because a per-token `flatMap` is far too slow for R2. |
| R2 | Minimal overhead, so a 4 GB JSON is parseable. Stack safe, using a mutable heap stack rather than the JVM thread stack. | `Long` offsets, streaming input with a bounded buffer, and no unbounded backtracking. An explicit `Array`-backed state/value stack. No allocation per token on the hot path. A mode that does not build a full AST, since a 4 GB AST does not fit in a normal heap. |
| R3 | Both compilers and REPLs; input from `String` and `InputStream`. | A push/resumable parser (feed chunk → `NeedInput` / `Done` / `Error`). Exact "incomplete input" detection. Error recovery is desirable. |
| R4 | Built-in debugging: runtime tracing, plus compile-time detection of unintended loops, ambiguities and shift/reduce conflicts. | The **whole grammar must be visible to a macro as data**. Nullable/FIRST/FOLLOW analysis, left-recursion and nullable-loop checks, LL or LR conflict reporting with counterexamples, and PEG-shadowing checks. The tracer is compiled in only when requested. |
| R5 | Easy to write algol-like, indentation-based, Markdown/markup and data-language grammars. | Precedence/Pratt support, a lexer layer, a layout (INDENT/DEDENT) scanner hook, and line-oriented/stateful escape hatches for CommonMark-class languages. |

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
- **Recommended shape:**
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
  - Each `rule { … }` expands to a value whose *type or annotation* carries a serialised grammar IR,
    e.g. a string literal in an annotation or a singleton literal type.
  - `Grammar.compile(rootRule)` collects and links the IR of every rule it reaches, including rules
    from other compilation units or libraries.
  - This allows reusable grammar libraries, such as a standard `Json` or `Expr` module.
  - Needs a Hearth spike: reading annotations and literal types of referenced symbols
    cross-platform [unverified].
- **C. Grammar as an ADT value, analysed at run time** (the Parsley model).
  - Portable, but loses compile-time diagnostics (R4). Rejected as the primary path, but useful as a
    test oracle and REPL-time fallback.

**Recommendation:** start with **A**, and design the IR so that B can be added later for modularity.

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

## 6. Decisions needed before prototyping

1. **Grammar class.** Is "LL(1) + precedence + layout, with LR(1) later" acceptable? Or must the first
   version accept arbitrary PEG with backtracking, like fastparse and parboiled2? PEG costs streaming
   guarantees and diagnostic precision. If "shift/reduce" diagnostics are wanted literally, the LR
   back end moves from "later" to "required".
2. **Grammar visibility.** Option A (one macro call, no cross-file modularity) first? Or is grammar
   modularity across files and libraries a day-one requirement (option B)?
3. **Semantic of `Something[F]`.** Applicative-only actions (static, analysable, fast), or should
   actions be able to influence parsing, e.g. a C typedef table that changes tokenisation? The latter
   needs a controlled parser→lexer feedback channel, not monadic `flatMap`.
4. **Output modes.** Should recognise/event/build all be first-class? A 4 GB JSON rules out `build` in
   practice.
5. **Input encoding.** Bytes (UTF-8) as the primary input with a `String` fast path, or chars first?
   Bytes are what 4 GB `InputStream` inputs are; chars are what REPLs have.
6. **Scope of v1 recovery.** Is REPL "incomplete input" plus good error messages enough, or is IDE-grade
   error recovery required?

## 7. Proposed next steps (still research/spikes)

1. **Hearth spike (macro visibility):**
   - parse a small multi-rule grammar block (option A) with `DestructuredExpr` on 2.13 and 3;
   - check whether option B's "IR in annotation/literal type" can be read back cross-platform.
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

## 8. Glossary of the less common terms

- **FLAST:** the tokens that can follow the *last* token of a non-empty word of an expression. Used for
  the separability check.
- **Selective functor:** between applicative and monad. It allows choosing among *statically known*
  branches based on a result.
- **Counterexample (unifying):** one input with two parse trees, which proves ambiguity.
- **Cut:** a commit point after which no backtracking is possible, so the input before it can be
  discarded.

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
- High-throughput JSON: Langdale & Lemire, *Parsing Gigabytes of JSON per Second*, VLDB J. 2019.
  https://arxiv.org/abs/1902.08318
