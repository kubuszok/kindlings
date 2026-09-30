# Parser branch review

Reviewed source: `claude/charming-thompson-w26mal` at `5e3cf198`, rebased locally onto
`master` at `1815765c` as `feat/parser`. Review date: 2026-09-30.

## Verdict

**Changes requested.** The existing JVM suites pass on both Scala versions, but additional
review regressions expose three runtime issues. The streaming lexer issue is particularly
important for the fs2/Reader use cases. The supplied PR description also needs corrections.

The 43 source commits have been replayed with the repository's configured author/committer
identity and local GPG signing. Their messages contain no Claude attribution or agent-session
links. References to a benchmark's “same session” are measurements, not agent-session links.

## Runtime findings

### High: a streamed token is scanned quadratically across refills

`parser/.../internal/runtime/Machine.scala`, `lexInput` (around lines 566–608), resets
`state`, `i`, `accepted`, and `acceptedEnd` on every call. Returning `NeedInput` loses the
lexer continuation, so the next call rescans the entire token from `pos`.

The deterministic `ReviewSpec` feeds a 1,000-character `a+` token one character per refill.
It obtains the correct value but requires **500,500 character reads**, instead of linear
work. This affects Reader and pushed/fs2 inputs. Buffering the input in bounded space does
not bound the time spent repeatedly scanning it.

Requested change: retain DFA state, scan offset, and last accepted token/end across
`NeedInput`. Clear the continuation when a token is accepted or skipped. Test long tokens,
long skipped tokens, maximal-munch fallback, and different chunk boundaries. Also distinguish
the shift/reduction budget from a bound on lexer work: currently one token/skip run can
perform arbitrarily much work before a Cats Effect cede.

### Medium: incomplete lexical tokens violate the documented REPL contract

`Machine.lexError`, `lexInput`, `lexString`, and `syntaxError` classify EOF inside an
unfinished token as an unexpected character. `endOfInput` is set only for an already-lexed
EOF token with an “Unexpected token” error.

A grammar accepting `"true"` reports `endOfInput = false` for `"tru"`, for both String and
Reader inputs. Appending `"e"` would complete a valid input. This contradicts the public
`ParseError.endOfInput` documentation (“valid prefix”, usable to prompt for another line).

Requested change: preserve whether lexing failed because the input ended in a viable token
prefix. Respect the parser's expected tokens as well: an incomplete token that cannot occur
in the current parse state is not necessarily a valid language prefix. Cover generated and
table lexers, String/Reader/push inputs, unterminated strings/comments, and truly invalid
characters. Alternatively, explicitly narrow the API contract before release, rather than
claiming full valid-prefix detection.

### Medium: nonpositive budgets create non-progressing engines

`Machine.run` immediately returns `Yield` for `budget <= 0`. `CatsEffectEngine.async` and
`sync` accept these budgets and repeatedly drive the same unchanged machine. A parse then
never completes.

Requested change: require a positive budget at public engine construction and at the public
machine boundary, with regression tests that fail immediately rather than hanging.

## Hearth findings

### Confirmed upstream bug: alias-sensitive `Type.CtorN.fromUntyped`

Reported upstream: [kubuszok/hearth#384](https://github.com/kubuszok/hearth/issues/384).

The standalone probe in `parser-hearth-probes/` uses Hearth 0.4.2 and Scala 3.8.4. For
`type AtLeastTwo = List[Int] :| MinLength[2]`, the same `Ctor2.fromUntyped[IronType]`
returns no match for the original type and a match after `.dealias`:

```text
Iron alias: original=false, dealiased=true
```

This does not require the parser or the Kindlings Iron provider. In Hearth,
`project/TypeConstructorsGen.scala`, `fromUntypedImpl3`, constructs `aRepr` without
dealiasing, matches `AppliedType`, then falls back to `baseType`. For an alias of an opaque
type constructor, that fallback does not recover the application. The parser's `.dealias`
is a justified workaround, not evidence that callers should generally have to normalize
types before using providers.

Removing only the Scala 3 parser bridge's `.dealias` makes the existing shared-platform
`IronParserSpec` grammar fail to compile; restoring it makes the baseline pass. Keep this
workaround until Hearth handles the alias, and retain the integration coverage. Suggested
upstream fix: normalize aliases before matching in `CtorN.fromUntyped`, including the
constructor representation; cover Ctor1/Ctor2/higher arities, aliases of opaque applications,
and aliases in constructor arguments. Do not erase opaque boundaries with underlying-type
extraction.

### No reproduced `AnonymousInstance` / `this` bug

The current parser does **not** call `AnonymousInstance`; its bridges directly emit anonymous
`GeneratedReductions` subclasses. The standalone probe verifies that Hearth can generate:

- a fluent method returning the generated object's own `this.type`, by returning
  `OverrideContext.self` directly;
- a method calling another method through `self`.

Both compile and run with Hearth 0.4.2 on Scala 3.8.4. The parser review suite also verifies
that token conversions and actions capture an enclosing class's explicit `this` correctly
on Scala 2.13 and 3. This does not prove every protected-member/owner/sibling-splice case;
the originally observed failing example is needed before filing a specific upstream bug.

Use the documented `returnsThisType`/`self` path rather than casting `self` to the widened
parent result type for fluent overrides. Avoid replacing the working bridge with a guessed
workaround on the basis of the name `AnonymousInstance` alone.

### Real API gap: block declarations in `DestructuredExpr`

The branch already records this in `hearth-gap-destructured-local-vals.md`: local val
definitions, their symbol identity/use sites, and imports are not represented as inspectable
nodes. That is a reasonable upstream feature request and justifies compiler-specific grammar
extraction.

It does not by itself explain all compiler-specific generation: the two `GrammarMacros`
files also duplicate reduction, lexer, LL-program, and recursive-descent emission. Their
class comments saying “everything after extraction is shared” should be narrowed. Shared
plans and analyses are a good foundation; upstream expression-rebinding/code-generation
needs should be described with concrete examples, separately from the block-extraction gap.

## Repository conventions and PR-description corrections

- `CollectionCodegen` duplicates the once-only extension loader and discards the result of
  `Environment.loadStandardExtensions()`. Use the shared `LoadStandardExtensionsOnce`
  mechanism, including failure handling, as required by AGENTS.md.
- The three parser modules override MiMa settings locally. After rebasing onto current
  master, register them in `modulesWithoutMimaBaseline`, alongside the new integrations,
  so the release-baseline cleanup has one list.
- The proposed calculator example parses `1 + 2 * (3 - 1)` but has no subtraction
  production. Add it (and division, or remove its precedence declaration), or change the
  input. The full calculator in the actual codegen tests already defines these productions.
- Unreachable non-terminals are **warnings**, not compile errors (`GrammarCompiler`
  appends them to `warnings`). Say so in the PR description.
- The latest ~89%/~81% jawn figures are recorded in the research narrative, but the
  checked-in benchmark JSON files cover earlier experiments. Attach the final raw JMH
  results/environment metadata before presenting that specific measurement as independently
  reproducible evidence. This review did not rerun JMH.
- “Bounded memory” needs the qualifications already partly present in the docs: input
  chunk size, longest token, parse-stack depth, and retained action results. It is not a
  constant-space guarantee for an arbitrary grammar or result AST.

## Verification

User explicitly authorized direct sbt verification because the configured Metals MCP
endpoint was unavailable. Actual sbt 2 matrix IDs use an unsuffixed Scala 3 project and
`2_13` for Scala 2.13; the inherited AGENTS.md naming examples are stale.

Before adding review regressions, clean JVM builds passed on both Scala versions:

| Module | Scala 3 | Scala 2.13 |
|---|---:|---:|
| parser | 115 | 115 |
| parser-cats-effect | 5 | 5 |
| parser-fs2 | 7 | 7 |
| integration tests selected by `*ParserSpec` | 4 | 2 |

`ReviewSpec` intentionally records the requested behavior rather than asserting the bugs:
the enclosing-`this` test passes; the two EOF tests, invalid-budget test, and linear-scanning
test fail on both Scala 2.13 and Scala 3. These tests stay in the shared test directory.
Do not merge the branch until they pass. JS/Native, full repository CI, documentation
snippets, and new benchmark measurements were not run as part of this review.

```bash
sbt --client 'parser/testOnly *ReviewSpec' 2>&1 | tee /tmp/parser-review-3.txt
sbt --client 'parser2_13/testOnly *ReviewSpec' 2>&1 | tee /tmp/parser-review-2_13.txt
scala-cli run docs/research/parser-hearth-probes --server=false
```
