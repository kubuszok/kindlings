# Portable macro DSLs with Hearth

## Conclusion

Hearth does not need a parser-specific API or a full public mirror of both compiler ASTs.
It needs a **binding-aware DSL inspection layer and a hygienic user-function application
API**. Most of the other machinery needed to generate the parser already exists. Literal
switch construction deserves a small extension and code-quality tests, rather than another
complete code-generation framework.

“No Scala-specific solutions” should mean no compiler-specific *grammar logic* in Kindlings.
Hearth will still implement its abstraction separately for Scala 2 and 3, and public macro
entrypoints necessarily differ (`macro` versus `inline`/`${...}`). JVM/JS/Native is a separate
axis from these compiler differences.

This audit inspected the parser draft PR #228, local Hearth 0.4.2 source, and the pinned
external releases listed below. It is an API/design audit, not a completed parser port.

## What Hearth actually needs

### 1. Binding-aware declarations and references — necessary addition

Existing `DestructuredExpr` represents method calls, lambda parameters/references, blocks,
literals, singletons, and varargs. It does not expose local val declarations and their bound
references as semantic nodes. An actual Hearth 0.4.2 probe of:

```scala
{ val x = scala.util.Random.nextInt(); x + 1 }
```

produces:

```text
{ <non-destructurable: val x: Int = scala.util.Random.nextInt()>; +<non-destructurable: x>(1) }
```

The parser must associate `val expr = nonTerminal[Int]` with every subsequent use of that
specific binding, not with a name string. It must similarly inspect terminal initializers
and recognize `import g.*` as a permissible statement.

Suggested semantic API (names are proposals, not existing signatures):

- `LocalBinding` with expansion-local identity, display name, precise/declared type and position;
- `ValDefinition(binding, rhs, flags)` and `LocalReference(binding)`;
- `Block(statements, result)` with declaration-capable statements;
- import statement recognition, even if the first version exposes no detailed selector model;
- explicit unsupported-definition nodes so local `def`, `var`, or nested classes can be rejected
  at their own position instead of going through opaque tree matching.

Extend lambda bindings consistently rather than introducing two incompatible notions of identity.
Normalize typed/inlined wrappers while **preserving nonempty bindings and their scopes**.
Imports have already been resolved by the compiler; a grammar reader should not reimplement
Scala name resolution. It needs resolved calls and reference identity, not an import interpreter.

Acceptance tests: repeated names in nested scopes, shadowed DSL methods, forward non-terminal
references after declaration, aliases to terminals, imported/qualified calls, generated inline
bindings, and precise diagnostics. Retain distinctions between a declaration and an arbitrary
expression statement.

### 2. Complete reference-use inspection — necessary addition

`DestructuredExpr.collect` follows semantic `children`; `NonDestructurable` is a leaf. It
cannot safely implement either of the parser's raw tree walks:

- reject grammar-binding references anywhere in an action or conversion;
- identify which action parameters are unused, including uses inside nested lambdas,
  conditionals, matches, local definitions, and exception handlers.

Provide a compiler-backed `referencedBindings`/`references(binding)` or equivalent visitor
that traverses the complete tree, even where semantic destructuring stops. Use symbol identity
and account for binding scope. A “free references” result and an “all references” result have
different semantics; specify which each API returns.

This is important for correctness as well as optimization: wrongly classifying a parameter as
unused can remove token conversions whose values are actually needed.

### 3. Hygienic application of existing user lambdas — necessary for equivalent generated code

The parser's Scala 3 `applyFn` uses `changeOwner`, constructs `.apply(args)`, and invokes
`Term.betaReduce`. Scala 2 inspects `Function`, allocates fresh argument temporaries, calls
`untypecheck`, and splices the body. These are duplicate implementations of one operation.

Expose a semantic function-application operation accepting a typed/existential function and
arguments. It should preserve ordinary call-by-value evaluation order and exactly-once argument
evaluation, beta-reduce a literal lambda when safe, and fall back to a normal call for a function
value or method reference. Ownership and capture avoidance belong inside Hearth.

Do not conflate this with `LambdaBuilder`: that builds a new lambda, not applies/rebinds a user
lambda, and Kindlings permits it only for collection/Optional iteration lambdas. Do not silently
erase unused argument evaluation in a generic beta-reducer. The parser may choose not to generate
unused conversions under its own documented pure-action contract, using the usage API from #2.

Acceptance tests: shadowing, outer `this`, private enclosing members, nested local defs/classes,
closures capturing parameters, multiple uses of an effectful argument, primitive results,
Function1 through Function22, and expansion into several generated methods. These tests should
run through both compiler backends with fatal warnings enabled.

### 4. Literal switches — existing capability plus a targeted extension

Hearth already has `MatchCase.eqValue`; the Scala 3 implementation emits a literal pattern when
given a `Literal`, rather than always lowering equality to guards. It is inaccurate to say
Hearth cannot generate literal dispatch at all.

The parser also emits grouped alternatives (`case 1 | 2 | 3`) and wildcard defaults. Current
`MatchCase` exposes type/value/type-test matches, not a first-class grouped-literal case. One
case per literal can preserve semantics, but duplicates bodies and may change code size/JIT
behavior. A small `oneOfLiterals`/literal-switch API with a default branch would express the
intended shape directly. Its contract should specify source-level matching semantics; JVM
`tableswitch` selection is a compiler/code-generation quality check, not a cross-platform promise.

Test generated bytecode/code size for the parser's dense integer states and grouped character
sets. Keep ordinary range tests as cross-quotes; no specialized range AST is necessary.

### 5. Use the generation APIs Hearth already has

These are porting work and regression coverage, not demonstrated missing abstractions:

| Parser operation | Existing Hearth capability |
|---|---|
| Mutable scanner state, loops, array reads/writes, primitive operations | Cross-quotes and `ValDefs.createVar` |
| Fresh typed locals | `ValDefs.createVal` and `FreshName` |
| Runtime casts and type classification | `Expr`/`Type`, existential witnesses, `Type.CtorN` |
| Generated recursive local methods/scanners | `ValDefBuilder.ofDef0`/`ofDef1`, cache forward declarations and cached calls |
| Sharing those methods in one lexical scope | `ValDefsCache.toValDefs.use` |
| Generated implementation of an abstract class | `AnonymousInstance`/`OverrideContext` |
| Value and collection providers | `IsValueType` and `IsCollection` |

In particular, recursive descent does **not** establish a need for a new general-purpose method
builder: Hearth already supports forward-declared defs. Prove the parser's required method
groups with that API first.

`AnonymousInstance.self` and returning `this.type` pass the standalone probe. No blanket fix to
`AnonymousInstance` is justified by the current evidence. Add a focused test for the exact
`GeneratedReductions` shape (protected factory, inherited initialized field, multiple overrides,
and nested helper defs); if it fails, that supplies a specific Hearth issue. An alternative is
a fixed runtime factory receiving generated helper functions, consistent with Kindlings'
existing instance-factory convention, but measure indirection before claiming performance parity.

### 6. Alias normalization — confirmed bug, independent of DSL syntax

[Hearth #384](https://github.com/kubuszok/hearth/issues/384) records the demonstrated
`Ctor2.fromUntyped[IronType]` mismatch: the alias does not match, its ordinary dealiased form does.
Fix normalization within constructor matching so provider callers do not duplicate compiler
reflection. Preserve opaque type boundaries; do not turn this into general opaque unwrapping.

## What other Kindlings modules do

- **Optics** already reads paths with shared `DestructuredExpr` code in
  `optics/.../scala/.../ModifyMacrosImpl.scala:431–683`. However, the Scala 3 bridge still
  applies a context function, beta-reduces it, and strips synthetic blocks with raw reflection
  (`scala-3/.../ModifyMacros.scala:17–37`). A binding-preserving context-function/lambda
  normalization helper would remove a real second consumer of these workarounds.
- **DI** parses `DIPlan` in shared code (`WiringMacrosImpl.scala:707`). **Mock** uses shared
  `AnonymousInstance` construction and `DestructuredExpr` selector parsing. These demonstrate
  that substantial non-derivation DSLs can already use Hearth.
- **DI Cats** still has real per-compiler code for applying higher-kinded method type arguments,
  walking explicit/implicit parameter clauses, and summoning subsequent arguments
  (`ResourceWiringMacros.scala`, both versions). This is a separate `Method`-application gap
  documented in the repository; it is not solved by adding local-val nodes. It also has thin
  version-specific varargs/HKT entrypoint adaptation, which should not be confused with a
  duplicated DSL algorithm.

## External libraries: what the source actually shows

| Library / inspected release | Scala-version-specific approach | Lesson for Hearth |
|---|---|---|
| **parboiled2 2.5.1** | Separate `ParserMacros` and `OpTreeContext` for Scala 2/3. Both deconstruct rule syntax to an `OpTree` and render it, but the tree/rendering code itself is compiler-bound in each version. | Closest comparison: native macro DSLs really do duplicate substantial parsing/emission. Hearth can move that boundary below the domain IR. |
| **FastParse 3.1.1** | Scala 2 `MacroImpls` uses reify/quasiquotes; Scala 3 mixes ordinary `inline` combinators with quoted macros for specialized literals, character classes, sequences, and tries. | It mostly specializes individual combinators, not a whole yacc block. Inline reduces some reflection but does not make both compiler implementations identical. |
| **Quicklens 1.9.12** | Scala 2 `collectPathElements` matches quasiquotes and emits copies; Scala 3 `toPath` matches `Select`/`Apply`/extension forms and builds its own path representation. | Resolved-call and lambda-path normalization are genuinely reusable; Kindlings optics already benefits from Hearth here. |
| **Chimney 1.8.2** | Separate `DslMacroUtils` implementations parse selector/constructor syntax and encode it into common runtime path/argument-list types. | A shared semantic representation reduces downstream duplication even when extraction remains compiler-specific. This source is the released native implementation, not a claim about a future Hearth port. |
| **cats-parse 1.1.0** | Core combinators are ordinary shared Scala objects/functions (`Map`, `Defer`, `parseMut`). No whole-grammar compiler-tree extraction is required. | Avoids this problem by choosing a runtime combinator representation; it is not a drop-in replacement for compile-time LALR conflict diagnostics and emitted actions. |
| **Parsley 4.6.2** | A shared lazy combinator graph becomes a strict optimized tree and then a parser-machine instruction array. | “Compiles a parser” need not mean “Scala macro”. A runtime/deep-embedding compiler is portable, but changes staging and where grammar errors can be reported. Small Scala-specific utility files still exist. |

This is evidence that version-specific DSL code is common, not evidence that Kindlings should
accept all of it. Stress-testing a compiler abstraction is precisely the opportunity to move
binding, hygiene, and code-construction operations into Hearth while leaving grammar semantics
in Kindlings.

## Recommended order

1. Add local binding/statement nodes and complete reference-use inspection, with cross-version tests.
2. Add hygienic application/beta-reduction and safe context-function normalization.
3. Fix #384 independently; add grouped-literal switch support if code-size measurements justify it.
4. Port extraction into one shared `GrammarExtractor`, then generation into shared emitters using
   existing quotes, variables, cached defs, and instance construction.
5. Keep only entrypoint/compatibility glue in `scala-2`/`scala-3`; use existing parser equivalence,
   diagnostics, collection, and benchmark suites as acceptance gates. No claimed performance
   parity until measured. Do not block ordinary runtime bug fixes on this architectural work.

## Sources

Local API audit: Hearth `hearth/src/main/scala/hearth/typed/Exprs.scala` (DestructuredExpr
2017–2359; MatchCase 798–822; ValDefs 885–965; def/cache APIs 1023–1581),
`scala-3/hearth/typed/ExprsScala3.scala:1550–1648,6153–6320`, and
`hearth/src/main/scala/hearth/typed/Classes.scala` (AnonymousInstance/OverrideContext).
Parser raw operations: `scala-3/.../GrammarMacros.scala:250–429,650–749,768–1052` and
the corresponding Scala 2 file. The executable probe is in `docs/research/parser-hearth-probes/`.

Pinned upstream sources:

- [parboiled2 Scala 2 OpTreeContext](https://github.com/sirthias/parboiled2/blob/8aa4e6410d479a4d506710a20c6e529b32456f20/parboiled-core/src/main/scala-2/org/parboiled2/support/OpTreeContext.scala)
  and [Scala 3](https://github.com/sirthias/parboiled2/blob/8aa4e6410d479a4d506710a20c6e529b32456f20/parboiled-core/src/main/scala-3/org/parboiled2/support/OpTreeContext.scala).
- [FastParse Scala 2 macros](https://github.com/com-lihaoyi/fastparse/blob/0926ba1bff8cba5d4ea7ee07635a188ec2f8d6ea/fastparse/src-2/fastparse/internal/MacroImpls.scala)
  and [Scala 3 inline/macros](https://github.com/com-lihaoyi/fastparse/blob/0926ba1bff8cba5d4ea7ee07635a188ec2f8d6ea/fastparse/src-3/fastparse/internal/MacroInlineImpls.scala).
- [Quicklens Scala 2 macros](https://github.com/softwaremill/quicklens/blob/964ff968bdb424f27ff7de2fbcd0b3b4cf582a7b/quicklens/src/main/scala-2/com/softwaremill/quicklens/QuicklensMacros.scala)
  and [Scala 3](https://github.com/softwaremill/quicklens/blob/964ff968bdb424f27ff7de2fbcd0b3b4cf582a7b/quicklens/src/main/scala-3/com/softwaremill/quicklens/QuicklensMacros.scala).
- [Chimney Scala 2 DSL utilities](https://github.com/scalalandio/chimney/blob/e720c6bc37c3eaa15e2371454e47e01c141eb9e1/chimney/src/main/scala-2/io/scalaland/chimney/internal/compiletime/dsl/utils/DslMacroUtils.scala)
  and [Scala 3](https://github.com/scalalandio/chimney/blob/e720c6bc37c3eaa15e2371454e47e01c141eb9e1/chimney/src/main/scala-3/io/scalaland/chimney/internal/compiletime/dsl/utils/DslMacroUtils.scala).
- [cats-parse Parser implementation](https://github.com/typelevel/cats-parse/blob/5a4abf64083a7e8a6f171f82f5d5cfa446268034/core/shared/src/main/scala/cats/parse/Parser.scala).
- [Parsley deep-embedding architecture](https://github.com/j-mie6/parsley/blob/de25f0ecd2d58dd967769e34409b7a03eb7a4996/parsley/shared/src/main/scala/parsley/internal/deepembedding/README.md).
