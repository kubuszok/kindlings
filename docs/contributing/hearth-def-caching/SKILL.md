---
name: hearth-def-caching
description: >
  Canonical recipe for generating type-class instances with cached defs via ValDefsCache.
  forwardDeclare, buildCachedWith, toValDefs.use, parTuple for parallel derivation.
  Use when implementing multi-method type class instances or solving Scala 3 sibling-splice isolation.
paths:
  - "**/compiletime/**Macros*.scala"
user-invocable: false
---

# Def-caching with `ValDefsCache`

This is the canonical recipe for generating type-class instances whose method bodies need
to be derived once but referenced from multiple places — including instances with
**multiple abstract methods**, where each method body becomes its own `Expr.splice` and
the splices share no `Quotes` instance on Scala 3.

If you came here because you tried to write `Expr.quote { new T[A] { def m1 = …; def m2 =
… } }` and got `scala.quoted.runtime.impl.ScopeException: Expression created in a splice
was used outside of that splice`, **this is the fix**. It is *not*
[`LambdaBuilder`](../hearth-lambda-builder/SKILL.md) — `LambdaBuilder` is a workaround
that papers over this exact problem and is reserved for collection / `Optional`
iteration.

## The mental model

Three Quotes scopes are in play when generating a type-class instance:

- **`Q0`** — the scope of the macro bundle's `MIO.scoped { runSafe => … }` block. This is
  where you start. Any `Expr.quote { … }` you build here lives in `Q0`.
- **`Q1`, `Q2`, …** — each `Expr.splice` inside an `Expr.quote` opens a fresh nested
  `Quotes`. On Scala 3, these are *isolated* from one another: an `Expr` value created in
  `Q1` cannot be reused in `Q2` (even if both are nested under the same outer quote).
- **The `ValDefsCache`** — emits a `def` declaration whenever you call
  `cache.toValDefs.use(body)`. The crucial property is that the emitted `def`s live in
  whatever scope `toValDefs.use` was called in. **The defs are pure source-level names
  from then on**: any reference to them inside `Q1`, `Q2`, etc. is just an `Ident` tree
  resolved by the compiler at use site, not an `Expr` carrying its own `Quotes`.

The recipe is therefore:

1. Run derivation **at `Q0`** (in the outer `MIO.scoped { runSafe => … }` block).
2. Use `cache.forwardDeclare` + `cache.buildCachedWith` to convert each method body into
   a cached `def`.
3. Retrieve `cache.getNAry` helper-call functions — these are functions that, when
   invoked, build a *call to the named def* using whichever `Quotes` is active at call
   time.
4. Wrap the entire wrapping `Expr.quote { new T[A] { … } }` with
   `cacheState.toValDefs.use { _ => … }` so the cached `def`s are emitted at the
   outermost scope, lexically visible to every method body's splice.
5. Inside each method body, splice in a call to the helper function. The helper is
   *invoked* inside the splice (so the call tree is built with the splice's `Quotes`),
   and the resulting tree is just `Apply(Ident("decode_X"), List(arg.tree))` — a name
   reference that scalac resolves at the splice site against the outer-scope `def`.

No `Expr` value is smuggled across `Quotes` instances. No splice contains derivation
logic. No `LambdaBuilder` is involved.

**Performance-critical**: step 4 is not just about correctness — it has a major runtime
impact. If `toValDefs.use` wraps only a method body (e.g., the encode lambda inside
`encoderInstanceWithSchema`), all cached `lazy val`s become **local** to the method and
re-initialize on every call. This caused a 6x slowdown in Avro encoding where schema
construction (with regex `Pattern.compile`) ran per encode instead of once. Always wrap
the **entire type class instance expression** with `toValDefs.use`, not just the method
body. See [`kindlings-runtime-perf`](../kindlings-runtime-perf/SKILL.md) § "Place cached
vals at instance scope".

## Worked example: cats `HashMacrosImpl`

`cats.kernel.Hash[A]` has two abstract methods (`hash: A => Int` and
`eqv: (A, A) => Boolean`). The previous version used `LambdaBuilder` to pre-build both as
`Expr[A => Int]` / `Expr[(A, A) => Boolean]` because deriving inside two sibling splices
hit a `ScopeException`. The corrected entry point uses one shared `ValDefsCache` for
both:

```scala
def deriveHash[A: Type]: Expr[cats.kernel.Hash[A]] = {
  // … type-parameter sanity checks, implicit Type setups …

  Log.namedScope(…) {
    MIO.scoped { runSafe =>
      // 1. ONE shared cache for BOTH derivations.
      val sharedCache = ValDefsCache.mlocal

      // Placeholder Exprs — only used to satisfy HashCtx/EqCtx's `value`/`x`/`y` slots
      // until the rule chain forwards-declares its own def with real param exprs.
      val placeholderA: Expr[A] = Expr.quote(null.asInstanceOf[A])
      val placeholderX: Expr[A] = Expr.quote(null.asInstanceOf[A])
      val placeholderY: Expr[A] = Expr.quote(null.asInstanceOf[A])

      // 2. Run BOTH derivations under Q0, populating the shared cache via the rule
      //    chains. HashCaseClassRule and EqCaseClassRule each call
      //    `cache.forwardDeclare(...)` + `cache.buildCachedWith(...)` for their key.
      runSafe {
        for {
          _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
          _ <- deriveHashRecursively[A](using
                 HashCtx[A](Type[A], placeholderA, sharedCache, selfType))
          _ <- deriveEqRecursively[A](using
                 EqCtx[A](Type[A], placeholderX, placeholderY, sharedCache, selfType))
        } yield ()
      }

      // 3. Retrieve helper-call functions. These build call trees lazily, using the
      //    Quotes active at the moment they're invoked.
      val hashCallFor: Expr[A] => Expr[Int] =
        runSafe(sharedCache.get1Ary[A, Int]("cached-hash-method")).get
      val eqCallFor: (Expr[A], Expr[A]) => Expr[Boolean] =
        runSafe(sharedCache.get2Ary[A, A, Boolean]("cached-eq-method")).get

      // 4. Snapshot the cache state and wrap the OUTERMOST Expr.quote with toValDefs.use,
      //    emitting the def declarations as siblings of `new Hash[A] { … }`.
      val cacheState = runSafe(sharedCache.get)
      cacheState.toValDefs.use { _ =>
        Expr.quote {
          new cats.kernel.Hash[A] {
            def hash(value: A): Int = {
              val _ = value
              // 5. Inside Q1, build a call to the outer-scope `def hash_A` by name.
              //    `hashCallFor(Expr.quote(value))` is evaluated at quote-construction
              //    time and produces an `Expr[Int]` of the form `hash_A(value)`.
              Expr.splice(hashCallFor(Expr.quote(value)))
            }
            def eqv(x: A, y: A): Boolean = {
              val _ = x
              val _ = y
              // Same for Q2 — references the outer-scope `def eqv_A` by name.
              Expr.splice(eqCallFor(Expr.quote(x), Expr.quote(y)))
            }
          }
        }.asInstanceOf[Expr[cats.kernel.Hash[A]]]
      }
    }
  }
  // …flatTap, runToExprOrFail…
}
```

The generated code looks like:

```scala
{
  def hash_Foo(value: Foo): Int = /* MurmurHash3.finalizeHash over field hashes */
  def eqv_Foo(x: Foo, y: Foo): Boolean = /* field-by-field equality */
  new cats.kernel.Hash[Foo] {
    def hash(value: Foo): Int = hash_Foo(value)
    def eqv(x: Foo, y: Foo): Boolean = eqv_Foo(x, y)
  }
}
```

Both `hash_Foo` and `eqv_Foo` are emitted by `cacheState.toValDefs.use` at the outermost
level. The two splices each construct a name reference (`hash_Foo(value)` /
`eqv_Foo(x, y)`) — there is no shared `Expr` value between them and no `Quotes`
mismatch.

## Where this pattern lives in the codebase

- **Reference (multi-method, simplest)**: `cats-derivation/.../HashMacrosImpl.scala`
  `deriveHash` — two-method instance via shared cache, no LambdaBuilder.
- **Reference (multi-method, full production)**:
  `jsoniter-derivation/.../CodecMacrosImpl.scala` `deriveCombinedCodecTypeClass` — five
  cached helper bodies derived in parallel via `parTuple`.
- **Reference (single-method with helper)**:
  `circe-derivation/.../DecoderMacrosImpl.scala` `setHelper` — the canonical helper-cache
  setter; reuse this shape any time you need to expose `set*Helper`/`get*Helper` on a
  ctx for downstream rules to populate.
- **Reference (per-rule cached defs)**:
  `cats-derivation/.../rules/SemigroupCaseClassRule.scala` and
  `EqCaseClassRule.scala` — show how a *rule* (rather than the entry point) populates
  the cache via `forwardDeclare` + `buildCachedWith`. Use this shape for case-class /
  enum / value-type rules that need to emit their own def per type.

For detailed examples (jsoniter combined codec, cache lifecycle API, step-by-step recipe,
common pitfalls, `parTuple` for parallel derivation, and memoizing repeated builds), see
[`reference.md`](reference.md).

## Related skills

- [`../hearth-lambda-builder/`](../hearth-lambda-builder/SKILL.md) — when to use LambdaBuilder (collection/Optional iteration only) vs def-caching
- [`../hearth-api-reference/`](../hearth-api-reference/SKILL.md) — ValDefsCache, ValDefBuilder API signatures
- [`../kindlings-runtime-perf/`](../kindlings-runtime-perf/SKILL.md) — placing cached vals at instance scope for performance
- [`../kindlings-factory-instance/`](../kindlings-factory-instance/SKILL.md) — factory pattern that works alongside def-caching
