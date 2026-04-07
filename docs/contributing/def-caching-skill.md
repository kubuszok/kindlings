# Def-caching with `ValDefsCache`

This is the canonical recipe for generating type-class instances whose method bodies need
to be derived once but referenced from multiple places — including instances with
**multiple abstract methods**, where each method body becomes its own `Expr.splice` and
the splices share no `Quotes` instance on Scala 3.

If you came here because you tried to write `Expr.quote { new T[A] { def m1 = …; def m2 =
… } }` and got `scala.quoted.runtime.impl.ScopeException: Expression created in a splice
was used outside of that splice`, **this is the fix**. It is *not*
[`LambdaBuilder`](lambda-builder-when-to-use-skill.md) — `LambdaBuilder` is a workaround
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

## Worked example: jsoniter combined codec

`KindlingsJsonValueCodec[A]` (and its key-codec sibling) has **five** abstract methods
(`nullValue`, `decodeValue`, `encodeValue`, `decodeKey`, `encodeKey`). It is the largest
production user of this pattern. See
`jsoniter-derivation/.../CodecMacrosImpl.scala:333-420` (`deriveCombinedCodecTypeClass`)
for the full implementation. Highlights:

```scala
MIO.scoped { runSafe =>
  val cache = ValDefsCache.mlocal      // ONE shared cache for all 5 method bodies

  // Each helper is forward-declared, its body derived inside buildCachedWith,
  // and the helper-call function retrieved.
  val encMIO: MIO[(Expr[A], Expr[JsonWriter], Expr[JsoniterConfig]) => Expr[Unit]] = {
    val defBuilder = ValDefBuilder.ofDef3[A, JsonWriter, JsoniterConfig, Unit](
      s"codec_encode_${Type[A].shortName}")
    for {
      _ <- cache.forwardDeclare("codec-encode-body", defBuilder)
      _ <- MIO.scoped { rs =>
        rs(cache.buildCachedWith("codec-encode-body", defBuilder) { case (_, (v, w, c)) =>
          rs(deriveEncoderRecursively[A](using EncoderCtx.from(v, w, c, cache, selfType)))
        })
      }
      fn <- cache.get3Ary[A, JsonWriter, JsoniterConfig, Unit]("codec-encode-body")
    } yield fn.get
  }
  val decMIO: MIO[(Expr[JsonReader], Expr[JsoniterConfig]) => Expr[A]]   = … // same shape
  val nullMIO: MIO[Expr[A]]                                              = deriveNullValue[A]
  val keyEncMIO: MIO[Option[Expr[(A, JsonWriter) => Unit]]]              = …
  val keyDecMIO: MIO[Option[Expr[JsonReader => A]]]                      = …

  // All 5 derivations execute IN PARALLEL via parTuple — they share the cache safely
  // because Hearth's MLocal supports parallel cache writes.
  val (((encFn, decFn), nullVal), (keyEncOpt, keyDecOpt)) = runSafe {
    for {
      _      <- ensureStandardExtensionsLoaded()
      result <- encMIO.parTuple(decMIO).parTuple(nullMIO).parTuple(keyEncMIO.parTuple(keyDecMIO))
    } yield result
  }

  // Snapshot the cache and wrap the outer quote.
  val vals = runSafe(cache.get)
  val resultExpr = Expr.quote {
    new KindlingsJsonCodec[A] {
      def nullValue: A                            = Expr.splice(nullVal)
      def decodeValue(in: JsonReader, default: A): A = {
        val _ = default
        Expr.splice(decFn(Expr.quote(in), configExpr))
      }
      def encodeValue(x: A, out: JsonWriter): Unit =
        Expr.splice(encFn(Expr.quote(x), Expr.quote(out), configExpr))
      def decodeKey(in: JsonReader): A =
        Expr.splice(keyDecFn).apply(Expr.splice(Expr.quote(in)))
      def encodeKey(x: A, out: JsonWriter): Unit =
        Expr.splice(keyEncFn).apply(Expr.splice(Expr.quote(x)), Expr.splice(Expr.quote(out)))
    }
  }
  vals.toValDefs.use(_ => resultExpr)   // wraps ALL 5 method splices
}
```

Five sibling splices, zero `LambdaBuilder`, no `ScopeException`. The generated code emits
five top-level `def`s followed by `new KindlingsJsonCodec[A] { … }`.

## The cache lifecycle: `forwardDeclare` → `getNAry` → `isBuilt` → `buildCachedWith`

The four cache operations interact in a specific way that you need to understand to build
recursive helpers correctly:

### `cache.forwardDeclare(key, builder): MIO[Unit]`

Registers a *placeholder* in the cache for the given key + signature. After this:

- `cache.getNAry[…](key)` returns `Some(callerFn)` where `callerFn` builds a **call-site
  reference** to the (eventually-resolved) named def. The reference is safe to splice
  even though the body has not been built yet.
- `builder.isBuilt(cacheState, key)` returns `false`.

`forwardDeclare` is idempotent — calling it twice with the same key + builder is safe;
the cache state is just overwritten with the same placeholder.

### `cache.buildCachedWith(key, builder)(f): MIO[Unit]`

Populates the body of the (already-or-now-forward-declared) cached def. Internally:

1. Checks `builder.isBuilt(cache, key)`. If already built, **throws**
   `HearthRequirementError("buildCachedWith('$key'): key+signature already built")`. You
   cannot double-build the same key.
2. Calls `forwardDeclare` (in case it wasn't already).
3. Eagerly invokes `f(value)` where `value` is the def's parameter expression(s). `f` may
   itself trigger nested cache writes via `runSafe` side effects (e.g. recursive helper
   construction).
4. Reads the *fresh* cache state (which now includes any entries `f` added).
5. Builds the entry, marking it as built.

After this, `isBuilt` returns `true` and `getNAry` continues to return a callable
reference — but now that reference resolves to a real body.

### `cache.getNAry[Args, Result](key): MIO[Option[…]]`

Returns `Some(callerFn)` if the key has been **forward-declared** (regardless of whether
the body is built), and `None` if the key was never declared. `callerFn` is a function
that builds the call-site tree using whichever `Quotes` is active at call time.

Use this to **construct call expressions** to a cached def. It is safe to call from
inside a `buildCachedWith` body builder for recursive self-references — the call is to
the forward-declared name, which will resolve once the surrounding `buildCachedWith`
finishes building.

### `builder.isBuilt(cacheState, key): Boolean`

Returns `true` if a body has been populated for that key + signature, `false` otherwise
(including the case where the key has been forward-declared but not yet built, and the
case where the key was never touched). Use this to **decide whether to populate** a key
or to **gate fallback paths** that need a guarantee the body is real.

`isBuilt` takes a *snapshot* of the cache (`cacheState: ValDefsCache`), not the
`MLocal[ValDefsCache]`. Get the snapshot via `runSafe(cache.get)`.

### Putting them together: recursive helpers

For a self-recursive type like `case class Tree[A](left: Tree[A], right: Tree[A], a: A)`:

```scala
val cacheKey = "cached-tree-decoder"
val builder  = ValDefBuilder.ofDef1[JsonReader, Tree[A]](s"decode_${Type[A].shortName}")

for {
  // Step 1: Forward-declare the cached def. After this, recursive references via
  //         cache.get1Ary work, even though the body isn't built yet.
  _ <- cache.forwardDeclare(cacheKey, builder)

  // Step 2: Populate the body. Inside the body builder, deriveDecoderRecursively for
  //         Tree[A] will hit `*UseCachedRule`, which calls cache.get1Ary(cacheKey),
  //         finds the forward-declared entry, and emits a call to it. That self-call
  //         resolves at code-emission time to the def we are about to build — a real
  //         recursive call in the generated code.
  _ <- MIO.scoped { rs =>
    rs(cache.buildCachedWith(cacheKey, builder) { case (_, readerExpr) =>
      rs(deriveDecoderRecursively[Tree[A]](using ctx.copy(reader = readerExpr)))
    })
  }
} yield ()
```

### Putting them together: gating a fallback with `isBuilt`

When the entry point runs the rule chain to populate a shared cache, but some shapes
(e.g. primitives going through a built-in rule that doesn't touch the cache) leave the
cache untouched, you need to detect that and fall back. **Use `isBuilt`, not `getNAry
returns Some`**, because in a recursive scenario `getNAry` may return `Some` of a
forward-declared-but-not-yet-built reference and using it as if it were built would emit
unresolvable names:

```scala
val cacheStateAfterRules = runSafe(cache.get)
val ruleBuilder = ValDefBuilder.ofDef1[A, Int]("hash_" + Type[A].shortName)
val ruleBuilt = ruleBuilder.isBuilt(cacheStateAfterRules, "cached-hash-method")

val callFor: Expr[A] => Expr[Int] =
  if (ruleBuilt) {
    // Reuse the rule-built helper directly.
    runSafe(cache.get1Ary[A, Int]("cached-hash-method")).get
  } else {
    // Fall back: forward-declare + build a fresh entry under a unique key, owned by
    // the entry point. Always populates, so the helper is always callable.
    val key = s"entry-fallback:${Type[A].prettyPrint}"
    val builder = ValDefBuilder.ofDef1[A, Int]("hash_" + Type[A].shortName)
    runSafe {
      MIO.scoped { rs =>
        rs(cache.buildCachedWith(key, builder) { case (_, valueExpr) =>
          rs(deriveHashRecursively[A](using HashCtx[A](Type[A], valueExpr, cache, selfType)))
        })
      }
    }
    runSafe(cache.get1Ary[A, Int](key)).get
  }
```

The reference implementation is `cats-derivation/.../HashMacrosImpl.scala` `deriveHash`.

## Step-by-step recipe

For any new derivation:

1. **Identify all the cached helpers you need.** One per method body that runs
   derivation. Pick a stable cache key per helper (e.g. `"cached-foo-method"` or a
   per-type variant `s"foo-thing:${Type[A].prettyPrint}"` if multiple distinct keys could
   collide).
2. **Pick the right `ValDefBuilder.ofDefN` arity.** The arity must include *every* runtime
   value the body refers to that comes from outside the def — typically the input(s) and
   any config / state objects. Anything captured from outer-scope identifiers other than
   the def's own parameters will fail to resolve once the def is hoisted.
3. **Forward-declare each helper** with `cache.forwardDeclare(key, defBuilder)`.
4. **Populate each helper** with `cache.buildCachedWith(key, defBuilder) { case (_,
   args) => derivation(args) }`. The `args` are the def's parameter expressions; build a
   fresh ctx using *those* exprs (e.g. `MyCtx.from(args._1, args._2, cache, selfType)`)
   so the body never references outer captured values.
5. **Retrieve helper-call functions** with `cache.getNAry[A1, …, Result](key)`.
6. **Snapshot the cache state** with `runSafe(cache.get)`.
7. **Wrap the entire `Expr.quote { new T[A] { … } }` with `cacheState.toValDefs.use { _
   => … }`.** Each method body inside the quote uses
   `Expr.splice(helperFn(Expr.quote(arg1), …))` to call the cached def by name.
8. **Test on Scala 2.13 *and* Scala 3.** The first sibling-splice issue is Scala-3-only;
   the second class of "out-of-scope identifier" issues is Scala-2-only (because Scala 2
   alpha-renames vals to `name$macro$NNN` and the failing reference will be visible in
   the error message).

## Common pitfalls

### "not found: value `xxx$macro$NNN`" on Scala 2

The cached def's body references a name (`xxx$macro$NNN` is the alpha-renamed form of
some `val xxx = …`) that's bound at the call site, not at the def's emission site. The
def is emitted as a sibling of the wrapping `Expr.quote`, **not** nested inside it, so
nothing bound by the wrapping quote is in scope at the def.

Fix: pass that value as a *parameter* of the cached def. Bump the `ofDefN` arity, change
the buildCachedWith callback to receive it, and have callers pass it explicitly via the
helper-call function.

### `ScopeException` on Scala 3 — "Expression created in a splice was used outside of that splice"

You're running derivation *inside* an `Expr.splice { … }` block, which means the
`ValDefsCache` emits its defs in that splice's `Q1`. A sibling splice with `Q2` cannot
reference them.

Fix: hoist all derivation up to `Q0` (the `MIO.scoped` block). Use the recipe above. The
splices in your wrapping `Expr.quote { new T[A] { … } }` should contain *only* a call to
a helper-call function, never a derivation invocation.

### Cyclic reference involving `*MacrosImpl with MacroCommons with StdExtensions with AnnotationSupport`

This is a Scala 2 **incremental compilation artifact**, not a real type-system cycle.
When you change a rule's signature (e.g. add new implicit `Type` declarations or change
the `ValDefBuilder.ofDefN` arity), zinc's incremental compiler can produce stale class
files that report a cyclic reference even though a clean build succeeds.

**Fix**: `sbt --client "<module>/clean ; <module>3/clean ; <module>/compile ;
<module>3/compile"`. Always clean both Scala-version variants when touching rule traits.

This was observed during the jsoniter `deriveFieldDecoder` conversion: the first attempt
appeared to fail with `illegal cyclic reference involving CodecMacrosImpl with
MacroCommons with StdExtensions with AnnotationSupport`, but a clean rebuild made it
compile fine.

### Body must use the def's params, not the placeholder/outer ctx values

Inside `cache.buildCachedWith(key, defBuilder) { case (_, args) => … }`, the `args` are
the def's *new* parameter Exprs. Do *not* call `deriveXRecursively[A](using ctx.nest(...))`
where `ctx` is the outer ctx — that ctx still carries the outer reader/config/etc., and
your derivation will produce a body that references them. Always build a fresh ctx from
`args` and the shared cache:

```scala
rs(cache.buildCachedWith(key, defBuilder) { case (_, (readerArg, configArg)) =>
  rs(deriveDecoderRecursively[A](using
    DecoderCtx.from(readerArg, configArg, cache, selfType)))
})
```

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

## Deriving multiple method bodies in PARALLEL via `parTuple`

When a multi-method type-class instance has several method bodies that need to be
derived independently, you have two choices for how to compose them:

```scala
// SEQUENTIAL — fail-fast: if `deriveHashRecursively` errors, the user never sees
// the (potentially also-failing) eqv derivation errors.
for {
  _ <- deriveHashRecursively[A](using …)
  _ <- deriveEqRecursively[A](using …)
} yield ()
```

```scala
// PARALLEL — error-aggregating: BOTH derivations always run, and any errors from
// either are combined into a single report. Strictly preferable for the multi-method
// case because users debugging an unsupported type get one combined message instead
// of having to fix-and-retry repeatedly.
for {
  _ <- deriveHashRecursively[A](using …)
         .parTuple(deriveEqRecursively[A](using …))
} yield ()
```

`parTuple` lives directly on `MIO` (`hearth.fp.effect.MIO.parTuple`) — no extra import
needed beyond `import hearth.fp.effect.*`. It's defined as
`parMap2(fb)((a, b) => (a, b))` and delegates to `MIO`'s `Parallel` instance, which
runs the two computations as if they were separate "fibers" with their own forks of any
`MLocal` state. The fork is the key: the two branches **do not** share `MLocal` writes,
so any cache state populated in one branch is invisible to the other.

### Strict rule: use `parTuple` for independent multi-method derivations

For any new multi-method type-class entry point, **derive every method body in parallel**
via `parTuple` (or `parMap2` for non-tuple results, or repeated `parTuple` chains for
N > 2 methods). This is the same pattern jsoniter's `deriveCombinedCodecTypeClass` uses
for its 5 method bodies (`encMIO.parTuple(decMIO).parTuple(nullMIO).parTuple(keyEncMIO.parTuple(keyDecMIO))`).

### When `parTuple` is unsafe: branches that read each other's MLocal state

`parTuple` forks `MLocal` per branch. If the two derivations are not actually
independent — i.e. one branch reads cache state populated by the other (e.g. nested
helpers) — they will silently see different snapshots and produce wrong code.

**Concrete example seen in this codebase**: avro `deriveFieldDecoder` originally tried
to run `deriveDecoderRecursively[Field]` and `deriveSelfContainedSchema[Field]` in
parallel via `parTuple`. The two derivations both go through the rule chain and share
some Hearth-internal `MLocal` state (not just the explicit `ValDefsCache`); the
parallelized version broke Scala 3 parameterized-enum tests because the schema branch
saw a stale fork of state populated by the decoder branch (or vice versa). We reverted
to a sequential `for`-comprehension and added a comment explaining why.

**Rule of thumb**: parallelize when the derivations are *aspects of distinct methods* of
the eventual type-class instance (Hash's hash + eqv, codec's encode + decode + nullValue
+ keyEncode + keyDecode). Do NOT parallelize when the derivations are *aspects of one
operation* on the same value (decoder body + schema for the same field).

### Why error aggregation matters

A user trying to derive `Hash[Foo]` for a Foo whose fields lack both `Hash` *and* `Eq`
instances should see **both** missing-instance errors in one go, not have to:

1. Run macro → see hash error → add a Hash instance for the field type
2. Re-run macro → see eq error → add an Eq instance for the field type
3. Re-run macro → success

With `parTuple` they see both errors in step 1 and fix them in one cycle.

## Memoizing repeated `buildCachedWith` calls

Some rules iterate over a list (e.g. case-class fields) and call `buildCachedWith` once
per element. If two elements share the same key + signature (e.g. two fields of the same
type), the second call hits the "key+signature already built" guard inside Hearth and
throws `HearthRequirementError`.

The fix is the standard memoize check using `isBuilt` before building:

```scala
for {
  cacheState <- ctx.cache.get
  _ <-
    if (defBuilder.isBuilt(cacheState, cacheKey)) MIO.pure(())
    else
      for {
        _ <- ctx.cache.forwardDeclare(cacheKey, defBuilder)
        _ <- MIO.scoped { rs =>
          rs(ctx.cache.buildCachedWith(cacheKey, defBuilder) { case (_, args) =>
            rs(deriveBody(args))
          })
        }
      } yield ()
  callerOpt <- ctx.cache.getNAry[…](cacheKey)
} yield wrapInLambda(callerOpt.get)
```

This pattern is what `jsoniter-derivation/.../rules/DecoderHandleAsCaseClassRule.scala`
`deriveFieldDecoder` uses to safely handle case classes with multiple same-typed fields.

## Outstanding migrations

The following sites still use `LambdaBuilder` for shapes that should be converted to
this pattern:

- `xml-derivation/.../rules/DecoderHandleAsCaseClassRule.scala` (per-field
  `unsafeCastWithFn` LambdaBuilders) and `DecoderHandleAsEnumRule.scala`.
- `ubjson-derivation/.../rules/DecoderHandleAsCaseClassRule.scala` and
  `DecoderHandleAsEnumRule.scala`.
- `avro-derivation/.../DecoderMacrosImpl.scala:366-383` — `LambdaBuilder.of1[Any]("fieldValue")`
  wrapping a field-decoder derivation.

Each of these is the same shape: an `Expr[X => Y]` is built with `LambdaBuilder` purely
to dodge a path-dependent `FieldT` leaking into `Expr.quote`. The fix is to forward-declare
a cached `def field_T(x: X): Y` against the surrounding entry point's shared cache, then
wrap the helper-call function in a direct cross-quotes lambda
(`Expr.quote { (x: X) => Expr.splice(callerFn(Expr.quote(x))) }`).

The reference conversion is `jsoniter-derivation/.../rules/DecoderHandleAsCaseClassRule.scala`
`deriveFieldDecoder` (forward-declare → memoize check → buildCachedWith with fresh ctx
→ wrap caller in `Expr.quote`).
