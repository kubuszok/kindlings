# When to use `LambdaBuilder` (and when not to)

## The rule

`LambdaBuilder` is reserved for **lambdas that are passed into a collection or `Optional`
iteration helper at runtime** — i.e. lambdas whose purpose is "for each element of this
`List` / `Iterable` / `Option` / `Map`, run this code". Anything else should be built
with a direct cross-quotes function literal:

```scala
Expr.quote { (x: T) => /* body referencing x */ }
```

The direct form is simpler, faster to compile, and produces a function expression you can
cache as a `lazy val` like any other.

This rule is **strict**: there is no documented exception for multi-method type-class
instances or for sibling-splice issues on Scala 3. Those problems exist, but
`LambdaBuilder` is *not* their solution — the correct solution is to emit cached `def`s
via `ValDefsCache` outside any splice. See
[`def-caching-skill.md`](def-caching-skill.md).

## Why the restriction exists

`LambdaBuilder` historically grew because two limitations bit Kindlings macros:

1. **Path-dependent types in `Expr.quote` on Scala 2.** When `param.tpe.Underlying` is
   imported as `FieldT` and referenced inside an `Expr.quote { … }`, Scala 2's
   reification fails. `LambdaBuilder` works around this because the lambda body is
   reified separately from the wrapping quote.
2. **Scala 3 sibling-splice isolation.** Multiple `Expr.splice` calls inside a single
   `Expr.quote` each receive their own `Quotes`, so an `Expr` value created in one
   splice cannot be reused in a sibling splice. `LambdaBuilder` works around this by
   pre-building both bodies as self-contained `Expr[A => B]` values that are then just
   spliced as finished trees.

Both problems have **better fixes** today:

- For path-dependent types: extract a helper method with a regular type parameter
  (`mkXyz[A, FieldT]`) that wraps the `Expr.quote` so the path-dependence stays out of
  the reified body. The collection-integration skill documents the helper-method
  pattern.
- For sibling-splice isolation: emit cached `def`s via `ValDefsCache.forwardDeclare` /
  `buildCachedWith` outside any splice (in the `MIO.scoped` block at `Q0`), wrap the
  outer `Expr.quote` with `cacheState.toValDefs.use`, and have each method body's splice
  call the cached def by name. See `def-caching-skill.md` for the full recipe and the
  `cats-derivation` `HashMacrosImpl` / `jsoniter-derivation` `deriveCombinedCodecTypeClass`
  reference implementations.

## The legitimate uses

Use `LambdaBuilder` when:

- You are about to pass the resulting `Expr[A => B]` (or 2-ary etc.) into a runtime
  helper that **iterates** something:
  - `xxxFromMappedPairs(iterable, pairLambda)` — Map iteration
  - `decodeCollection(reader, itemLambda)` — `List` / `Vector` / `Set` decoding
  - `decodeOption(opt, innerLambda)` — `Option` inner decoding
  - `sequenceDecodeResults(list).map(arrayCtorLambda)` — `Array[Any]` constructor (the
    constructor lambda is shaped like a single-element collection map)

Examples that pass:

- `circe-derivation/.../EncoderHandleAsCollectionRule.scala`
- `circe-derivation/.../DecoderHandleAsCollectionRule.scala`
- `circe-derivation/.../EncoderHandleAsOptionRule.scala`
- `xml-derivation/.../DecoderHandleAsCaseClassRule.scala:273-292` — `Array[Any]`
  constructor wrapper for `sequenceDecodeResults`
- `jsoniter-derivation/.../DecoderHandleAsCaseClassRule.scala:284,464` — `Array[Any]`
  constructor

## How to migrate an inappropriate `LambdaBuilder` usage

**Step 1.** Identify the shape:

- Is the resulting `Expr[T => U]` going to be passed into a collection / Optional
  iteration helper, or stored as a function value spliced into a runtime call site?
- If "collection iteration": legitimate, leave it alone.
- If "function value spliced into a single use site": convert it.

**Step 2a — simple case: direct cross-quotes lambda.** When the body does not need
recursive macro state and only references already-derived `Expr` values:

```scala
LambdaBuilder.of1[X]("arg")
  .traverse { argExpr => MIO.pure(/* Expr.quote referencing argExpr */) }
  .map(_.build[Y])
```

→

```scala
Expr.quote { (arg: X) => /* body referencing arg */ }
```

If the body needs to capture a path-dependent value (e.g.
`isValueType.value.unwrap`), extract a helper method that closes over it:

```scala
private def buildXyz[A: Type, Inner: Type](
    isValueType: IsValueTypeOf[A, Inner]
): Expr[Inner => A] =
  Expr.quote { (inner: Inner) =>
    Expr.splice(isValueType.wrap.apply(Expr.quote(inner)).asInstanceOf[Expr[A]])
  }
```

The helper isolates the path-dependent reference outside the `Expr.quote`. Reference
rewrites:

- `circe-derivation/.../EncoderHandleAsMapRule.scala` — `buildValueTypeKeyEncoder`
  helper for value-type key wrap.
- `circe-derivation/.../DecoderHandleAsMapRule.scala` — `buildValueTypeKeyDecoder`.
- `xml-derivation/.../DecoderHandleAsValueTypeRule.scala` — `buildPlainWrap` /
  `buildEitherWrap`.
- `ubjson-derivation/.../DecoderHandleAsValueTypeRule.scala` — `buildWrap`.

**Step 2b — recursive case: cached `def` via `ValDefsCache`.** When the body needs to
run derivation against the lambda's parameter expression (the LambdaBuilder was being
used because "I need a fresh ctx to derive against"):

This is a `def-caching-skill.md` situation. Forward-declare a cached `def`, populate it
via `cache.buildCachedWith`, retrieve a helper-call function, and wrap it in a direct
cross-quotes lambda. See that skill for the full recipe.

**Step 3.** Compile **and** run the module's full test suite for both Scala 2.13 and
Scala 3. The simple-case rewrite usually works on the first try. The recursive-case
rewrite is more delicate and can fail with either:

- `ScopeException` on Scala 3 — you're still running derivation inside a splice instead
  of at `Q0`. Re-read the def-caching skill.
- `not found: value xxx$macro$NNN` on Scala 2 — the cached def's body references an
  outer-scope-bound value that's not visible at the def's emission site. Bump the
  `ofDefN` arity to pass the value as a parameter. Again see the def-caching skill.

## Outstanding rule-5 violations

These sites still use `LambdaBuilder` for shapes that should be converted via the
recursive (def-caching) path:

- `xml-derivation/.../rules/DecoderHandleAsCaseClassRule.scala` — per-field decoder
  LambdaBuilders used as `unsafeCastWithFn` type-inference helpers. Conversion needs to
  introduce a per-field cached def (`def decodeField_X(elem: Elem): Either[…, FieldT]`).
- `xml-derivation/.../rules/DecoderHandleAsEnumRule.scala` — same shape for enum
  branches.
- `ubjson-derivation/.../rules/DecoderHandleAsCaseClassRule.scala` — same shape.
- `ubjson-derivation/.../rules/DecoderHandleAsEnumRule.scala` — same shape.
- `avro-derivation/.../DecoderMacrosImpl.scala:366-383` — `LambdaBuilder.of1[Any]("fieldValue")`
  wrapping a field-decoder derivation.

The reference conversion is now
`jsoniter-derivation/.../rules/DecoderHandleAsCaseClassRule.scala` `deriveFieldDecoder`,
which followed the recipe in `def-caching-skill.md` exactly:

1. Forward-declare a cached `def decode_field_X(reader: JsonReader, config: JsoniterConfig): Field`
   under the shared `ValDefsCache`.
2. Memoize-check via `defBuilder.isBuilt(cacheState, key)` so case classes with multiple
   same-typed fields don't double-build.
3. Populate via `buildCachedWith` using a fresh `DecoderCtx.from(readerExpr, configExpr,
   ctx.cache, ctx.derivedType)` built from the def's own parameter Exprs (not the outer
   ctx's reader/config).
4. Retrieve the helper-call function via `ctx.cache.get2Ary[…]`.
5. Wrap in a direct cross-quotes lambda: `Expr.quote { (r: JsonReader) =>
   Expr.splice(callerFn(Expr.quote(r), ctx.config)) }`.

Tests pass on both Scala 2.13 (209/209) and Scala 3 (209/209).
