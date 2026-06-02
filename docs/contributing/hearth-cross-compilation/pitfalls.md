# Cross-Compilation Pitfalls -- Detailed Reference

See [SKILL.md](SKILL.md) for the index with severity ratings.

---

## Splice and quote scoping

### 1. Sibling `Expr.splice` isolation

**Severity: CRITICAL | Platform: Scala 3**

Each `Expr.splice` inside a single `Expr.quote { ... }` receives its own `Quotes` instance
on Scala 3. A `def` or closure that captures an outer `Quotes` and is invoked from inside
a splice triggers `scala.quoted.runtime.impl.ScopeException`.

The fix is **not** `LambdaBuilder` (that was a misdiagnosis). The fix is to keep all
derivation outside any splice and emit per-method bodies as cached `def`s via
`ValDefsCache`:

1. Run derivation at `Q0` using a shared `ValDefsCache`
2. For each method, `forwardDeclare` a cached `def` and populate via `buildCachedWith`
3. Retrieve `cache.getNAry` helper-call functions
4. Wrap the outer `Expr.quote { new T[A] { ... } }` with `cacheState.toValDefs.use`
5. Inside each method body's splice, call the helper function -- builds a name reference,
   not an `Expr` value carrying its own `Quotes`

Full recipe in [hearth-def-caching](../hearth-def-caching/SKILL.md).

### 2. `Expr.quote { (x: T) => ... }` is supported

**Severity: LOW | Platform: Both**

Cross-quotes accept Scala function literals directly: `Expr.quote { (x: T) => body }`
returns `Expr[T => ...]`. Use this whenever the lambda is **not** going to be passed into
a collection iteration helper.

If the body needs to capture a path-dependent reference, extract a helper method that
closes over the path-dependent value and returns the `Expr[T => U]`.

### 3. Path-dependent types in `Expr.quote`

**Severity: CRITICAL | Platform: Scala 2**

On Scala 2, `import param.tpe.Underlying as Field` and then referencing `Field` inside
`Expr.quote` generates code that references the path variable (`param`), which doesn't
exist at the expansion site.

```scala
// BROKEN on Scala 2:
import param.tpe.Underlying as Field
Expr.quote { someExpr.asInstanceOf[Field] }  // "not found: value param"
```

**Solutions:**
1. Use `LambdaBuilder.of1[Field]` which handles the type parameter correctly
2. Use a runtime type-witness utility (`unsafeCast`)
3. Use `primaryConstructor(Map[String, Expr_??])` instead of `construct` with dependent types

### 4. Macro-internal types leaking into `Expr.quote`

**Severity: HIGH | Platform: Scala 2**

`??`, `Expr_??`, and other macro-internal existentials inside an `Expr.quote` cause
reification failures. Extract them to a `val` (with concrete `import ...Underlying`)
before the quote.

```scala
// BROKEN -- Some(Type[A].as_??) has type Option[MyMacroImpl.this.??]
Expr.quote {
  Expr.splice {
    val opt: Option[??] = Some(Type[A].as_??)  // leaks into reified tree
    doSomethingWith(opt)
  }
}

// CORRECT -- compute outside Expr.quote
val opt: Option[??] = Some(Type[A].as_??)
Expr.quote {
  Expr.splice {
    doSomethingWith(opt)
  }
}
```

### 5. `Expr.upcast` only widens

**Severity: MEDIUM | Platform: Both**

`expr.upcast[B]` requires `A <:< B`. It cannot narrow types. For narrowing, use
`.asInstanceOf` inside `Expr.quote`.

**Additional constraint:** `upcast` also requires `Type[A]` (the source type) in scope.
For expressions from pattern matchers (e.g., `isMap.CtorResult`), use `.asInstanceOf`
inside `Expr.quote` instead.

### 6. `ValDefsCache` wrapping scope

**Severity: HIGH | Platform: Both**

`vals.toValDefs.use { _ => result }` must wrap the **outermost** expression that contains
all references to those vals. Wrapping a nested sub-expression hides cached vals from
other branches and emits duplicates.

---

## Type system details

### 7. Macro methods need concrete types

**Severity: MEDIUM | Platform: Both**

You cannot wrap macro calls in generic helper methods -- the call site needs the concrete
`Type[A]` available. Always inline the macro call or use a separate macro per concrete type.

### 8. Phantom type-parameter inference

**Severity: HIGH | Platform: Both**

An unconstrained `A` (not appearing in any value parameter or return type) is inferred as
`Nothing` on Scala 2 and `Any` on Scala 3. Guard against both:

```scala
if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] ||
    Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
  Environment.reportErrorAndAbort(
    s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}"
  )
```

The `.asInstanceOf[Type[A]]` cast is needed because `Type[Nothing]` has no `=:=` method.

**Reference:** All `derive*FromCtxAndAdaptForEntrypoint` methods.

### 9. `Type.of[A]` bootstrap cycle in extensions

**Severity: CRITICAL | Platform: Both**

Cross-quotes `Type.of[A]` resolves the implicit `Type[A]` lazily. When defining that very
implicit, you get a stack overflow.

**Scala 2 bypass:**
```scala
val sc2 = ctx.asInstanceOf[MacroCommonsScala2]
implicit val ConfigT: Type[Configuration] =
  UntypedType.toTyped[Configuration](sc2.c.universe.typeOf[Configuration].asInstanceOf[UntypedType])
```

**Scala 3 bypass:**
```scala
val sc3 = ctx.asInstanceOf[MacroCommonsScala3]
given scala.quoted.Quotes = sc3.quotes
implicit val ConfigT: Type[Configuration] =
  scala.quoted.Type.of[Configuration].asInstanceOf[Type[Configuration]]
```

**For shared code**, move `Type.of` calls into a helper object where the self-referential
implicit is not in scope:
```scala
object TsTypes {
  def SchemaOf[A: Type]: Type[Schema[A]] = Type.of[Schema[A]]
}
// In calling method:
implicit val schemaAType: Type[Schema[A]] = TsTypes.SchemaOf[A]
```

**Reference:** `tapir-schema-derivation/SchemaMacrosImpl.TsTypes`,
`circe-derivation/scala-2/CirceJsonFieldConfigExtension.scala`.

### 10. `IsMap` before `IsCollection` ordering

**Severity: HIGH | Platform: Both**

`Map[K, V] <: Iterable[(K, V)]`, so `IsCollection` matches map types. Always check `IsMap`
before `IsCollection` in the pattern match.

```scala
// CORRECT:
Type[A] match {
  case IsMap(im) => // catches maps first
  case IsCollection(ic) => // only non-map collections
}
```

### 11. `Type.Ctor2.of[Function1].unapply` wrong

**Severity: HIGH | Platform: Scala 3**

`Impl.unapply` returns `Nothing` for the first type arg of `Function1[Int, Boolean]` on
Scala 3. Always wrap with `Type.Ctor2.fromUntyped[Function1](impl.asUntyped)` for reliable
decomposition.

```scala
// CORRECT:
val Function1Ctor2 = {
  val impl = Type.Ctor2.of[Function1]
  Type.Ctor2.fromUntyped[Function1](impl.asUntyped)
}
```

Same fix applies to any `Type.Ctor2` use across SPI boundaries.

**Reference:** `ContravariantMacrosImpl.scala`

### 12. `primaryConstructor` strict type checking

**Severity: HIGH | Platform: Both**

`primaryConstructor(Map[String, Expr_??])` checks `Underlying <:< paramType`. You cannot
pass `Expr[Any]` for non-`Any` fields. Use the helper-method pattern to preserve field types.

### 13. `Array` needs `ClassTag`

**Severity: MEDIUM | Platform: Both**

Hearth's `IsCollectionProviderForArray` summons `ClassTag[T]` at expansion time. For
**macro-internal** arrays, use `List` and `::` instead.

---

## Implicit summoning

### 14. `summonExprIgnoring` vs OOM

**Severity: CRITICAL | Platform: Both**

`Expr.summonImplicit` without ignoring the library's auto-derivation methods causes
infinite macro expansion and OOM. **Always** call `summonExprIgnoring(...)` and pass the
methods to exclude.

### 15. Newtype aliases in `Expr.quote`

**Severity: HIGH | Platform: Scala 2**

cats Newtype types (`NonEmptyChain`, `NonEmptyMap`, `NonEmptySet`) fail on Scala 2 with
`"not found: value data"`. Use the runtime helper pattern (see
[collection-integration-skill](../collection-integration-skill.md)).

### 16. Cross-quotes unused `Type` implicit warnings

**Severity: MEDIUM | Platform: Both**

Implicit `Type[X]` declared for an `Expr.quote` is flagged as "never used" with
`-Xfatal-warnings`. Wrap the declaration in `@nowarn("msg=is never used") def`:

```scala
@scala.annotation.nowarn("msg=is never used")
def emitSRef: MIO[Expr[Schema[A]]] = {
  implicit val sNameT: Type[SName] = TsTypes.SNameType
  MIO.pure(Expr.quote { TapirSchemaUtils.refSchema[A](Expr.splice(sNameExpr)) })
}
emitSRef
```

The `@nowarn` must be on a named `def`, not on a `val` or anonymous block.

---

## HKT (kind `* -> *`) derivation

### 17. HKT type constructor summoning

**Severity: HIGH | Platform: Both**

Summoning `ConsK[G]` where `G` is a field's type constructor requires platform-specific
APIs. Use an abstract bridge method -- see
[hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md).

### 18. Erased approach for polymorphic type classes

**Severity: MEDIUM | Platform: Scala 2**

Scala 2 macros cannot handle free type variables. Work with `F[Any]` and `(Any => Any)`,
cast with `.asInstanceOf` at boundaries. See
[hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md).

### 19. `IsValueType` intercepts named tuples

**Severity: HIGH | Platform: Scala 3**

Named tuples are opaque types in Scala 3. `IsValueTypeProviderForOpaque` matches
single-element named tuples like `(field: Int)`. Guard every `HandleAsValueTypeRule` with
`Type[A].isNamedTuple` before the `IsValueType` match. Returns `false` on Scala 2.

### 20. `MacroExtension` ClassTag erasure

**Severity: MEDIUM | Platform: Both**

`MacroExtension[A & B & C]` ClassTag only preserves the first component. Use a runtime
`match`/`asInstanceOf` in `extend()`:

```scala
abstract class JsonSchemaConfigExtension
  extends MacroExtension[MacroCommons & StdExtensions] {

  final override def extend(ctx: MacroCommons & StdExtensions): Unit = ctx match {
    case _: JsonSchemaConfigs =>
      extendJsonConfig(ctx.asInstanceOf[MacroCommons & StdExtensions & JsonSchemaConfigs])
    case _ => ()
  }
}
```

---

## Additional Scala 2 reification pitfalls

### 21. `.asInstanceOf` is NOT fully erased for outer types

**Severity: HIGH | Platform: JVM**

The JVM checks the **outer type constructor** at runtime. Only inner type parameters are
erased.

```scala
// FAILS at runtime:
val result: Validated[Nel[E], A] = ...
result.asInstanceOf[Either[E, A]]  // ClassCastException!

// SUCCEEDS:
val result: Either[E, String] = ...
result.asInstanceOf[Either[E, Int]]  // OK (inner types erased)
```

### 22. Scala 2 reification failure with refined types

**Severity: HIGH | Platform: Scala 2**

Runtime type constructions inside nested `Expr.quote` blocks fail on Scala 2 with
`object Trees is not a member of package scala.reflect.api` errors.

```scala
// BROKEN on Scala 2:
Expr.quote {
  new MyTypeClass[A] {
    def apply(c: HCursor) = Expr.splice {
      Expr.quote { Left(DecodingFailure("error", Nil)): Either[DecodingFailure, A] }
    }
  }
}
```

**Solution:** Use a runtime helper method or `LambdaBuilder` with fresh derivation
(`directDecoderOpt` pattern).

### 23. Helper method pattern for providers

**Severity: MEDIUM | Platform: Scala 2**

When `Type.Ctor1.unapply` extracts `elem.Underlying`, using it in `Expr.quote` fails on
Scala 2. Move quote code to helper methods with regular type parameters:

```scala
def mkNEL[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
  implicit val eType: Type[E] = elemType
  implicit val aType: Type[A] = AT
  Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
    override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
      Expr.quote(Expr.splice(value).asInstanceOf[cats.data.NonEmptyList[E]].toList)
  })
}
```

**Reference:** `CatsCollectionAndMapProviders.scala`

### 24. Newtype type aliases in `Expr.quote`

**Severity: HIGH | Platform: Scala 2**

Cats Newtype types are type aliases to `*Impl.Type[A]`. Referencing inside `Expr.quote`
fails with "not found: value data". Create a runtime helper object:

```scala
object CatsConversions {
  def nonEmptyChainToIterable[A](a: Any): Iterable[A] =
    a.asInstanceOf[cats.data.NonEmptyChain[A]].toChain.toList
}
// In Expr.quote -- only calls the helper:
Expr.quote(CatsConversions.nonEmptyChainToIterable[E](Expr.splice(value)))
```

**Reference:** `CatsConversions.scala`, `CatsCollectionAndMapProviders.scala`

### 25. Macro methods require concrete types

**Severity: MEDIUM | Platform: Both**

A generic `def helper[A](value: A)` calling `KindlingsEncoder.encode(value)` won't work --
the macro sees `A` as abstract. Always call macro methods directly with concrete types.

### 26. Sibling `Expr.splice` isolated `Quotes`

**Severity: CRITICAL | Platform: Scala 3**

On Scala 3, each splice gets its own `Quotes` context. Expressions created in one splice
cannot be used in a sibling splice. This affects combined codec derivation where encoder
and decoder share state.

**Solution:** Pre-derive with `LambdaBuilder` or def-caching in a single `MIO.scoped`/
`runSafe` call before the `Expr.quote`:

```scala
MIO.scoped { runSafe =>
  val (encLambdaRaw, decLambdaRaw, nullVal, vals) = runSafe {
    for {
      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
      encLambdaRaw <- deriveEncoder(...)
      decLambdaRaw <- deriveDecoder(...)
      vals <- cache.get
    } yield (encLambdaRaw, decLambdaRaw, nullVal, vals)
  }
  val resultExpr = Expr.quote {
    new MyCodec[A] {
      private val _encode = Expr.splice(encLambdaRaw)
      private val _decode = Expr.splice(decLambdaRaw)
      // ...
    }
  }
  vals.toValDefs.use(_ => resultExpr)
}
```

**Reference:** `jsoniter-derivation/CodecMacrosImpl.scala`

### 27. `ValDefsCache` wrapping for multi-function

**Severity: HIGH | Platform: Both**

Do NOT wrap cached val defs around each function individually -- causes duplicates and
unused-method warnings (fatal with `-Xfatal-warnings`).

```scala
// BROKEN:
encodeFn = vals.toValDefs.use(_ => encLambdaRaw)  // has unused decode_X
decodeFn = vals.toValDefs.use(_ => decLambdaRaw)  // has unused encode_X

// CORRECT:
val resultExpr = provideCtxAndAdapt(encLambdaRaw, decLambdaRaw, nullVal)
vals.toValDefs.use(_ => resultExpr)  // all defs in scope for both
```

### 28. Path-dependent types from extractors

**Severity: MEDIUM | Platform: Scala 3**

When using `IsMap` or `IsCollection`, matched types must be **explicitly imported** to
bring their `Type` instances into scope:

```scala
Type[A] match {
  case IsMap(isMap) =>
    import isMap.{Key, Value, CtorResult}  // brings Type instances into scope
    val factoryExpr = isMap.factory
    Expr.quote { ... Expr.splice(factoryExpr) ... }  // works!
}
```

### 29. Discriminator-style enum decoding

**Severity: MEDIUM | Platform: Both**

After reading the discriminator field, remaining fields must be read from the same
already-opened object. Implement two variants: `decodeCaseClassFields` (full object) and
`decodeCaseClassFieldsInline` (remaining fields). See
[hearth-enum-rules](../hearth-enum-rules/SKILL.md).

### 30. Built-in type rule is mandatory

**Severity: HIGH | Platform: Both**

Some libraries don't provide standalone implicit instances for primitives. If you skip the
built-in type rule, primitive fields fall through to the collection rule (e.g., `String`
matches `IsCollection[Char]`), causing cascading failures.

### 31. `group()` is from `MacroSuite`

**Severity: LOW | Platform: Both**

Tests needing `group("...")` blocks must extend `MacroSuite` (from `hearth-munit`).
Modules without hearth dependency should use plain `munit.FunSuite`.

### 32. `ProviderResult` is not `Option`

**Severity: MEDIUM | Platform: Both**

`IsOption.parse[A]`, `IsCollection.parse[A]`, `IsMap.parse[A]` return `ProviderResult`,
not `Option`. Use `Type[A] match` with extractors:

```scala
Type[A] match {
  case IsOption(isOption) => ...
  case IsMap(isMap) => ...
  case IsCollection(isCollection) => ...
  case _ => CaseClass.parse[A] match { ... }
}
```

### 33. `primaryConstructor` type-checks strictly

**Severity: HIGH | Platform: Both**

You must preserve the field's original type through transformations. Use the helper method
pattern:

```scala
@scala.annotation.nowarn("msg=is never used|unused implicit parameter")
private def mkComposeExpr[Field](
    fieldExpr: Expr[Field],
    fExpr: Expr[Any => Any]
)(implicit FieldType: Type[Field], AnyType: Type[Any]): Expr[Field] =
  Expr.quote {
    Expr.splice(fieldExpr).asInstanceOf[Function1[Any, Any]]
      .compose(Expr.splice(fExpr)).asInstanceOf[Field]
  }
```

**Reference:** `ContravariantMacrosImpl.scala`

### 34. `parTraverse` and `ValDefsCache` threading

**Severity: HIGH | Platform: Both | Fixed in Hearth 0.2.0-268+**

Prior to 0.2.0-268, `parTraverse` did not thread `ValDefsCache` state between branches,
causing shared types to be re-derived exponentially.

Fixed via `MLocal.unsafeSharedParallel`: branch B sees branch A's cache writes, and
`forwardDeclare` is idempotent. All modules can safely use `parTraverse`.

On older Hearth versions, use `.traverse` (sequential) instead.

---

## Resolved Hearth issues (for context only)

These are kept only so old PR descriptions referencing them are not confusing.

- **`parTraverse` and `ValDefsCache`** -- fixed in Hearth `0.2.0-268+` by
  `MLocal.unsafeSharedParallel`.
- **`loadStandardExtensions` repeated registration** -- fixed in Hearth `0.2.0-257`.
- **Cross-quotes `Type.Ctor1` resolution in HKT** -- fixed in Hearth `0.2.0-263+`.
