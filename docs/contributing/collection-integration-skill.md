# Collection & Map Integration Skill

This document covers creating **collection and map integrations** — modules that teach Hearth's derivation system how to handle collection types from external libraries (e.g., cats `NonEmptyList`, `NonEmptyMap`) as collections or maps.

## When to create a collection integration

Create a collection integration when:
- An external library defines collection-like types (e.g., `NonEmptyList`, `Chain`, `NonEmptyMap`)
- These types should be treated as iterable collections (or key-value maps) by derivation macros
- Existing `IsCollectionProviderForScalaCollection` doesn't match them (they don't extend `Iterable`)

Do **NOT** create a collection integration for types that should be treated as:
- **Value types** (thin wrappers) — use a value type integration instead (see `value-type-integration-skill.md`)
- **Case classes** — these are derived structurally by default

## Architecture

### How `IsCollection` / `IsMap` works

1. `StandardMacroExtension` implementations register `IsCollection.Provider` instances
2. When a derivation rule encounters a type, it checks `IsCollection.unapply(tpe)` (and `IsMap.unapply(tpe)`)
3. Providers are tried in registration order until one matches
4. The matched `IsCollectionOf[A, E]` provides:
   - `asIterable: Expr[A] => Expr[Iterable[E]]` — convert to iterable for encoding
   - `factory: Expr[Factory[E, CtorResult]]` — factory for building intermediate collection
   - `build: CtorLikeOf[Builder[E, CtorResult], A]` — construct final type from builder result
5. For maps, `IsMapOf[A, (K, V)]` extends `IsCollectionOf` and adds:
   - `key`, `value`, `pair` — decompose/compose key-value pairs
   - `Key`, `Value` — type members for key and value types

### `CtorLikeOf` variants for collection construction

| Variant | Use case |
|---------|----------|
| `PlainValue` | Collection that always succeeds (e.g., `Chain` — can be empty) |
| `EitherStringOrValue` | Non-empty collections that reject empty input |

### `IsMap` before `IsCollection` ordering

`Map <: Iterable`, so `IsCollection` matches maps too. Derivation rules always check `IsMap` before `IsCollection`. When implementing a map-like type (e.g., `NonEmptyMap`), register it via `IsCollection.registerProvider` but return an `IsMapOf` instance — the rule engine will detect it as a map.

## Step-by-step: Creating a collection integration

### 1. Module structure

```
my-integration/
└── src/main/scala/hearth/kindlings/myintegration/
    └── internal/
        ├── compiletime/
        │   └── MyCollectionProviders.scala   # IsCollection.Provider implementations
        └── runtime/
            └── MyConversions.scala           # Runtime helpers (for Newtype aliases)
```

### 2. Build configuration

Same as value type integrations — create a `projectMatrix` in `build.sbt` with `macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")`. See `value-type-integration-skill.md` § "Add build configuration".

**Critical:** After modifying `build.sbt`, run `sbt --client "reload"` — the running SBT server does not auto-detect build definition changes. Without reload, the SPI extension won't be on the classpath and type matching will silently fail.

### 3. Implement the provider

The provider class extends `StandardMacroExtension` and registers `IsCollection.Provider` instances in `extend()`.

#### Key patterns

**a) Helper method pattern — avoid path-dependent types in `Expr.quote`**

Types from `Type.Ctor1.unapply` (like `elem.Underlying`) are path-dependent. Scala 2's reifier fails with "not found: value elem" when these appear inside `Expr.quote`.

**Solution:** Move `Expr.quote` code to helper methods where element types are regular type parameters:

```scala
// BROKEN on Scala 2 — path-dependent `Elem` inside quote:
override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
  NELCtor.unapply(tpe) match {
    case Some(elem) =>
      import elem.Underlying as Elem
      implicit val eType: Type[Elem] = elem.Underlying
      // ... Expr.quote { ... Elem ... }  // "not found: value elem"
  }

// CORRECT — helper method with regular type parameter:
def mkNEL[A, E](AT: Type[A], elemType: Type[E]): IsCollection[A] = {
  implicit val eType: Type[E] = elemType
  implicit val aType: Type[A] = AT
  // ... Expr.quote { ... E ... }  // E is a regular type parameter, works fine
}

override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
  NELCtor.unapply(tpe) match {
    case Some(elem) =>
      ProviderResult.Matched(mkNEL(tpe, elem.Underlying))
  }
```

**b) Runtime helper pattern — avoid Newtype type aliases in `Expr.quote`**

Some types (cats `NonEmptyChain`, `NonEmptyMap`, `NonEmptySet`) are Newtype aliases (`*Impl.Type[A]`). Scala 2's reifier fails with "not found: value data" when these types appear inside `Expr.quote`.

**Solution:** Create a runtime helper object that encapsulates the Newtype references. Quotes only call the helper methods:

```scala
// Runtime helper (in internal/runtime/):
object MyConversions {
  def nonEmptyChainToIterable[A](a: Any): Iterable[A] =
    a.asInstanceOf[cats.data.NonEmptyChain[A]].toChain.toList

  def buildNonEmptyChain(list: List[Any]): Either[String, Any] =
    if (list.nonEmpty) {
      val chain = cats.data.Chain.fromSeq(list)
      Right(cats.data.NonEmptyChain.fromChainUnsafe(chain))
    } else Left("Cannot create NonEmptyChain from empty collection")
}

// In the provider (quote only calls the helper):
override def asIterable(value: Expr[A]): Expr[Iterable[E]] =
  Expr.quote(
    MyConversions.nonEmptyChainToIterable[E](Expr.splice(value))
  )
```

The helper methods take/return `Any` where the Newtype aliases would appear, and the generated code casts to the correct types.

**c) `Type.Ctor1.fromUntyped` for cross-compilation-boundary matching**

Same as value type integrations — always wrap with `fromUntyped`:

```scala
private lazy val NELCtor = {
  val impl = Type.Ctor1.of[cats.data.NonEmptyList]
  Type.Ctor1.fromUntyped[cats.data.NonEmptyList](impl.asUntyped)
}
```

**d) Scala 3 tree invariant checker — keep correct types inside quotes**

Scala 3's `-Xcheck-macros` verifies that the tree type matches the `Expr[T]` wrapper type. Using `Any` inside quotes and casting the outer `Expr` fails. Always use `.asInstanceOf` **inside** the `Expr.quote` to maintain correct tree types:

```scala
// BROKEN on Scala 3 — tree type is Any, Expr wrapper is Iterable[(K, V)]:
val raw: Expr[Any] = Expr.quote(MyConversions.method(Expr.splice(value)))
raw.asInstanceOf[Expr[Iterable[(K, V)]]]  // tree invariant failure!

// CORRECT — cast inside the quote:
Expr.quote {
  MyConversions.method(Expr.splice(value)).asInstanceOf[Iterable[(K, V)]]
}
```

**e) Summoning `Order` for sorted types**

`NonEmptyMap` and `NonEmptySet` require `cats.kernel.Order` for construction. Summon it in the `parse` method and pass to the helper:

```scala
val OrderCtor = Type.Ctor1.of[cats.kernel.Order]

override def parse[A](tpe: Type[A]): ProviderResult[IsCollection[A]] =
  NESCtor.unapply(tpe) match {
    case Some(elem) =>
      import elem.Underlying as Elem
      implicit val orderElemType: Type[cats.kernel.Order[Elem]] = OrderCtor[Elem]
      val orderElem = Expr.summonImplicit[cats.kernel.Order[Elem]].toOption match {
        case Some(o) => o
        case None    =>
          return skipped(s"Order[${elem.Underlying.prettyPrint}] not found")
      }
      ProviderResult.Matched(mkNES(tpe, elem.Underlying, orderElem))
  }
```

### 4. Implementing `IsCollectionOf`

Each collection provider returns an `IsCollectionOf[A, E]` implementation. The key members:

```scala
Existential[IsCollectionOf[A, *], E](new IsCollectionOf[A, E] {
  // Encoding: convert to iterable
  override def asIterable(value: Expr[A]): Expr[Iterable[E]] = ???

  // Decoding: intermediate result type (usually List[E])
  override type CtorResult = List[E]
  implicit override val CtorResult: Type[CtorResult] = listEType

  // Decoding: factory for building CtorResult
  override def factory: Expr[scala.collection.Factory[E, CtorResult]] = ???

  // Decoding: construct final type from builder
  override def build: CtorLikeOf[scala.collection.mutable.Builder[E, CtorResult], A] = ???
})
```

**Using `List[E]` as `CtorResult`:** The intermediate collection type doesn't have to match the final type. Using `List` avoids needing `ClassTag` or `Ordering` at the factory level. The conversion from `List` to the target type happens in `build`.

### 5. Implementing `IsMapOf`

For map-like types, extend `IsMapOf` instead of `IsCollectionOf`. The element type is `(K, V)`:

```scala
Existential[IsCollectionOf[A, *], (K, V)](new IsMapOf[A, (K, V)] {
  // ... all IsCollectionOf members ...

  // Additional map members:
  override type Key = K
  implicit override val Key: Type[Key] = keyType
  override type Value = V
  implicit override val Value: Type[Value] = valueType
  override def key(pair: Expr[(K, V)]): Expr[Key] = Expr.quote(Expr.splice(pair)._1)
  override def value(pair: Expr[(K, V)]): Expr[Value] = Expr.quote(Expr.splice(pair)._2)
  override def pair(key: Expr[Key], value: Expr[Value]): Expr[(K, V)] =
    Expr.quote((Expr.splice(key), Expr.splice(value)))
})
```

### 6. Add tests to `integration-tests`

Create test case classes and spec files:

```scala
// catsExamples.scala
case class WithNEL(values: NonEmptyList[Int])
case class WithNEM(values: NonEmptyMap[String, Int])

// CatsCirceSpec.scala — test encode, decode, decode-rejects-empty
// CatsFastShowPrettySpec.scala — test rendering
```

Non-empty collections should test that decoding rejects empty input.

### 7. Verify

```bash
# Clean and test (macro changes require clean)
sbt --client "catsIntegration/clean ; catsIntegration3/clean ; test-jvm-2_13 ; test-jvm-3"
```

## Cross-compilation pitfalls summary

| Pitfall | Symptom | Fix |
|---------|---------|-----|
| Path-dependent types in `Expr.quote` | Scala 2: "not found: value elem" | Helper method pattern (§3a) |
| Newtype aliases in `Expr.quote` | Scala 2: "not found: value data" | Runtime helper pattern (§3b) |
| Missing `fromUntyped` | Type matching silently fails | Always use `fromUntyped` (§3c) |
| Tree type mismatch | Scala 3: tree invariant checker failure | Cast inside quotes (§3d) |
| SBT not reloaded after build.sbt change | SPI extension not loaded | `sbt --client "reload"` (§2) |

## Reference implementation

- **Cats integration**: `cats-integration/src/main/scala/.../CatsCollectionAndMapProviders.scala` — cross-compiled (Scala 2 + 3), covers `NonEmptyList`, `NonEmptyVector`, `NonEmptyChain`, `Chain`, `NonEmptyMap`, `NonEmptySet`
- **Runtime helpers**: `cats-integration/src/main/scala/.../runtime/CatsConversions.scala` — encapsulates Newtype type references
- **Hearth built-in**: `hearth/hearth/src/main/scala/hearth/std/extensions/IsCollectionProviderForScalaCollection.scala` — reference for standard library collections

## Checklist for new collection integrations

- [ ] Module directory and `build.sbt` configuration with `macroExtensionTraits`
- [ ] `StandardMacroExtension` class with `IsCollection.registerProvider` calls
- [ ] Helper methods with regular type parameters (not path-dependent)
- [ ] Runtime helper object for Newtype aliases (if applicable)
- [ ] `Type.Ctor1.fromUntyped` / `Type.Ctor2.fromUntyped` for all type constructors
- [ ] `@nowarn("msg=is never used")` on methods with implicit `Type` for cross-quotes
- [ ] `EitherStringOrValue` for non-empty collections, `PlainValue` for always-valid collections
- [ ] `IsMapOf` (not `IsCollectionOf`) for map-like types
- [ ] Added to `root` aggregate and `integrationTests.dependsOn`
- [ ] Integration tests: encode, decode, decode-rejects-empty (for non-empty types)
- [ ] Tests pass: `sbt --client "test-jvm-2_13 ; test-jvm-3"`
