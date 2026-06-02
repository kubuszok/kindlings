---
name: hearth-value-types
description: >
  Creating IsValueType providers for external value types (refined, iron, opaque types).
  Provider registration, wrap/unwrap methods, Type.Ctor2.fromUntyped for cross-compilation.
paths:
  - "**/compiletime/*ValueType*.scala"
  - "**/compiletime/*Provider*.scala"
user-invocable: false
---

# Value Type Integration Skill

This document covers creating **value type integrations** — modules that teach Hearth's derivation system how to handle types from external libraries (e.g., refined types, iron types) as value types.

## When to create a value type integration

Create a value type integration when:
- An external library defines wrapper types (opaque types, AnyVal subclasses with private constructors, etc.)
- These types should be treated as "thin wrappers" around an underlying type
- Existing encoders/decoders should encode/decode the **underlying** value, not the wrapper
- The existing `IsValueTypeProviderForAnyVal` and `IsValueTypeProviderForOpaque` cannot handle them (e.g., private constructors, no companion smart constructors)

Do **NOT** create a value type integration for types that need full derivation (multiple fields, sealed hierarchies, etc.) — use a derivation module instead.

## Architecture

### How `IsValueType` works

1. `StandardMacroExtension` implementations register `IsValueType.Provider` instances
2. When a derivation macro encounters a type, it checks `IsValueType.unapply(tpe)`
3. Providers are tried in registration order until one matches
4. The matched `IsValueTypeOf[Outer, Inner]` provides:
   - `unwrap: Expr[Outer] => Expr[Inner]` — extract the underlying value
   - `wrap: CtorLikeOf[Inner, Outer]` — construct the wrapper from the underlying value
   - `ctors: CtorLikes[Outer]` — all available constructors

### `CtorLikeOf` variants

| Variant | `Result[A]` | Use case |
|---------|------------|----------|
| `PlainValue` | `A` | No-fail wrapping (AnyVal, opaque `assume`) |
| `EitherStringOrValue` | `Either[String, A]` | Validated wrapping (refined, iron) |
| `EitherThrowableOrValue` | `Either[Throwable, A]` | Wrapping that may throw |

For value types with **validation** (refined, iron), set `wrap` to `EitherStringOrValue`. Decoder rules handle Both `PlainValue` and `EitherStringOrValue`:
- **Circe/YAML** (Either-based decoders): `EitherStringOrValue` -> `.flatMap` + convert `Left(String)` to `Left(LibraryError)`
- **Jsoniter/Avro** (exception-based decoders): `EitherStringOrValue` -> match on result, throw on `Left`
- **Encoders**: only use `unwrap`, so `wrap` variant doesn't matter

### Service discovery via `ServiceResourceGenPlugin`

Setting `macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")` in `build.sbt` causes the plugin to:
1. Scan compiled classes for implementations of `StandardMacroExtension`
2. Generate `META-INF/services/hearth.std.StandardMacroExtension` listing the providers
3. `Environment.loadStandardExtensions()` discovers and registers them at macro expansion time

## Step-by-step: Creating a new integration

### 1. Create the module directory

```
my-integration/
└── src/main/scala/hearth/kindlings/myintegration/
    └── internal/compiletime/
        └── IsValueTypeProviderForMyType.scala
```

### 2. Add build configuration

```scala
// In build.sbt versions:
val myLib = "x.y.z"

// Project matrix:
lazy val myIntegration = projectMatrix
  .in(file("my-integration"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ only1VersionInIDE) *)
  .enablePlugins(GitVersioning, GitBranchPrompt)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "kindlings-my-integration",
    name := "kindlings-my-integration",
    description := "My types integration — IsValueType provider for ...",
    macroExtensionTraits := Seq("hearth.std.StandardMacroExtension")
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(versionSchemeSettings *)
  .settings(publishSettings *)
  .settings(libraryDependencies += "org.example" %%% "my-lib" % versions.myLib)
```

Add to: `root` aggregate, `al.prodProjects` (or `al.scala3OnlyProdProjects` for Scala 3-only).

### 3. Implement the provider

```scala
package hearth.kindlings.myintegration.internal.compiletime

import hearth.fp.data.NonEmptyList
import hearth.std.{MacroCommons, StandardMacroExtension, StdExtensions}

final class IsValueTypeProviderForMyType extends StandardMacroExtension { loader =>

  override def extend(ctx: MacroCommons & StdExtensions): Unit = {
    import ctx.*

    IsValueType.registerProvider(new IsValueType.Provider {

      override def name: String = loader.getClass.getName

      // Always use fromUntyped for cross-compilation-boundary compatibility
      private lazy val MyTypeCtor = {
        val impl = Type.Ctor2.of[mylib.MyType]  // or Ctor1 for single-param
        Type.Ctor2.fromUntyped[mylib.MyType](impl.asUntyped)
      }

      @scala.annotation.nowarn("msg=is never used")
      override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
        MyTypeCtor.unapply(tpe) match {
          case Some(types) =>
            import types.{A => Inner, B => Param}
            implicit val AT: Type[A] = tpe

            val unwrapExpr: Expr[A] => Expr[Inner] = outerExpr => /* extract inner value */
            val eitherCtor = CtorLikeOf.EitherStringOrValue[Inner, A](
              ctor = innerExpr => /* validate and wrap, returning Either[String, A] */,
              method = None
            )

            ProviderResult.Matched(Existential[IsValueTypeOf[A, *], Inner](
              new IsValueTypeOf[A, Inner] {
                override val unwrap = unwrapExpr
                override val wrap = eitherCtor
                override lazy val ctors = NonEmptyList.one(
                  Existential[CtorLikeOf[*, A], Inner](eitherCtor)
                )
              }
            ))

          case None => skipped(s"${tpe.prettyPrint} is not a MyType")
        }
    })
  }
}
```

### 4. Add tests to `integration-tests`

Add test types to `integration-tests/src/test/scala/` (or `scala-3/` for Scala 3-only types) and create spec files following the pattern in existing refined/iron specs.

### 5. Verify

```bash
# Compile the integration module
sbt --client "myIntegration/clean ; myIntegration3/clean ; myIntegration/compile ; myIntegration3/compile"

# Run existing tests (verify no regressions)
sbt --client "test-jvm-2_13 ; test-jvm-3"

# Run integration tests
sbt --client "integrationTests/test ; integrationTests3/test"
```

## Cross-compilation considerations

- **Shared code** (Scala 2 + 3): Place in `src/main/scala/`. Use Hearth's macro-agnostic APIs (`Expr.quote`, `Type.of`, etc.).
- **Scala 3 only**: Can use `scala.compiletime.summonInline` and other Scala 3 features directly.
- **`@nowarn` for unused types**: `implicit Type[X]` needed by `Expr.quote` may trigger "never used" warnings. Wrap with `@scala.annotation.nowarn("msg=is never used")`.
- **Path-dependent types in quotes**: Types from `Type.Ctor2.unapply` are path-dependent. They generally work in cross-quotes, but if issues arise, use `LambdaBuilder` or `asInstanceOf` to avoid direct type references.

### Critical: `Type.Ctor2.fromUntyped` for cross-compilation-boundary matching

When a provider module is compiled separately from the downstream project that triggers macro expansion, `Type.Ctor2.of[F]` may fail to match types. This happens because the `Impl` class (generated by `of`) captures `scala.quoted.Type[F]` at compile time of the provider module, which may resolve to a different internal `TypeRepr` path than the types seen during macro expansion in the downstream project.

**Always use `fromUntyped`** to create your `Ctor2`:

```scala
// WRONG — may fail across compilation boundaries:
private lazy val MyCtor = Type.Ctor2.of[mylib.MyType]

// CORRECT — uses =:= and baseType for matching, handles path differences:
private lazy val MyCtor = {
  val impl = Type.Ctor2.of[mylib.MyType]
  Type.Ctor2.fromUntyped[mylib.MyType](impl.asUntyped)
}
```

The `FromUntypedImpl.unapply` uses `TypeRepr.=:=` (semantic type equality) and `baseType` (symbol-based lookup) for matching, which correctly handles different internal path representations. The `Impl.unapply` uses `case '[HKT[a, b]]` pattern matching which is sensitive to `Type[HKT]` compilation context.

This pattern applies to both `Type.Ctor1` and `Type.Ctor2`, and also for summoning types:

```scala
val validateType: Type[Validate[Inner, Pred]] = {
  val impl = Type.Ctor2.of[Validate]
  Type.Ctor2.fromUntyped[Validate](impl.asUntyped).apply[Inner, Pred]
}
```

## Opaque types, newtypes, and named tuples — interaction pitfalls

Hearth's built-in `IsValueTypeProviderForOpaque` matches **any** Scala 3 opaque type. This
creates three interaction patterns that rule authors must understand:

### Opaque types intercept newtypes

Libraries like `monix.newtypes`, `scala-newtype`, and `zio-prelude` define newtype wrappers
that on Scala 3 are compiled as opaque types. The built-in opaque provider matches them
*before* any custom newtype provider can run. This is usually correct — the newtype IS an
opaque wrapper and should encode/decode as the underlying type. But if the newtype has
validation logic (like refined types), a custom provider with `EitherStringOrValue` wrapping
must be registered to run before the built-in opaque provider.

### Opaque types intercept named tuples (Scala 3.7+)

Single-element named tuples like `(field: Int)` are implemented as opaque types in Scala 3.
The `IsValueTypeProviderForOpaque` matches them, treating `(field: Int)` as a wrapper around
`Int` and stripping the named-tuple semantics.

**Fix**: Every `HandleAsValueTypeRule` must guard with `Type[A].isNamedTuple` check before
the `IsValueType` match:

```scala
def apply[A: Ctx]: MIO[Rule.Applicability[...]] =
  if (Type[A].isNamedTuple) MIO.pure(Rule.yielded("named tuple, not a value type"))
  else Type[A] match {
    case IsValueType(vt) => // handle as value type
    case _               => MIO.pure(Rule.yielded(...))
  }
```

`isNamedTuple` returns `false` on Scala 2, so this guard is safe cross-platform.

### Refined on Scala 3 is handled by the built-in opaque provider

`eu.timepit.refined.api.Refined` is an opaque type on Scala 3, so Hearth's built-in opaque
provider matches it before the custom refined provider. The custom provider is primarily
needed for Scala 2 (where `Refined` is an `AnyVal` that needs validated wrapping via
`refineV`).

## Reference implementation

- **Refined integration**: `refined-integration/src/main/scala/.../IsValueTypeProviderForRefined.scala` — cross-compiled (Scala 2 + 3), uses `refineV` for validation, `Refined.unapply` for unwrapping
- **Iron integration**: `iron-integration/src/main/scala/.../IsValueTypeProviderForIron.scala` — Scala 3 only, uses `RuntimeConstraint.test` for validation, `asInstanceOf` for unwrapping (opaque type)

## Checklist for new integrations

- [ ] Module directory and `build.sbt` configuration
- [ ] `macroExtensionTraits` set to `"hearth.std.StandardMacroExtension"`
- [ ] `IsValueType.Provider` implementation with `unwrap`, `wrap` (EitherStringOrValue), and `ctors`
- [ ] Added to `root` aggregate and appropriate project lists (`prodProjects` / `scala3OnlyProdProjects`)
- [ ] Integration tests covering: encoding, decoding valid, decoding invalid, round-trip
- [ ] Tests pass on all target platforms: `sbt --client "test-jvm-2_13 ; test-jvm-3"`

## Related skills

- [`../hearth-collection-map/`](../hearth-collection-map/SKILL.md) — IsCollection/IsMap providers for collection types (not value types)
- [`../hearth-standard-extensions/`](../hearth-standard-extensions/SKILL.md) — loading extensions that register providers
- [`../hearth-api-reference/`](../hearth-api-reference/SKILL.md) — IsValueType API signatures and type extractors
- [`../hearth-documentation/`](../hearth-documentation/SKILL.md) — verifying Hearth APIs via MCP
