---
name: kindlings-debugging
description: >
  Debugging Kindlings macro derivation. LogDerivation imports, MIO diagnostic logging,
  flame graph generation, error trait hierarchy per module, compile-time annotation
  collection, recursive type tracking with MLocal.
paths:
  - "**/compiletime/**Macros*.scala"
  - "**/debug/*.scala"
user-invocable: false
---

# Skill: Debugging Kindlings Derivation

How to debug macro derivation issues in Kindlings modules.

## Enabling derivation logging

### Method 1: Import-based logging

Import the debug logger in the scope of the macro call:

```scala
import hearth.kindlings.fastshowpretty.debug.logDerivationForFastShowPretty
```

Each module provides its own `debug/package.scala` with a module-specific implicit:

```scala
package hearth.kindlings.mymodule

package object debug {
  implicit val logDerivationForMyTypeClass: hearth.LogDerivation = hearth.LogDerivation.Enabled
}
```

### Method 2: Scalac option

Set a scalac option globally to enable logging for all macro calls in the compilation:

```
-Xmacro-settings:fastShowPretty.logDerivation=true
-Xmacro-settings:circeDerivation.logDerivation=true
-Xmacro-settings:jsoniterDerivation.logDerivation=true
```

This is checked by `shouldWeLogDerivation` in the macro bridge code.

### What the log shows

The derivation log prints at compile time, showing:
- Which rules were attempted for each type
- Which rule matched or why each was skipped
- Cache hits and misses
- Recursive derivation path (hierarchical via `Log.namedScope`)
- Error details when derivation fails

## MIO diagnostic logging

Every rule object starts with `Log.info(...)` and logs on match/yield. The hierarchical
structure from `Log.namedScope` makes it easy to trace nested derivation:

```
[info] Deriving for type PersonWithAddress
[info]   Attempting to use cached definition for PersonWithAddress
[info]   The type PersonWithAddress does not have a cached definition
[info]   Attempting to use implicit for PersonWithAddress
[info]   PersonWithAddress has no implicit instance
[info]   Attempting to handle PersonWithAddress as a case class
[info]     Deriving the value x.name: String
[info]       Attempting to use built-in support for String
[info]       Found built-in support for String
[info]     Deriving the value x.address: Address
[info]       Attempting to handle Address as a case class
[info]       ...
```

## Flame graph generation

Hearth 0.2.0-256+ supports flame graph generation for profiling macro compilation
performance:

```
-Xmacro-settings:hearth.mioBenchmarkScopes=true
-Xmacro-settings:hearth.mioBenchmarkFlameGraphDir=/tmp/kindlings-flamegraphs
```

Generates `.speedscope.json` files viewable at [speedscope.app](https://speedscope.app).

### Key profiling findings (Hearth 0.2.0-257+)

- `loadStandardExtensions` caching was fixed in 0.2.0-257 (was 7.6s / 15.7% of total)
- Remaining budget: rule evaluation self-time (52%), outer gap/expr tree build (31%),
  field derivation (17%)
- Rule evaluation cost is irreducible compiler reflection: `=:=`, `baseType()`, implicit
  search
- Type comparisons cannot be memoized (no reliable hash codes on compiler types)
- Rules already use symbol-based fast rejection before expensive type comparisons

## Error trait hierarchy

Each module defines a sealed error trait hierarchy. All error traits extend
`util.control.NoStackTrace` and have a `message` field.

| Module | File | Sealed Trait | Case Classes |
|--------|------|-------------|--------------|
| fast-show-pretty | `DerivationError.scala` | `DerivationError` | `UnsupportedType`, `NoChildrenInSealedTrait` |
| circe encoder | `EncoderMacrosImpl.scala` | `EncoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait` |
| circe decoder | `DecoderMacrosImpl.scala` | `DecoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `CannotConstructType` |
| yaml encoder | `EncoderMacrosImpl.scala` | `EncoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait` |
| yaml decoder | `DecoderMacrosImpl.scala` | `DecoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `CannotConstructType` |
| avro schema | `SchemaForMacrosImpl.scala` | `SchemaDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait` |
| avro encoder | `EncoderMacrosImpl.scala` | `EncoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait` |
| avro decoder | `DecoderMacrosImpl.scala` | `DecoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `CannotConstructType`, `NoChildrenInSealedTrait`, `EnumChildError` |
| jsoniter codec | `CodecMacrosImpl.scala` | `CodecDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait`, `CannotConstructType`, `UnexpectedParameterInSingleton` |

### Error fail pattern

All error sites follow `Log.error >> MIO.fail`:

```scala
val err = XxxDerivationError.SomeCase(...)
Log.error(err.message) >> MIO.fail(err)
```

### Conditional errorRendering

The `errorRendering` parameter in `runToExprOrFail` is conditional on whether logging is
enabled:

```scala
errorRendering = if (shouldWeLogXxx) RenderFrom(Log.Level.Info) else DontRender
```

## Compile-time annotation collection

When a derived type class needs to propagate annotations (e.g., Tapir `@description`,
`@deprecated`):

1. **Collect at compile time** as `List[UntypedExpr]` via platform-specific APIs
2. **Convert to `Expr[List[Any]]`** using `ann.asTyped[Any]`
3. **Pattern-match at runtime** in a utility method

```scala
// Compile-time:
private def collectAnnotationsExpr(annotations: List[UntypedExpr]): Expr[List[Any]] = {
  implicit val anyType: Type[Any] = TsTypes.AnyType
  implicit val listAnyType: Type[List[Any]] = TsTypes.ListAnyType
  annotations.foldRight(Expr.quote(List.empty[Any]: List[Any])) { (ann, acc) =>
    val typedAnn: Expr[Any] = ann.asTyped[Any]
    Expr.quote(Expr.splice(typedAnn) :: Expr.splice(acc))
  }
}

// Runtime:
def enrichSchema[T](schema: Schema[T], annotations: List[Any]): Schema[T] =
  annotations.foldLeft(schema) {
    case (s, ann: description) => s.description(ann.text)
    case (s, ann: deprecated)  => s.deprecated(true)
    case (s, _)                => s
  }
```

**Platform-specific annotation access:**
- **Scala 2**: `param.asUntyped.asInstanceOf[sc2.UntypedParameter].symbol.annotations`;
  `sc2.c.untypecheck(ann.tree)` for expression trees
- **Scala 3**: `param.asUntyped.annotations.asInstanceOf[List[QTerm]]`

**Reference:** `tapir-schema-derivation/SchemaMacrosImpl.scala`,
`AnnotationSupportScala2.scala`, `AnnotationSupportScala3.scala`.

## Recursive type tracking with `MLocal[Set[String]]`

For types that can be recursive (e.g., `case class Tree(children: List[Tree])`), use an
`MLocal[Set[String]]` to track in-progress derivations. When a type is encountered that is
already in-progress, emit a forward reference instead of recursing.

This differs from `ValDefsCache`-based recursion (used in encoder/decoder derivation where
the cache forward-declares a `def`). The `MLocal[Set[String]]` pattern is for cases where
you need a **lazy reference value** rather than a cached function.

```scala
val inProgress: MLocal[Set[String]] =
  MLocal(Set.empty[String])(identity)((a, b) => a ++ b)

// In the recursive derivation method:
inProgress.get.flatMap { inProgressSet =>
  if (inProgressSet.contains(cacheKey[A])) {
    // Recursive reference detected -- emit lazy forward reference
    Log.info(s"Recursive reference for ${Type[A].prettyPrint}, emitting SRef") >>
      emitForwardReference[A]
  } else {
    for {
      _ <- inProgress.set(inProgressSet + cacheKey[A])
      result <- deriveStructurally[A](...)
      _ <- inProgress.set(inProgressSet)
    } yield result
  }
}
```

**`MLocal` constructor arguments:**
- `Set.empty[String]` -- initial value
- `identity` -- fork function (child scope inherits parent's set)
- `(a, b) => a ++ b` -- join function (merge parent and child sets)

**Reference:** `tapir-schema-derivation/SchemaMacrosImpl.scala` -- `deriveSchemaRecursively`.

## Configurable derivation timeout

All derivation modules use `DerivationTimeout` from `derivation-commons`. Default: 5
seconds. Override per-module via `-Xmacro-settings`:

```
-Xmacro-settings:jsoniterDerivation.timeout=300s
-Xmacro-settings:catsDerivation.timeout=5m
-Xmacro-settings:circeDerivation.timeout=5000ms
```

Supported formats: plain integer (seconds), `Ns`/`Nseconds`, `Nms`/`Nmilliseconds`,
`Nm`/`Nminutes`. Invalid values emit a compiler warning and fall back to default.

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture, logging
- [kindlings-new-module](../kindlings-new-module/SKILL.md) -- module setup, error hierarchy
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- pitfalls that commonly need debugging
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching pattern
