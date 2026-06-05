# Hearth Bug: `Method.fold` fails for default values of generic case classes on Scala 3

**Hearth version**: 0.3.0-92-g1988265-SNAPSHOT (0.4.0 branch)
**Affects**: Scala 3 only (Scala 2.13 works)
**Discovered**: 2026-06-05

## Symptom

Macro derivation fails at compile time with (after latest Hearth fix for eta-expansion):

```
Macro derivation failed with the following errors:
  - 
      Expected type: scala.Nothing
      Actual type: scala.Predef.String @scala.annotation.unchecked.uncheckedVariance
      Expression: circederivation.BoxWithDefault.$lessinit$greater$default$2[scala.Int]
```

Previously (before the eta-expansion fix):
```
  - Expected an expression. This is a partially applied Term. Try eta-expanding the term first.
```

when deriving a type class for a **generic** case class that has a default value on
a **non-generic** field:

```scala
case class BoxWithDefault[A](value: A, label: String = "unlabeled")

// This fails:
KindlingsDecoder.decode[BoxWithDefault[Int]](json)
```

Non-generic case classes with the same default pattern work fine:

```scala
case class PersonWithDefaults(name: String, age: Int = 25)

// This works:
KindlingsDecoder.decode[PersonWithDefaults](json)
```

## Root Cause Analysis

In Hearth 0.4.0, `Parameter.defaultValue` returns `Option[Method]` where `Method`
is a builder chain (`OnInstance → ApplyTypes → ApplyValues → Result`).

For a generic case class `BoxWithDefault[A]`, the default value method for `label`
is the compiler-generated `BoxWithDefault$default$2[A]()`. When the case class is
instantiated as `BoxWithDefault[Int]`, this method needs:

1. **Type application**: `A = Int` (the `ApplyTypes` step)
2. **No value arguments**: empty `ApplyValues`
3. **Result**: the expression `"unlabeled"`

The current `method.fold(...)` call in kindlings passes `onTypes = _ => Map.empty`
(empty type arguments), but the method chain expects the type parameter `A` to be
resolved. On Scala 3, the resulting expression is a "partially applied Term" because
the type parameter was not supplied.

For non-generic `PersonWithDefaults`, the default value method
`PersonWithDefaults$default$2()` has no type parameters, so `ApplyTypes` is never
needed and `fold` works correctly.

## Reproducer

### Minimal types (already in kindlings test suite)

```scala
// works — no type parameters
case class PersonWithDefaults(name: String, age: Int = 25)

// fails — type parameter A, even though default is on non-generic field
case class BoxWithDefault[A](value: A, label: String = "unlabeled")
```

### Macro code triggering the bug

In any decoder rule that uses default values (e.g. `DecoderHandleAsCaseClassRule`):

```scala
param.defaultValue.flatMap { method =>
  method.fold(
    onInstance = _ => throw new RuntimeException("Default value should not need instance"),
    onTypes = _ => Map.empty,   // ← BUG: needs actual type args for generic classes
    onValues = _ => Map.empty
  ).toOption.map { ee => import ee.Underlying; ee.value.upcast[Any] }
}
```

The `onTypes = _ => Map.empty` line is the problem. For `BoxWithDefault[Int]`,
the `ApplyTypes` step receives empty type arguments, so the type parameter `A`
remains unresolved. The default value method is `$lessinit$greater$default$2[A](): String`
— it needs `A = Int` supplied in the `ApplyTypes` step, but `Map.empty` provides nothing.

After the latest Hearth fix (eta-expansion), the method evaluates but returns
type `Nothing` (unresolved type var) instead of `String`, causing a type mismatch.

### Expected fix

`Parameter.defaultValue` should return a `Method` that already has type parameters
resolved (since the `Parameter` comes from a fully-applied `CaseClass[BoxWithDefault[Int]]`),
OR `Method.fold` should handle the case where type parameters can be inferred from
the instance type.

### Update: Hearth already resolves type params — bug is in return type

`Parameter.defaultValue` (Methods.scala lines 51-60) already handles `ApplyTypes`:
```scala
case at: Method.ApplyTypes =>
  val instanceTypeArgs = UntypedType.typeArguments(untypedInstanceType)
  val typeArgs = at.typeParams.flatten.zip(instanceTypeArgs)...
  at.apply(typeArgs)
```

So the returned `Method` should be past the `ApplyTypes` step with `A = Int` resolved.
But `Method.Result.build()` still fails: it expects return type `Nothing` instead of
`String`. The type parameter resolution in `apply(typeArgs)` doesn't update the
expected return type (`knownReturning`) of the resulting `Method.Result`.

The `build()` call in `fold` validates `expectedType =:= actualType` — `Nothing`
(unresolved) vs `String` (actual) fails.

## Workaround

None currently. The test is present in the circe-derivation test suite
(`KindlingsDecoderSpec` line 1152) and will fail on Scala 3 until fixed.

## Impact

Affects ALL decoder modules (circe, jsoniter, yaml, xml, pureconfig, sconfig, avro,
ubjson) when deriving for generic case classes with default values. Currently only
tested in circe.
