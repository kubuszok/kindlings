# Avro derivation of generic enums / sealed traits

Two distinct defects observed when a **generic** sum type (one value case carrying the
type parameter + one singleton case) meets the `avro-derivation` module. The canonical
shape:

```scala
sealed trait Updatable[+A]
object Updatable {
  final case class SetUpdate[A](value: A) extends Updatable[A]
  case object Keep extends Updatable[Nothing]
}
final case class UpdContent(text: String)
final case class UpdRecord(field: Updatable[UpdContent])
```

## Defect 1 — mixed-union decoder ignores the applied type parameter (FIXED)

**Symptom (runtime):**

```
java.lang.IllegalArgumentException: Unknown Avro record type: SetUpdate__UpdContent.
  Expected one of: SetUpdate, Keep
```

**Cause.** For a mixed sealed trait / enum, the schema + encoder name each child record
via `SchemaForMacrosImpl.computeAvroNameExpr`. For a *generic* case that name embeds the
applied type parameters (`SetUpdate[UpdContent]` → `SetUpdate__UpdContent`) and honours
`@avroName` / `@avroErasedName` / `@avroFqnParamNames`. The decoder's mixed-union branch
(`AvroDecoderHandleAsEnumRule`) instead dispatched on the *plain simple class name*
(`SetUpdate`) after passing it through `config.transformConstructorNames`, so the incoming
record name (`SetUpdate__UpdContent`) never matched any branch and fell through to
`failedToMatchSubtype`. The transform was also wrong on a second axis: record names in a
union are **not** passed through `transformConstructorNames` on the encoder side (only
enum-of-case-object *symbols* are).

**Fix.** `AvroDecoderHandleAsEnumRule`'s mixed-union branch now recomputes each child's
expected record name with the exact same `computeAvroNameExpr[ChildType]` the schema uses
(constructing a throwaway `SchemaForCtx` from the decoder ctx), and compares it directly
to `record.getSchema.getName` — no `transformConstructorNames`, no separate `@avroName`
special-casing (subsumed by `computeAvroNameExpr`). Regression test:
`AvroRoundTripSpec` → "generic sealed trait field with a value case round-trip"; example
types `Updatable` / `UpdContent` / `UpdRecord` in `examples.scala`. Verified on Scala
2.13 + 3 JVM.

## Defect 2 — `derives` directly on the generic enum → infinite recursion (FIXED)

**Symptom.** Putting the derivation on the generic type itself, Scala 3 only:

```scala
enum Updatable[+A] derives AvroEncoder, AvroDecoder:
  case SetUpdate(value: A)
  case Keep
```

The Scala 3 compiler emits **`Infinite loop in function body`** at the `derives` clause,
and the generated `derived$AvroEncoder` / `derived$AvroDecoder` self-recurse at runtime
(`StackOverflowError` in `Updatable$.derived$AvroEncoder`).

**Root cause.** `AvroEncoder[A]` and `AvroDecoder[A]` both **extend `AvroSchemaFor[A]`**.
`derives AvroEncoder, AvroDecoder` on `Updatable[A]` puts two synthetic companion givens
into scope, `derived$AvroEncoder: AvroEncoder[Updatable[A]]` and
`derived$AvroDecoder: AvroDecoder[Updatable[A]]` — each of which *is-a*
`AvroSchemaFor[Updatable[A]]`.

The encoder/decoder embed their Avro schema by calling `deriveSelfContainedSchema[Updatable[A]]`,
which ran the schema rule chain with `derivedType = None`. With `None`,
`AvroSchemaForUseImplicitWhenAvailableRule` does **not** skip the implicit search for the
*root* type, so it summons `AvroSchemaFor[Updatable[A]]`, finds the enclosing
`derived$AvroEncoder` given (a subtype), and emits `derived$AvroEncoder(...).schema` — the
instance being defined summons its own schema. For a generic root this is a runtime
`StackOverflowError`; the non-generic case was masked by the compiler's implicit-divergence
check.

The `value: A` field itself was fine: `derives` supplies `using AvroEncoder[A]` /
`using AvroDecoder[A]`, and because those extend `AvroSchemaFor[A]` the field's schema is
correctly taken from that supplied instance. Only the *root* schema summon looped.

**Fix.** `deriveSelfContainedSchema[B]` now passes `derivedType = Some(Type[B].as_??)`
(`SchemaForMacrosImpl.scala`), so the root type is derived structurally and never resolves
to an instance of itself — while nested field/child types (whose type differs from `B`)
still resolve implicits normally and reuse an in-scope `AvroEncoder`/`AvroDecoder` as their
schema source. This mirrors `deriveSchemaForTypeClass`, the real `AvroSchemaFor.derived`
entry point, which already passed `selfType`; `deriveSelfContainedSchema`'s `None` was the
lone inconsistency.

**Why `derivedType`, not the `summonExprIgnoring` ignore-list?** The looping given is the
*synthetic per-companion* `Updatable$.derived$AvroEncoder`, not the module method
`AvroEncoder.derived`. `ignoredImplicits` filters known auto-derivation **methods** on
`AvroEncoder.type` / `AvroDecoder.type` / `AvroSchemaFor.type`; it cannot enumerate a
companion given the compiler synthesises per `derives` site, so it would not catch this.
Blanket-ignoring `AvroEncoder`/`AvroDecoder` as schema sources would also be wrong — nested
fields (including the `value: A` evidence) *must* be able to source their schema from an
in-scope encoder/decoder. "Don't resolve the schema of the exact type currently being
derived to another instance of itself" is precisely what `derivedType` expresses, and it is
already the mechanism the encoder/decoder bodies use
(`AvroEncoderUseImplicitWhenAvailableRule` / `AvroDecoderUseImplicitWhenAvailableRule`).

**Tests.** `AvroScala3Spec` → "`derives` directly on a generic enum" group
(`Updatable3`/`Content3`/`Record3` in `scala3examples.scala`): standalone generic-enum
round-trip and generic-enum-nested-in-a-record round-trip, both with `derives` on the type.
Verified on Scala 3 JVM (the `derives` clause is Scala-3-only). The shared
`AvroRoundTripSpec` "generic sealed trait field" test from Defect 1 continues to exercise
the structural (non-`derives`) path on both 2.13 + 3.
