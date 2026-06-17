# `buildEitherFailFast` + nested-case-class field → "Block contains definitions with different owners"

## Status

Open. Blocks adopting the zero-allocation fail-fast field construction
(`derivation-commons` `EitherFieldsConstruct.buildEitherFailFast`) in **pureconfig**. circe and
yaml adopt it fine; pureconfig is left on the legacy `List` + `sequenceResults` path.

## What works vs. what fails

`buildEitherFailFast` combines N per-field `Either[E, _]` results into the constructor via nested
`IsEither.fold` (zero-closure `match` on Hearth ≥ 0.3.1-51), binding each field result to a local
`val` first:

```scala
val r0 = <decode field 0>           // Either[E, Any]
r0 match {
  case Right(v0) =>
    val r1 = <decode field 1>
    r1 match { case Right(v1) => Right(new A(v0, v1, …)); case Left(e) => Left(e) }
  case Left(e) => Left(e)
}
```

- **circe / yaml** — field decoders return an `Either` directly
  (`cursor.downField(name).as(decoder)`, `node.as[Field]`). The scrutinee `rN` is a clean value.
  Works on Scala 2.13 + 3.
- **pureconfig** — field decoders wrap the result in a `flatMap` lambda:
  `cur.asObjectCursor.flatMap(obj => readRequiredFieldWithSuggestions(obj, "address", reader))`.
  When that expression is bound to `val rN` and matched on inside the nested fold, Scala 3 aborts:

  ```
  assertion failed: Block contains definitions with different owners.
  Found definitions 2 distinct owners: method $anonfun, val macro
  Block: {
    val either$macro$7: Either[ConfigReaderFailures, Any] = {
      val cur: ConfigCursor = configcursor$macro$4
      cur.asObjectCursor.flatMap((obj: ConfigObjectCursor) => …readRequiredField…)
    }
    (either$macro$7 …) match { … }
  }
  ```

  The `flatMap` lambda's internal definitions keep their `$anonfun` owner while the surrounding
  `val`/match binders are owned by `val macro`; the two land in one `Block` and the owner-consistency
  assertion fires. Binding to a `val` (as above) did **not** help — the lambda is inside the val RHS.

## Reproducer

`pureconfig-derivation` test `KindlingsConfigConvertSpec` →
`KindlingsConfigConvert.derived[PersonWithAddress]` (a case class with a **direct** nested
case-class field `address: Address`). Re-apply `EitherFieldsConstruct.buildEitherFailFast` to
`ReaderHandleAsCaseClassRule` (see the circe/yaml wiring for the shape) and the derivation fails to
compile on Scala 3 with the assertion above. Collections/maps of nested case classes are fine —
only a *direct* nested case-class field whose decoder introduces a lambda triggers it.

## Hypotheses / next steps

- Likely a tree-owner-fixup gap when a `MatchCase` scrutinee (or a `ValDefs.createVal` RHS) contains
  a lambda with its own definitions — candidate Hearth fix in `MatchCase` / `ValDefs` owner handling
  (consistent with "fix it in Hearth"). Minimal Hearth-level repro still TODO.
- Alternative kindlings-side shapes to try: normalize the field-decode expr so the lambda is hoisted
  to its own cached `def` (no inline lambda in the scrutinee); or special-case direct nested
  case-class fields to a non-`MatchCase` construction.

Until resolved, pureconfig keeps the `List`+`sequenceResults`+`Array[Any]` construction.
