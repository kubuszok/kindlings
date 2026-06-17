# `buildEitherFailFast` + nested-case-class field → "Block contains definitions with different owners"

## Status

**Resolved** in Hearth `0.3.1-52` (`MatchCase.matchOn` now re-owns case bodies). All of
`derivation-commons`' `EitherFieldsConstruct.buildEitherFailFast` adopters — circe, yaml, **and
pureconfig** — now compile the zero-allocation fail-fast construction on Scala 2.13 + 3.

## Symptom

`buildEitherFailFast` combines N per-field `Either[E, _]` results into the constructor via nested
`IsEither.fold` (zero-closure `match`), binding each field result to a local `val` first:

```scala
val r0 = <decode field 0>           // Either[E, Any]
r0 match {
  case Right(v0) =>
    val r1 = <decode field 1>
    r1 match { case Right(v1) => Right(new A(v0, v1, …)); case Left(e) => Left(e) }
  case Left(e) => Left(e)
}
```

circe/yaml field decoders return an `Either` directly (`cursor.downField(name).as(decoder)`,
`node.as[Field]`) — they worked from the start. **pureconfig** field decoders wrap the result in an
inline `flatMap` lambda (`cur.asObjectCursor.flatMap(obj => readRequiredField(obj, key, reader))`).
Deriving `KindlingsConfigConvert.derived[PersonWithAddress]` (a case class with a **direct** nested
case-class field `address: Address`) then aborted on Scala 3:

```
assertion failed: Block contains definitions with different owners.
Found definitions 2 distinct owners: method $anonfun, val macro
```

## Root cause

`MatchCase.matchOn` (Scala 3, `ExprsScala3.scala`) changed the **scrutinee** owner to
`Symbol.spliceOwner` but left the **case bodies** (`stripInlined(result.asTerm)`) untouched. A body
that nests definitions built in another context — here a `ValDefs.createVal` whose value contains the
field decoder's inline `flatMap` lambda — kept stale owners, so the spliced `CaseDef` body mixed
`$anonfun`- and `val macro`-owned definitions in one `Block`.

## Fix

Re-own each case body to the splice owner, matching the scrutinee:

```scala
stripInlined(result.asTerm).changeOwner(Symbol.spliceOwner)
```

Hearth commit on branch `foreach-zero-closure-default`; full macro suite green (1152 2.13 + 1280 3 +
sandwich). kindlings bumped to `0.3.1-52`; pureconfig 148 (3) + 133 (2.13), circe 419, yaml 248 green.
