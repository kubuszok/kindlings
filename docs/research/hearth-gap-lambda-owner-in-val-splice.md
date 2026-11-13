# Hearth gap: a lambda built inside a splice that initialises a local `val` (Scala 3)

## Symptom

`KindlingsEncoder.derived[A]` (which for products returns `deriveEncoderAsObjectTypeClass`) failed on Scala 3 for a
case class with an `Option` of a value class (`case class R4OnlyOpt(maybePlate: Option[R4Plate])`, `R4Plate` an
`AnyVal`) or a `List` of a universal-trait sum, when derived in an `object` with an implicit `Configuration`:

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val macro, val json
```

The generated code was

```scala
val encodeObject: A => JsonObject = (a: A) => {
  val json: Json = ${ /* derived encoding of `a` */ }
  json.asObject match { ... }
}
```

The `LambdaBuilder` lambda of the `Option` rule (`(inner: R4Plate) => encode_R4Plate(inner, cfg)`) was created with an
owner other than `val json`, while the block around it (`val item = a; ...; lambda.apply(item)`) is owned by
`val json`; Scala 3's `-Xcheck-macros` rejects the block.

## Reproducer

`CirceParityRound4Spec` ("a universal-trait sum and a value class in a List / Option (derived in an object)") in the
shared circe-derivation tests: `R4GarageCodecs` derives `KindlingsEncoder.derived[R4Garage]` and
`KindlingsEncoder.derived[R4OnlyOpt]`.

## Workaround in Kindlings

No local `val` around the splice: `CirceDerivationUtils.asJsonObjectOrThrow(${ ... })` (the same issue as the one worked
around in circe's `EncoderHandleAsMapRule`, where the key-encoding lambda is no longer spliced inside the pair lambda).

## Suggested Hearth fix

Lambdas built by `LambdaBuilder` (and other definitions created while a splice is expanded) should be owned by the
splice's owner (`Symbol.spliceOwner` at splice time), or Hearth should change their owner when splicing them.

## Re-checked with Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (2026-11-13)

Status: **still OPEN**, the workaround stays. Hearth's maintainer reports that a minimal version (a `LambdaBuilder`
lambda spliced into a local `val`'s right-hand side) passes in Hearth, so the Kindlings case needs more than that.
Restoring the original shape in `EncoderMacrosImpl.deriveEncoderAsObjectTypeClass`

```scala
val encodeObject: A => JsonObject = (a: A) => {
  val _ = a
  val json: Json = Expr.splice { fromCtx(EncoderCtx.from(Expr.quote(a), Expr.quote(cfg), derivedType = selfType, evConfig)) }
  CirceDerivationUtils.asJsonObjectOrThrow(json)
}
```

still fails (`circeDerivation/testFull`, `CirceParityRound4Spec`, both `R4Garage` and `R4OnlyOpt`), now with the owner
named explicitly:

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val json, val macro
Block: {
  val item$macro$3: R4Plate = a
  (item$macro$3: R4Plate @scala.unchecked) match { case _ => () }
  ((a₂: R4Plate) => {
    val inner$macro$4: R4Plate = a₂
    (inner$macro$4: R4Plate @scala.unchecked) match { case _ => () }
    encode_R4Plate$macro$2(inner$macro$4, configuration$macro$35)
  }).apply(item$macro$3)
}
Definitions owned by val json:  val item$macro$3
Definitions owned by val macro: def $anonfun(a: R4Plate): Json
```

`val macro` is the synthetic owner of the macro expansion itself (`EnvironmentsScala3` treats an owner literally named
`macro` as the splice glue), so the lambda is owned by the expansion's root, while the block that applies it is owned
by `val json`. Ingredients the minimal version probably lacks, all present here: (1) the `val json` is inside a lambda
(`(a: A) => ...`) that is itself inside the outer `Expr.quote`, nested in `ValDefs.createVal(...).use`; (2) the
`Option` rule's `LambdaBuilder` lambda is built while deriving a cached `def` (`encode_R4Plate`) is in progress, i.e.
inside `ValDefsCache` derivation, and spliced through `Expr.splice` of a `fromCtx` result computed in the inner splice;
(3) the element is a value class (`R4Plate <: AnyVal`) whose encoding is a cached def call. A reproducer should start
from `KindlingsEncoder.derived[R4OnlyOpt]` with `case class R4OnlyOpt(maybePlate: Option[R4Plate])`, derived in an
`object` with an implicit `Configuration`, and remove ingredients one at a time.
