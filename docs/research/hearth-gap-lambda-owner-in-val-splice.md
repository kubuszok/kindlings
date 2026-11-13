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
