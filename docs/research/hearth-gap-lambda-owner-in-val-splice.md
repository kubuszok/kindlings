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

## Re-checked with Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (2026-11-14)

Status: **still OPEN** (kubuszok/hearth#402 did not address it), and the workaround stays. Restoring the `val json`
shape above still fails to compile `CirceParityRound4Spec` (lines 23 and 25) on Scala 3 with
"Found definitions 2 distinct owners: val json, val macro".

## Same root cause: `ValDefs` vals inside a `LambdaBuilder` body (pureconfig-derivation, 2026-11-15)

Status: **OPEN**, worked around. Hearth `0.4.4-6-gb8ae5aa-SNAPSHOT`, Scala 3.8.4 only (Scala 2.13 compiles).

Definitions created with `ValDefs.createVal(...).use` while the code being built ends up inside a `LambdaBuilder`
lambda are owned by `val macro` (the expansion's root), so a block that also holds definitions with the right owner
(`$anonfun` of the lambda) is rejected. Two shapes hit it in `ReaderHandleAsCaseClassRule` /
`ReaderHandleAsEnumRule`, both reached when the type is derived inside the field-reader `LambdaBuilder` of an
enclosing type (e.g. a named tuple `(person: SimplePerson, score: Int)` whose field `person` is a case class).

### Shape 1: `ValDefs` vals next to vals the quote itself defines

```scala
def bindAll(remaining: List[Expr[Either[E, Any]]], bound: List[Expr[Either[E, Any]]]): Expr[Either[E, A]] =
  remaining match {
    case r :: rest => ValDefs.createVal[Either[E, Any]](r).use(ref => bindAll(rest, ref :: bound))
    case Nil =>
      val results = bound.reverse.foldRight(Expr.quote(List.empty[Either[E, Any]])) { (r, acc) =>
        Expr.quote(Expr.splice(r) :: Expr.splice(acc)) // Scala 3 desugars `x :: xs` into `val elem$1 = x; xs.::(elem$1)`
      }
      Expr.quote {
        val constructed = Expr.splice(buildEitherFailFast(bound.reverse)(construct)) // holds `ValDefs` vals too
        if (constructed.isRight) constructed else Left(combine(Expr.splice(results)))
      }
  }
```

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val macro, method $anonfun
Block: {
  val either$macro$5: Either[ConfigReaderFailures, SimplePerson] = { val either$macro$3 = either$macro$1; ... }
  if (either$macro$5.isRight) either$macro$5 else Left(combineFieldFailures({
    val elem$1: Either[ConfigReaderFailures, Any] = either$macro$1
    { val elem$1₂ = either$macro$2; List.empty[...].::(elem$1₂) }.::(elem$1)
  }, None))
}
Definitions owned by val macro: val either$macro$5 ...
Definitions owned by method $anonfun: val elem$1, val elem$1
```

(The first attempt, with `val constructed` written in the quote, failed the same way with `val constructed` owned by
`$anonfun` and the `ValDefs` vals inside it owned by `val macro`.)

### Shape 2: nested `ValDefs.createVal` inside a quote's splice

```scala
def attempt(remaining: List[Child], failed: List[Expr[Result[A]]]): Expr[Result[A]] = remaining match {
  case child :: rest =>
    ValDefs.createVal[Result[A]](child.read(cur)).use { result =>
      Expr.quote(if (Expr.splice(result).isRight) Expr.splice(result) else Expr.splice(attempt(rest, result :: failed)))
    }
  case Nil => Expr.quote(Left(noValidOption(...)))
}
```

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val macro, method $anonfun
Block: {
  val either$macro$62: Either[ConfigReaderFailures, UsSource] = readObjectAs[UsSource](configcursor$macro$39, UsNoSource)
  if (either$macro$62.isRight) either$macro$62 else {
    val either$macro$63: Either[ConfigReaderFailures, UsSource] = read_UsUrlSource$macro$1(configcursor$macro$39, ...)
    ...
  }
}
Definitions owned by val macro: val either$macro$62
Definitions owned by method $anonfun: val either$macro$63
```

(The outer val is owned by `val macro`, the one created while expanding the `else` splice by the lambda.)

### Workarounds in Kindlings

- Shape 1 (`pureconfig-derivation` `ReaderHandleAsCaseClassRule`, `decodeCaseClassFields`): every val is created with
  `ValDefs` (`constructedResult`, `unknownKeys`), none in the quote, and lists are built as `acc.::(r)` instead of
  `r :: acc` (no desugared `elem$` vals).
- Shape 2 (`ReaderHandleAsEnumRule.firstSuccess`): no val per attempt; a runtime accumulator
  (`PureConfigDerivationUtils.FirstSuccessAttempts`, one quote-level `val attempts`) records the attempts in an
  `if (attempts.succeeded(read(cur))) attempts.result else ...` chain.
- Reproducers: `PureconfigScala3Spec` (named tuple with a case-class field) and `UpstreamSemanticsSpec`
  ("FirstSuccessCoproductHint" group) in pureconfig-derivation; restoring either shape fails their compilation on
  Scala 3.
