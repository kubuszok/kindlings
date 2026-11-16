# Hearth gap: a lambda built inside a splice that initialises a local `val` (Scala 3)

**Status: RESOLVED** in Hearth `90383964` ("Give definitions the owner of the place they end up in (Scala 3)"), first
in `0.4.4-13-g9038396-SNAPSHOT` (2026-11-16). Every variant of `hearth-lambda-owner-repro/` compiles (including
variant M, the regression of the reverted first attempt `d62d007e`), and every Kindlings workaround is removed - see
"Re-checked with Hearth `0.4.4-13-g9038396-SNAPSHOT`" at the end. The history below is kept as a record.

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

## Re-checked with Hearth `0.4.4-10-ga030def-SNAPSHOT` (2026-11-15)

Status: **still OPEN**, the workarounds stay. Hearth `b9454a46` ("Test owners of ValDefs vals created inside a
LambdaBuilder body") only adds tests (`ExprsSpec`: `testNestedValDefsInsideLambdaBuilder` for shape 2,
`testValDefsNextToQuotedValsInsideLambdaBuilder` for shape 1, each built directly inside a `LambdaBuilder.of1(...).map`)
and no change to Hearth itself: those minimal shapes already passed, so they do not cover the Kindlings case.

Restoring shape 2 in `ReaderHandleAsEnumRule.firstSuccess` (a `ValDefs.createVal` per attempt, the next attempt in the
`else` splice, the attempts' results collected with `acc.::(r)` for `noValidCoproductOptionFromResults`) still fails
`pureconfigDerivation/Test/compile` on Scala 3 (`UpstreamSemanticsSpec`, `KindlingsConfigReader.derived[UsSource]`,
lines 122 and 130):

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: method $anonfun, val macro
Block: {
  val either$macro$62: Either[ConfigReaderFailures, UsSource] = PureConfigDerivationUtils.readObjectAs[UsSource](configcursor$macro$32, UsNoSource)
  if (either$macro$62.isRight) either$macro$62 else {
    val either$macro$63: Either[ConfigReaderFailures, UsSource] = read_UsUrlSource$macro$1(configcursor$macro$32, pureconfig$macro$53)
    if (either$macro$63.isRight) either$macro$63 else Left(noValidCoproductOptionFromResults(...))
  }
}
Definitions owned by method $anonfun: val either$macro$63
Definitions owned by val macro: val either$macro$62
```

The difference from Hearth's test: here the chain is built while deriving a cached `def` through `ValDefsCache`
(`read_UsUrlSource` and the enum's own reader) inside `MIO.scoped` - the "fuller structure" the Hearth test's comment
mentions. Shape 1 and the circe
`val json` shape were not retried: with no change in Hearth they cannot have changed either. A Hearth reproducer
should start from `KindlingsConfigReader.derived[UsSource]` with the shape-2 code above.

## Minimal Hearth-only reproducer (2026-11-15, Hearth `0.4.4-10-ga030def-SNAPSHOT`, Scala 3.8.4)

`docs/research/hearth-lambda-owner-repro/` - two scala-cli projects (`macro/Repro.scala`: the macro written with
Hearth's API; `usage/Usage.scala`: the expansions), compiled with `-Xcheck-macros`. Run `./run.sh` from that directory
(it packages `macro` into `macro.jar`, then compiles `usage` against it). No ValDefsCache, MIO or recursive derivation
is needed: it is enough to create the `ValDefs` vals **inside the `Expr.splice` of a quoted lambda**
(`Expr.quote((i: Int) => ... Expr.splice(...))`). Building the same code *outside* the quoted lambda and splicing the
finished `Expr` in (variant `builtOutsideQuotedLambda`) compiles, and so does building it directly in a
`LambdaBuilder.map` body (Hearth's own `b9454a46` tests).

Shape 2 (nested `ValDefs.createVal` inside a quote's splice):

```scala
def chain(remaining: List[Int])(implicit IntT: Type[Int]): Expr[Int] = remaining match {
  case n :: rest =>
    ValDefs.createVal[Int](Expr(n), "attempt").use { r =>
      Expr.quote(if (Expr.splice(r) > 100) Expr.splice(r) else Expr.splice(chain(rest)))
    }
  case Nil => Expr(0)
}

Expr.quote((i: Int) => i + Expr.splice(chain(List(1, 2, 3))))   // fails
```

```
java.lang.AssertionError: assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: method $anonfun, val macro

Block: {
  val attempt$macro$5: scala.Int = 2
  if (attempt$macro$5.>(100)) attempt$macro$5 else {
    val attempt$macro$6: scala.Int = 3
    if (attempt$macro$6.>(100)) attempt$macro$6 else 0
  }
}

Definitions owned by method $anonfun:
val attempt$macro$6: scala.Int = 3

Definitions owned by val macro:
val attempt$macro$5: scala.Int = 2
```

Shape 1 (`ValDefs` vals next to vals the quote defines - `r :: acc` desugars to `{ val elem$1 = r; acc.::(elem$1) }`):

```scala
def bindAll(remaining: List[Expr[Int]], bound: List[Expr[Int]]): Expr[List[Int]] = remaining match {
  case r :: rest => ValDefs.createVal[Int](r, "bound").use(ref => bindAll(rest, ref :: bound))
  case Nil       => bound.reverse.foldRight(Expr.quote(List.empty[Int]))((r, acc) => Expr.quote(Expr.splice(r) :: Expr.splice(acc)))
}

Expr.quote((i: Int) => Expr.splice(bindAll(List(Expr.quote(i), Expr.quote(i + 1)), Nil)))   // fails
```

```
java.lang.AssertionError: assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val macro, method $anonfun

Block: {
  val bound$macro$2: scala.Int = i.+(1)
  val elem$1: scala.Int = bound$macro$1
  {
    val `elem$1₂`: scala.Int = bound$macro$2
    scala.List.empty[scala.Int].::[scala.Int](`elem$1₂`)
  }.::[scala.Int](elem$1)
}

Definitions owned by val macro:
val bound$macro$2: scala.Int = i.+(1)

Definitions owned by method $anonfun:
val elem$1: scala.Int = bound$macro$2
val elem$1: scala.Int = bound$macro$1
```

The same shape-2 chain also fails inside a quoted lambda nested in a `LambdaBuilder` body
(`quotedLambdaInsideLambdaBuilder`) and inside a `LambdaBuilder` body built in the splice of a quoted lambda
(`lambdaBuilderInsideQuotedLambda`). In every failing case the `ValDefs` vals created directly in the lambda's splice
are owned by `val macro` (the expansion's root) while the definitions created in the nested splices (or by the quote
itself) are owned by the lambda (`$anonfun`) - i.e. `ValDefs.createVal` does not use the splice owner of the quoted
lambda it is created in.

## Re-checked with Hearth `0.4.4-11-gd62d007-SNAPSHOT` (2026-11-15)

Hearth `d62d007e` makes `ValDefs` take the current `CrossQuotes.ctx` splice owner even when it is a quoted lambda's
`$anonfun` (it used to fall back to the expansion's owner), and gives `LambdaBuilder` bodies their parameters' owner.

**Fixed:** every variant of `hearth-lambda-owner-repro/` above compiles, and in Kindlings these workarounds could be
removed (Scala 2.13 and 3 compile and pass):

- circe `deriveAsObject`: `val json: Json = ${ ... }` around the splice again;
- circe `EncoderHandleAsMapRule`: non-`String` keys are encoded inside the pair lambda again
  (`jsonFromMappedPairs`; `jsonFromKeyedPairs` is deprecated, kept for 0.3.x-compiled code);
- sconfig `ReaderHandleAsEnumRule.firstSuccess`: one `ValDefs` val per attempt, the next attempt in the `else` splice
  (the unreleased `FirstSuccessAttempts` runtime accumulator is deleted);
- pureconfig `ReaderHandleAsEnumRule.firstSuccess`: the same (blocked from verification by the regression below).

**Regression (OPEN):** `ValDefs` vals nested through `MatchCase.matchOn` case bodies (which is how
`IsEither.fold` folds a Scala `Either` without closures, and so how `EitherFieldsConstruct.buildEitherFailFast`
constructs a case class) inside the splice of a quoted lambda now get two different owners: the outer val, created
directly in the splice, is owned by `$anonfun`, while the val created in the case body's callback is owned by
`val macro`. With `0.4.4-10` both were owned by `val macro` and it compiled. Unchanged pureconfig-derivation code from
`master` (`ReaderHandleAsCaseClassRule.bindAll` + `buildEitherFailFast`) now fails 95 expansions in
`pureconfigDerivation/Test/compile` on Scala 3 (every derived case-class reader), e.g.
`KindlingsConfigReader.derived[(person: SimplePerson, score: Int)]` (`PureconfigScala3Spec.scala:26`):

```
assertion failed: Block contains definition with different owners.
Found definitions 2 distinct owners: val macro, method $anonfun
Block: {
  val either$macro$3: Either[ConfigReaderFailures, Any] = either$macro$1
  (either$macro$3 @unchecked) match {
    case right$macro$1: Right[ConfigReaderFailures, Any] =>
      { val either$macro$4: Either[ConfigReaderFailures, Any] = either$macro$2
        (either$macro$4 @unchecked) match { case right$macro$2: Right[...] => Right(new SimplePerson(...)) ... } }
    case left$macro$2: Left[ConfigReaderFailures, Any] => ...
  }
}
Definitions owned by val macro: left$macro$1, right$macro$2, val either$macro$4
Definitions owned by method $anonfun: left$macro$2, right$macro$1, val either$macro$3
```

Hearth-only reproducer: variant M (`matchCaseChainInsideQuotedLambda`) in `hearth-lambda-owner-repro/`, `./run.sh`:

```scala
def foldAll(remaining: List[Expr[Either[String, Int]]], sum: Expr[Int]): Expr[Either[String, Int]] = remaining match {
  case r :: rest =>
    ValDefs.createVal[Either[String, Int]](r, "either").use { bound =>
      MatchCase.matchOn[Either[String, Int], Either[String, Int]](bound)(NonEmptyVector(
        MatchCase.typeMatch[Right[String, Int]]("right").map(right => foldAll(rest, Expr.quote(Expr.splice(sum) + Expr.splice(right).value))),
        MatchCase.typeMatch[Left[String, Int]]("left").map(left => Expr.quote(Expr.splice(left): Either[String, Int]))
      ))
    }
  case Nil => Expr.quote(Right(Expr.splice(sum)): Either[String, Int])
}
Expr.quote((i: Int) => Expr.splice(foldAll(List(Expr.quote(Right(i): Either[String, Int]), Expr.quote(Right(i + 1): Either[String, Int])), Expr(0))))
```

compiles with `0.4.4-10-ga030def-SNAPSHOT` and fails with `0.4.4-11-gd62d007-SNAPSHOT` ("Found definitions 2 distinct
owners: val macro, method $anonfun": `either$macro$1`/`right$macro$1` owned by `$anonfun`, `either$macro$2`/
`right$macro$2` by `val macro`), while variants D, shape 1, G and H are the other way round. Suggested Hearth fix:
build `MatchCase` case bodies (the `.map` callbacks run by `matchOn`) under the same `CrossQuotes.ctx` as the
`matchOn` call, so vals created in them get the same splice owner as the scrutinee's val - or re-own case bodies to
the scrutinee's owner as `matchOn` already does for the scrutinee.

**Extent of the regression** (`<module>/Test/compile` on Scala 3 with `0.4.4-11-gd62d007-SNAPSHOT`, after `cleanFull`;
everything compiles with `0.4.4-10`):

| Module | Failure |
|---|---|
| pureconfig-derivation | 95 expansions, "different owners" in `buildEitherFailFast` (above) |
| yaml-derivation | 17 expansions, the same (`KindlingsYamlDecoder.fromYamlString[SimplePerson]`, `InlineMethodsSpec.scala:29`) |
| cats-derivation | 53 expansions, "different owners" between `ValDefsCache` entries: cached `def`s (e.g. `eqv_Green`, `compare_Green`) owned by `val macro`, cached `lazy val`s (e.g. `eq_Int`) owned by `$anonfun` (`cats.kernel.Eq.derived[TrafficLight]`, `CatsScala3Spec.scala:83`) |
| cats-tagless-derivation | 7 expansions, the same between two cached `lazy val`s (`KindlingsInvariantK.derived[OuterAlg]`, `examples.scala:46`) |
| integration-tests | 12 expansions (cats + yaml, `CatsYamlSpec.scala:25`) |
| optics | compiler crash in `lambdaLift`: "Could not find proxy for case val x25: SealedFieldSpec.Motorized" (`SealedFieldSpec.scala`) - no `-Xcheck-macros` there, so the wrong owner surfaces as a crash |
| upstream-oracle-tests | compiler crash in `lambdaLift`: "value recon is not visible from <none>" (`oracle/catsderivation/CtSides.scala`) |

All other JVM modules compile.

**Still a workaround, kept until the regression is fixed:** pureconfig `ReaderHandleAsCaseClassRule.bindAll` keeps
`constructedResult`/`unknownKeys` as `ValDefs` vals and builds lists with `acc.::(r)` (restoring the quote-level
`val constructed` + `r :: acc` fails the same way as the unchanged code, so it could not be evaluated either).

## Re-checked with Hearth `0.4.4-13-g9038396-SNAPSHOT` (2026-11-16)

Status: **RESOLVED**. Hearth reverted `d62d007e` (`15b70edb`) and replaced it with `90383964`: `ValDefs` vals and
`MatchCase` case bodies use the current splice owner (the lambda inside its splice), definitions built with
`ValDefBuilder` (`ValDefsCache`) keep the owner of the place the cache is turned into definitions, and `LambdaBuilder`
gives its body the owner of its parameters' vals. Variant M is now a Hearth test.

- `hearth-lambda-owner-repro/` (`./run.sh`): every variant compiles, variant M included.
- All Kindlings JVM modules compile and pass on Scala 3 and 2.13 (the seven modules broken by `0.4.4-11` included).
- Workarounds removed: circe `deriveAsObject` (`val json`), circe `EncoderHandleAsMapRule` (keys encoded inside the
  pair lambda; `jsonFromKeyedPairs` deprecated, kept for 0.3.x-compiled code), sconfig and pureconfig
  `ReaderHandleAsEnumRule.firstSuccess` (one `val` per attempt; the unreleased `FirstSuccessAttempts` deleted),
  pureconfig `ReaderHandleAsCaseClassRule.bindAll` (`val constructed`/`val unknownKeys` in the quote, `r :: acc`), and
  yaml `deriveInlineToYamlString` (`val node = ${ ... }` again, from #298's `toYamlString` of recursive types reached
  through an `Option`).

