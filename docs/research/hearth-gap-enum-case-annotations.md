# Hearth gap: annotations of Scala 3 enum case singleton types (regression)

Status: **OPEN** (found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`, after
kubuszok/hearth#395 made `directChildren` return parameterless enum cases as singleton types). Scala 3 only. Worked
around in Kindlings.

## Symptom

```scala
enum ParitySide {
  @named("0") case Buy
  @named("1") case Sell
}
KindlingsJsonValueCodec.derived[List[ParitySide]] // encodes ["Buy","Sell"], expected ["0","1"]
```

(`JsoniterScala3ParitySpec` "@named on enum cases (NewEnumSpec Side)".)

## Cause

`directChildren` now returns `ParitySide.Buy.type` (a `TermRef`) for the case. `Type#annotations` reads
`untyped.typeSymbol.annotations` (`UntypedTypesScala3.annotations`), and the `typeSymbol` of that `TermRef` is the enum
class `ParitySide`, while `@named("0")` is on the case's TERM symbol (`termSymbol`).

Suggested fix: for a `TermRef` (singleton) type, `annotations`/`annotationTypes` should (also) read
`termSymbol.annotations`.

## Reproducer

```scala
enum E { @deprecated("x") case A }
// Scala 3: Type.of[E.A.type].annotations is empty (the annotations of class E)
```

## Workaround in Kindlings

`derivation-commons`' `EnumCaseAnnotations.of` (Scala 3: the annotations of an enum case's term symbol; Scala 2:
`Nil`) is added to the type annotations looked up by `AnnotationSupport` (`enumCaseAnnotations`) and by
jsoniter-derivation's `JsoniterJsonFieldConfigExtension` (the `@named` subtype names of tapir-schema-derivation's
jsoniter schemas, `JsoniterScala3EnumSchemaSpec`).
