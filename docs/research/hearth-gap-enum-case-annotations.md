# Hearth gap: annotations of Scala 3 parameterless enum cases (regression)

Status: **RESOLVED** in Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (kubuszok/hearth#402, to be released as 0.4.4):
`annotations` / `annotationTypes` of a parameterless enum case read the case's own (term) symbol. Kindlings' workaround
is removed. Scala 3 only.

Found 2026-11-13 while bumping Kindlings to Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT`, after kubuszok/hearth#395 made
`directChildren` return parameterless enum cases as singleton types.

## Symptom

```scala
enum ParitySide {
  @named("0") case Buy
  @named("1") case Sell
}
KindlingsJsonValueCodec.derived[List[ParitySide]] // encoded ["Buy","Sell"], expected ["0","1"]
```

(`JsoniterScala3ParitySpec` "@named on enum cases (NewEnumSpec Side)", and the `@named` subtype names of
tapir-schema-derivation's jsoniter schemas, `JsoniterScala3EnumSchemaSpec`.)

## Cause

`directChildren` returned `ParitySide.Buy.type` (a `TermRef`) for the case. `Type#annotations` read
`untyped.typeSymbol.annotations` (`UntypedTypesScala3.annotations`). The `typeSymbol` of that `TermRef` is the enum
class `ParitySide`, while `@named("0")` is on the case's TERM symbol. So the case got the enum's annotations.

The regression is the `directChildren` path: on `0.4.3-24-gc0c6e43-SNAPSHOT` children were `TypeRef`s on the case's
symbol, and kubuszok/hearth a97ecd88 made them `TermRef`s. The written singleton type (`Type.of[E.A.type]`) never
returned the case's annotations, on 0.4.3-24 either. Reproducer shape (Hearth `EnumCaseChildrenSpec`, both forms fixed
in #402):

```scala
@ExampleAnnotation2(0) enum E {
  @ExampleAnnotation case A
  @ExampleAnnotation2(2) case B(a: Int)
}
// Type[E].directChildren("A").annotations was the enum's: @SourceFile(...), @Child[...], ..., @ExampleAnnotation2(0)
```

## Former workaround in Kindlings (removed)

`derivation-commons`' `EnumCaseAnnotations.of` (Scala 3: the annotations of an enum case's term symbol; Scala 2: `Nil`)
was added to the type annotations looked up by `AnnotationSupport` and by jsoniter-derivation's
`JsoniterJsonFieldConfigExtension`. With 0.4.3-90 both use `Type#annotations` / `annotationsOfType` alone, and the
tests above pass.
