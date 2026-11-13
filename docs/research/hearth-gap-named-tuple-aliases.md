# Hearth gap: named tuples behind type aliases

Status: **RESOLVED** in Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (to be released as 0.4.4): `isNamedTuple` dealiases, so
`NamedTuple.Empty` and named tuples behind type aliases are recognized. `JsoniterScala3ParitySpec` "named tuples behind
type aliases" round-trips them now. The `Tuple22.caseFieldValuesAt` side note below was not reproducible in Hearth (all
22 fields come back on both Scala versions); see the end of this document for the Kindlings side.

Found while porting jsoniter-scala's `JsonCodecMakerNamedTupleSpec` to jsoniter-derivation.

`Type.isNamedTuple` (`hearth/src/main/scala-3/hearth/untyped/UntypedTypesScala3.scala`) compares
`instanceTpe.typeSymbol` / the `AppliedType` constructor with `scala.NamedTuple.NamedTuple` **without dealiasing**, so
`NamedTuple.parse` reports "is not a named tuple" for:

```scala
KindlingsJsonValueCodec.derived[NamedTuple.Empty]                 // alias of NamedTuple[EmptyTuple, EmptyTuple]
type GenericNamedTuple[A, B] = (a: A, b: B)
KindlingsJsonValueCodec.derived[GenericNamedTuple[Option[Int], List[String]]]
type NT23 = (i1: Int, /* ... */ i23: Int)
KindlingsJsonValueCodec.derived[NT23]
```

while the same named tuples written inline derive fine. (`namedTupleComponents` in `UntypedMethodsScala3.scala` already
dealiases, only the detection does not.)

Suggested fix: `isNamedTuple` should test `instanceTpe.dealias` (as `namedTupleComponents` does).

Kindlings test pinning the current behaviour: `JsoniterScala3ParitySpec` "named tuples behind type aliases" (scala-3
test dir, since named tuples are Scala 3-only). When the fix lands, that test fails and should become round-trips.

Related, not investigated further: a recursive tuple alias (`type R = (Int, Option[R])`, upstream
`NamedTupleSpec` "recursive regular and named tuples") is not recognized as a tuple even after `dealias` in Kindlings'
Scala 3 `tupleElementTypes` hook.

Also found while adding tuple support: `CaseClass.parse[Tuple22[...]].caseFieldValuesAt(x)` returns no fields (smaller
tuples are fine). Kindlings' `EncoderHandleAsTupleRule` falls back to `productElement` when the accessor count differs
from the element count.
