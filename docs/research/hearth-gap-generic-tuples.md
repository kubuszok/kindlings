# Hearth gap: `CaseClass` does not decompose Scala 3 generic tuples

Status: **OPEN** (worked around in Kindlings). Found 2026-11-28 with Hearth `0.4.5-18-g99ac2ae-SNAPSHOT`, while adding
tuples with more than 22 elements to pureconfig-derivation and sconfig-derivation.

## What is wrong

On Scala 3, `Type[A].isTuple` is `true` for every subtype of `Tuple` (`EmptyTuple`, `H *: T`, tuples with more than 22
elements, which Scala 3 represents as `*:` chains and builds as `TupleXXL` at runtime), but `CaseClass.parse[A]` only
works for `TupleN` (`(A, B)`, `Tuple2[A, B]`). For `Int *: String *: EmptyTuple` or a 23-element tuple it does not
return `Left` with a reason: it returns a `CaseClass` of the abstract class `*:` whose use fails later with an
exception, `key not found: T2` (the derivation fails with that message instead of a rule yielding).

## Reproducer

With the tuple rules of pureconfig-derivation as they were before generic tuple support (only `CaseClass.parse`):

```scala
type T23 = (Int, String, Int, String, Int, String, Int, String, Int, String, Int, String,
            Int, String, Int, String, Int, String, Int, String, Int, String, Boolean)
KindlingsConfigReader.derived[T23]                          // Macro derivation failed: key not found: T2
KindlingsConfigReader.derived[Int *: String *: EmptyTuple]  // Macro derivation failed: key not found: T2
```

## Workaround in Kindlings

`derivation-commons` `GenericTuples` (`src/main/scala-3/.../GenericTuples.scala`) reads the element types from the
type itself (unfolding `*:` chains down to `EmptyTuple` or a `TupleN`), builds the value with
`Tuple.fromArray(Array[Object](...))` and the module reads the elements with `Product.productElement`. Rules check it
before `CaseClass.parse`. jsoniter-derivation does the same in its own `CodecMacros` (Scala 3). Covered by
`GenericTupleSpec` (scala-3 test dirs of pureconfig-derivation and sconfig-derivation) and `PureConfigScala3OracleSpec`.

## What Hearth could do

Either make `CaseClass.parse` return `Left` for tuples that are not `TupleN` (so that rules can yield cleanly), or
support them as it supports named tuples (a synthetic constructor with `_1`..`_N` parameters built through
`Tuple.fromArray` above 22 elements, as `UntypedMethodsScala3`'s named-tuple constructor already does).
