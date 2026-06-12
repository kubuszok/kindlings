# Hearth gap: no typed cross-platform annotation extraction

Last updated: 2026-06-12. Status: **workaround in place** (shared `AnnotationSupport` in
`derivation-commons`); should be deleted once Hearth provides the API.
Filed as [kubuszok/hearth#283](https://github.com/kubuszok/hearth/issues/283).

## Problem

Hearth has no cross-platform API for reading annotations off constructor parameters or
types and extracting literal arguments from them. Worse, the API that *does* exist is
lossy on Scala 2:

- `param.asUntyped.annotations` on Scala 2 runs the annotation trees through
  `c.untypecheck`, which **strips `tree.tpe`** — making it impossible to match an
  annotation by type (`ann.tree.tpe =:= annTpe` sees `null`).
- There is no helper for extracting literal arguments, and the tree shapes differ per
  platform: `Apply(_, List(Literal(Constant(v: String))))` on Scala 2 vs
  `Apply(_, List(Literal(StringConstant(v))))` on Scala 3.

## Impact in Kindlings

Before deduplication, **12 modules** (avro, cats, circe, fast-show-pretty, jsoniter,
pureconfig, sconfig, tapir-schema, ubjson, xml, yaml derivation + the field-config
extensions) each carried a hand-rolled `AnnotationSupportScala2` / `AnnotationSupportScala3`
pair — ~860 lines of copy-pasted platform-specific code in total. It now lives once in
`derivation-commons/src/main/scala{,-2,-3}/hearth/kindlings/derivation/compiletime/AnnotationSupport*.scala`,
but it is still platform-specific code that every Hearth user with annotation-driven
derivation will have to rediscover and rewrite.

## Reproducer

Given a user-side annotation and case class:

```scala
final class fieldName(val name: String) extends scala.annotation.StaticAnnotation
final case class Person(@fieldName("first_name") firstName: String)
```

Macro-side, with only Hearth's API:

```scala
// Goal: find @fieldName on the parameter and read out "first_name".
val param: Parameter = ??? // from CaseClass.parse(...).primaryConstructor.parameters

// Scala 3: works (terms keep their types)
param.asUntyped.annotations.find(_.tpe =:= UntypedType.fromTyped[fieldName]) // Some(term)

// Scala 2: NEVER matches — c.untypecheck strips tree.tpe, so tpe is null
param.asUntyped.annotations // trees with tpe == null
```

The Scala 2 workaround has to bypass Hearth and read the raw symbol:

```scala
// Scala 2 only — note the direct c.universe access
param.asUntyped.symbol.annotations.collectFirst {
  case ann if ann.tree.tpe =:= annTpe => c.untypecheck(ann.tree)
}
```

And literal extraction must be duplicated per platform because the constant tree shapes
differ (see `AnnotationSupportScala2.scala` vs `AnnotationSupportScala3.scala`).

## Proposed Hearth API

```scala
// On Parameter and on Type (type-level annotations):
def Parameter.annotationsOfType[Ann: Type]: List[UntypedExpr]   // typed matching, both platforms
def Type.annotationsOfType[Ann: Type, A: Type]: List[UntypedExpr]

// Literal extraction helpers (cross-platform tree matching):
def UntypedExpr.literalArg[T]: Option[T]          // single-literal annotations
def UntypedExpr.literalArgs: List[Any]            // positional literal args
```

Requirements:

1. On Scala 2, annotation trees must keep their types (match before any `untypecheck`).
2. Literal extraction must understand both `Constant(v)` (Scala 2) and
   `StringConstant`/`IntConstant`/... (Scala 3).
3. Should work on both `Parameter` (constructor params / fields) and types
   (`typeSymbol.annotations`).

## Reference

- Kindlings shared workaround: `derivation-commons/.../compiletime/AnnotationSupport.scala`
  (+ `Scala2`/`Scala3` siblings) — the abstract method set there is effectively the API
  surface 12 real modules needed.
- Original per-module copies: git history of e.g.
  `circe-derivation/src/main/scala-2/.../AnnotationSupportScala2.scala`.
