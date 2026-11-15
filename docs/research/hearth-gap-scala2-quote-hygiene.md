# Hearth gap: Scala 2 `Expr.quote` output is not hygienic (unrooted package paths)

**Status:** open in Hearth (found 2026-11-15 by `upstream-oracle-tests`). Scala 2.13 only; Scala 3 quotes are hygienic.

## Symptom

Any Kindlings derivation expanded inside a scope that has a member named like a top-level package fails on Scala 2.13:

```scala
object Codecs {
  implicit val circe: Configuration = Configuration.default
  def scala: Encoder[Person] = KindlingsEncoder.derived[Person] // stable identifier required, but Codecs.this.scala found
  def cats: Schema[WithNel] = KindlingsSchema.derived[WithNel].schema // value data is not a member of Schema[WithNel]
}
```

Dangerous names: `scala` (almost every expansion), `java`, `hearth`, and the first segment of any library package the
macro's quotes mention (`io` for circe, `cats`, `sttp`, ...), plus `scala` sub-packages written short (`math`).

## Cause

On Scala 2, Hearth's cross-quotes print the typed quote body back to source (`showCodePretty`) and parse it again at the
expansion site (`hearth-cross-quotes/src/main/scala-2/hearth/cq/CrossQuotesMacros.scala`, `convert`). Package paths are
printed without `_root_.`: term paths through packages (`scala.Some`, `java.util.UUID`, `cats.data...` —
`qualifyStaticModuleRefs` only roots static modules), types written in the quote (`TypeTree.original` is not visited)
and inferred type arguments (`printTypePrefix` in `ShowCodePrettyScala2`). When re-parsed, an enclosing member captures
the first segment.

Kindlings builds no code from strings; the paths come only from what it writes inside `Expr.quote`.

## Reproducer and fix

A Hearth-only reproducer (`CrossQuotesHygieneSpec`, enclosing `def scala`, `java`, `hearth` and a package stand-in) and
a fix (root package paths in `qualifyStaticModuleRefs`, visit `TypeTree.original`, print with `printRootPkg = true`,
root top-level packages in `printTypePrefix`) were handed to Hearth; Hearth's Scala 2.13 suite passes with the fix.

## Kindlings follow-up

When a Hearth release contains the fix: add a shared regression test (Scala 2.13 + 3) deriving circe, jsoniter and
tapir instances inside an object with members named `scala`, `java`, `hearth`, `io`, `cats`, `sttp`.
