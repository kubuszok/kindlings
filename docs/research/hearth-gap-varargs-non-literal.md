# Hearth gap: `VarArgs.toIterable` cannot tell "no varargs" from "non-literal varargs" (Scala 3)

Status: **RESOLVED** by kubuszok/hearth#389 (Hearth snapshot `0.4.3-17-gbd1e277-SNAPSHOT`, `@since 0.4.4`). Found
2026-10-25 with Hearth 0.4.3, while trying to replace the `scala.quoted.Varargs` match in
`optics/src/main/scala-3/.../ModifyMacros.scala` with Hearth's `VarArgs`.

Resolution: Hearth added `VarArgs.toIterableOption[A](args: VarArgs[A]): Option[Iterable[Expr[A]]]`, which returns
`Some(elements)` for literal varargs and `None` for a splatted sequence on both platforms (Scala 3 via
`scala.quoted.Varargs.unapply`, Scala 2 by detecting the single `Typed(_, Ident(WILDCARD_STAR))` argument);
`toIterable` is unchanged. Kindlings optics now takes the extra paths as `VarArgs[P]` in the shared
`ModifyMacrosImpl.modifyAll`/`modifyAllLens` and rejects a splat via `toIterableOption(...).getOrElse(abort(...))`
("`modifyAll` expects an explicit list of paths, ..."), so the Scala 3-only `Varargs` match is gone and Scala 2 now
reports the same error for `paths: _*` (it used to hand the splat to the path parser as if it were a path). Covered by the shared
`optics/src/test/scala/.../ModifyAllSplatSpec.scala` and the Scala 3 `ModifyAllVarargsSpec.scala` (`paths*` syntax).
The sections below describe the original gap.

## What is wrong

On Scala 3, `VarArgs[A] = Expr[Seq[A]]`, and (`hearth/src/main/scala-3/hearth/typed/ExprsScala3.scala:1505`):

```scala
override def toIterable[A](args: VarArgs[A]): Iterable[Expr[A]] = scala.quoted.Varargs.unapply(args).getOrElse(Nil)
```

A splatted, non-literal sequence (`f(xs*)`) cannot be split into elements, and it comes back as `Nil`, the same as an
empty argument list. A macro that walks its varargs with `VarArgs.toIterable` therefore **silently ignores** every
splatted argument instead of reporting it. On Scala 2, `VarArgs[A] = Seq[Expr[A]]`, and a splat arrives as one
`xs: _*` element, which the macro can at least see.

## Reproducer

`optics/src/test/scala-3/hearth/kindlings/optics/ModifyAllVarargsSpec.scala`:

```scala
val more: Seq[OpticsContext ?=> (Names => String)] = Seq(_.second)
Names("a", "b").modifyAll(_.first, more*).setTo("x")
```

With the current `Varargs(es)` match this fails to compile with
"`modifyAll` expects an explicit list of paths, got: more" (the test passes). We swapped the bridge's `varargs` body
for `new ModifyMacros(quotes).VarArgs.toIterable(paths).toList` and ran
`sbt --client "optics/testOnly hearth.kindlings.optics.ModifyAllVarargsSpec"`. The snippet then **compiles without
any error**: `compileErrors` returns an empty message, so `modifyAll` would modify only `first` and silently drop
`second`:

```
==> X hearth.kindlings.optics.ModifyAllVarargsSpec.modifyAll with splatted (non-literal) varargs / a splatted path
      sequence is rejected instead of silently ignored  java.lang.AssertionError: assertion failed: Error message did
      not contain expected snippet
Error message:

Expected Snippet:
`modifyAll` expects an explicit list of paths
[error] Failed: Total 1, Failed 1, Errors 0, Passed 0
```

After the experiment we restored the `Varargs` match.

## Current code it forces

`optics/src/main/scala-3/hearth/kindlings/optics/internal/compiletime/ModifyMacros.scala`, `private def varargs`:
a raw `scala.quoted.Varargs(es)` match whose fallback is `report.errorAndAbort`.

## Wanted

One of:

```scala
trait VarArgsModule {
  /** `Some(elements)` when the varargs were written out literally, `None` for a splatted non-literal `Seq` (Scala 3). */
  def toIterableOption[A](args: VarArgs[A]): Option[Iterable[Expr[A]]]
}
```

or make `toIterable` abort with a positioned error instead of returning `Nil` for the non-literal case. With that, the
optics bridge can call `VarArgs.toIterableOption(paths).getOrElse(abort(...))`, and the Scala 3-only `Varargs` import
goes away.
