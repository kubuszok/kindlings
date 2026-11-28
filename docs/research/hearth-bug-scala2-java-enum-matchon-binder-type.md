# Hearth bug: `Enum.matchOn` over a Java enum gives a matched value typed as the enum, not the constant (Scala 2)

**Status:** OPEN (Hearth `0.4.5-18-g99ac2ae-SNAPSHOT`, Scala 2.13 only). Worked around in fast-show-pretty
(commit 70b87f08, "Java enum constants rendered in the enum rule"): the enum rule no longer passes the matched value of
a Java enum to the helper derived for the constant's type.

## Symptom

Before 70b87f08, `FastShowPretty.derived[java.nio.file.AccessMode]` on 2.13 failed with

```
type mismatch;
 found   : READ$macro$1.type (with underlying type java.nio.file.AccessMode)
 required: java.nio.file.AccessMode(READ)
```

The expanded code (`logDerivationForFastShowPretty`, full log in
`repro-details/fast-show-pretty-2_13-accessmode-before-70b87f08.txt`):

```scala
accessmode match {
  case (READ @ READ) => {
    ...
    render_READ(stringbuilder, renderconfig, int.+(1), READ).append("): ").append("AccessMode")
  }
  ...
}
// render_READ: the cached helper derived for the child type, `value: java.nio.file.AccessMode(READ)`
```

## Cause

`matchOn` hands out `matched.value: Expr[AccessMode(READ)]`, but its tree is the binder of
`case READ$macro$1 @ READ =>` (`Classes.matchCaseFor` -> `Expr.singletonOf` gives `Literal(Constant(<READ>))` ->
`MatchCase.eqValue`, whose matched expression is `q"$name"`). Scala 2 types a binder over a literal pattern with the
widened type, so the value is an `AccessMode`. Using it as `Any` (`.toString`) compiles; passing it where the child type
is expected does not. The shape of the scrutinee does not matter.

Possible fix: make the matched expression of `EqValue` `q"$name.asInstanceOf[$A]"` (a no-op after erasure), or hand out
the singleton expression itself.

## Minimal standalone reproducer (Hearth only, Scala 2.13.18)

`macro/Repro.scala` (with `//> using dep com.kubuszok::hearth:0.4.5-18-g99ac2ae-SNAPSHOT`, the snapshots repository and
`scala-reflect`):

```scala
class ReproMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {
  import c.universe.Quasiquote

  private def go[A: c.WeakTypeTag](value: c.Expr[A], asChild: Boolean)(implicit
      S: c.WeakTypeTag[String]
  ): c.Expr[String] = {
    val enumm = Enum.parse[A].toEither.fold(e => c.abort(c.enclosingPosition, e.toString), identity)
    enumm
      .matchOn[hearth.fp.effect.MEval, String](value.asInstanceOf[Expr[A]]) { matched =>
        val childTpe = matched.Underlying.asInstanceOf[c.WeakTypeTag[Any]].tpe
        val v = matched.value.asInstanceOf[c.Expr[Any]].tree
        val body = if (asChild) q"_root_.scala.Predef.identity[$childTpe]($v).toString" else q"$v.toString"
        hearth.fp.effect.MEval.pure(c.Expr[String](body).asInstanceOf[Expr[String]])
      }
      .run
      .get
      .asInstanceOf[c.Expr[String]]
  }

  def asAny[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = go[A](value, false)(c.weakTypeTag[A], c.weakTypeTag[String])
  def asChild[A: c.WeakTypeTag](value: c.Expr[A]): c.Expr[String] = go[A](value, true)(c.weakTypeTag[A], c.weakTypeTag[String])
}
object Repro {
  def asAny[A](value: A): String = macro ReproMacros.asAny[A]
  def asChild[A](value: A): String = macro ReproMacros.asChild[A]
}
```

`usage/Usage.scala`:

```scala
object Main extends App {
  val mode: java.nio.file.AccessMode = java.nio.file.AccessMode.valueOf("WRITE")
  println(repro.Repro.asAny(mode))   // compiles
  println(repro.Repro.asChild(mode)) // type mismatch; found: READ$macro$2.type (with underlying type
                                     // java.nio.file.AccessMode), required: java.nio.file.AccessMode(READ)
}
```

Output: `repro-details/scala2-java-enum-matchOn-binder-output.txt`.
