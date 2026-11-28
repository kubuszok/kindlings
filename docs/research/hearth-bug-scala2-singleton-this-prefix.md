# Hearth bug: `Expr.singletonOf` refers to a case object in a class-nested companion as `Shape.this.Empty` (Scala 2)

**Status:** OPEN (Hearth 0.4.5, `0.4.5-16-g985e5e6-SNAPSHOT` and `0.4.5-18-g99ac2ae-SNAPSHOT` - the reproducer below
re-run on 2026-11-28 still prints `Shape.this.Empty` and fails the same way; Scala 2.13 only). Found 2026-11-19 while adding the
class-nested shape of `hearth-bug-enum-child-this-type-prefix.md` (the Scala 3 TYPE bug, fixed in 985e5e6) to the
Kindlings tests. This is the Scala 2 counterpart, but it affects the VALUE reference, not the type.

## Symptom

```scala
class MySuite extends munit.FunSuite { // or anything else that is a class
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }
  final case class Circle(r: Double) extends Shape

  val codec = KindlingsJsonValueCodec.derived[Shape] // or KindlingsDecoder.derived[Shape], ...
}
```

fails on Scala 2.13 with

```
Internal error: unable to find the outer accessor symbol of class MySuite
```

Encoders compile (they only match on the case object's type). Decoders, codecs and anything else that refers to the
case object's VALUE fail. If the case object is not inside the companion (`case object Empty extends Shape` directly in
`MySuite`), or the hierarchy is in an `object` or at the top level, it compiles.

## Cause

`ExprsScala2.singletonOf` (used by `SingletonValue.singletonExpr`) builds the reference with
`gen.mkAttributedRef(moduleSym)`, which takes the prefix from the module's owner: `Shape.this.Empty`. Outside
`object Shape` but inside `MySuite`, `Shape.this` is not an enclosing `this`, so later phases try to reach it through
outer pointers and fail. The generated Kindlings code shows it (`hearth.debug.Debug.withFinalCodeInIDE`):

```scala
if ("Empty".==(typeName)) A.this.AEmpty.asInstanceOf[...TmpProbeSpec.A]
```

The reference should use the type's own prefix, e.g. `gen.mkAttributedRef(tpe.prefix, moduleSym)` (giving
`MySuite.this.Shape.Empty`), as the Scala 3 fix did for the type.

## Minimal standalone reproducer (Hearth only, Scala 2.13.18)

`macro/Repro.scala`:

```scala
//> using scala 2.13.18
//> using dep com.kubuszok::hearth:0.4.5-16-g985e5e6-SNAPSHOT
//> using repository https://central.sonatype.com/repository/maven-snapshots
//> using dep org.scala-lang:scala-reflect:2.13.18
package repro

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Repro {

  /** The value of the singleton type `A`, built with Hearth's `Expr.singletonOf`. */
  def singleton[A]: A = macro ReproMacros.impl[A]
}

class ReproMacros(val c: blackbox.Context) extends hearth.MacroCommonsScala2 {

  def impl[A: c.WeakTypeTag]: c.Expr[A] = {
    val expr = Expr.singletonOf[A].get
    c.info(c.enclosingPosition, expr.tree.toString, force = true)
    expr
  }
}
```

`usage/Usage.scala`:

```scala
//> using scala 2.13.18
package usage

class Suite {
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }

  def empty: Shape = repro.Repro.singleton[Shape.Empty.type]
}

object Main extends App {
  println(new Suite().empty)
}
```

```bash
scala-cli --power package macro --library -o macro.jar -f
scala-cli run usage --jar macro.jar --dep com.kubuszok::hearth:0.4.5-16-g985e5e6-SNAPSHOT \
  --repository https://central.sonatype.com/repository/maven-snapshots
# [info] Shape.this.Empty
# [error] ./usage/Usage.scala:10:7
# [error] Internal error: unable to find the outer accessor symbol of class Suite
```

## Workaround for users

Declare the sealed hierarchy at the top level or inside an `object` (for test suites, in the suite's companion object),
or put the case object next to the trait instead of in its companion. Documented in the FAQ.

## Kindlings test

The class-nested hierarchy in `circe-derivation/.../NestedHierarchySpec.scala` and
`fast-show-pretty/.../NestedHierarchySpec.scala` is only used with encoders. A class-nested decoder test cannot be kept
as a failing test: the compile error fails every test of the module on Scala 2. When Hearth is fixed, derive and test
the decoder there too (and add the shape back to jsoniter-derivation, whose codecs need the value).
