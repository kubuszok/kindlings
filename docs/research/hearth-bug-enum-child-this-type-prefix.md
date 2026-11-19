# Hearth bug: a case object in a companion of a class-nested sealed trait is typed `Shape.this.Empty$` (Scala 3)

**Status:** FIXED in Hearth 985e5e6 (next Hearth release after 0.4.5; Kindlings uses snapshot `0.4.5-16-g985e5e6-SNAPSHOT`). Was OPEN (Hearth 0.4.5, Scala 3.8.x; Scala 2 not affected as far as tested). Found 2026-11-18 while writing the
Kindlings testing guide (circe-derivation and diff-derivation reproduced it; any derivation that uses the type of such
a case object in generated code is affected).

## Symptom

```scala
class MySuite extends munit.FunSuite { // or a .sc script, which wraps the code in a class
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }
  final case class Circle(r: Double) extends Shape

  val encoder = KindlingsEncoder.derived[Shape]
}
```

crashes the Scala 3 compiler in erasure:

```
java.lang.AssertionError: assertion failed: missing outer accessor in class MySuite
  at dotty.tools.dotc.transform.ExplicitOuter$.outerParamAccessor(ExplicitOuter.scala:234)
  at dotty.tools.dotc.transform.ExplicitOuter$OuterOps$.path$extension(ExplicitOuter.scala:472)
  at dotty.tools.dotc.transform.Erasure$Typer.typedThis(Erasure.scala:772)
```

The same hierarchy at the top level or inside an `object` compiles.

## Cause

`Enum.parse[Shape]` gives the child `Empty` the type

```
TypeRef(ThisType(TypeRef(ThisType(TypeRef(ThisType(<pkg>), "MySuite")), "Shape$")), "Empty$")
```

i.e. `Shape.this.Empty$`: the module class seen from INSIDE `object Shape` (its owner's `this` type). Inside `MySuite`
but outside `Shape`, `Shape.this` is not an enclosing `this`, so when the type is used in generated code (e.g. the type
of a helper `def`'s parameter, as every Kindlings derivation does for its cached per-type helpers) erasure tries to
reach `Shape.this` through outer pointers and fails. The type should be the one seen from `Shape`'s own prefix,
`MySuite.this.Shape.Empty.type` (`TermRef(TermRef(ThisType(MySuite), Shape), Empty)`), e.g. by building the child type
as a member of the parent's prefix (`asSeenFrom` / `select` on the parent `TypeRepr`'s prefix) instead of
`childSymbol.typeRef`.

A plain reference to the value (`SingletonValue.singletonExpr`) is fine; only the TYPE is wrong.

## Minimal standalone reproducer (Hearth only, Scala 3.8.3, Hearth 0.4.5)

`macro/Repro.scala`:

```scala
//> using scala 3.8.3
//> using dep com.kubuszok::hearth:0.4.5
package repro

import scala.quoted.*
import hearth.fp.instances.*

object Repro {

  /** The name of the case of a sealed `A`, computed by a `match` built with Hearth's `Enum#matchOn`, each case passing
    * the matched value to a local helper `def` whose parameter has the matched case's type (as derived codecs do).
    */
  inline def caseName[A](value: A): String = ${ ReproMacros.impl[A]('value) }
}

class ReproMacros(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

  def caseNameOf[A: scala.quoted.Type](value: scala.quoted.Expr[A]): scala.quoted.Expr[String] = {
    given Quotes = q
    Enum
      .parse[A]
      .toOption
      .get
      .matchOn[Option, String](value) { matched =>
        import matched.{Underlying as M, value as m}
        val name = scala.quoted.Expr(Type.plainPrint[M])
        // prints TypeRef(ThisType(TypeRef(ThisType(TypeRef(ThisType(TypeRef(NoPrefix(), "usage")), "Suite")), "Shape$")), "Empty$")
        locally {
          import q.reflect.*
          report.info(TypeRepr.of[M](using M).show(using Printer.TypeReprStructure))
        }
        Some('{
          def helper(x: M): String = $name
          helper($m)
        })
      }
      .flatten
      .get
  }
}
object ReproMacros {
  def impl[A: Type](value: Expr[A])(using q: Quotes): Expr[String] = new ReproMacros(q).caseNameOf[A](value)
}
```

`usage/Usage.scala`:

```scala
//> using scala 3.8.3
package usage

class Suite {
  sealed trait Shape
  object Shape {
    case object Empty extends Shape
  }
  final case class Circle(r: Double) extends Shape

  def names: List[String] = List[Shape](Shape.Empty, Circle(1.0)).map(s => repro.Repro.caseName[Shape](s))
}

@main def run(): Unit = println(new Suite().names)
```

```bash
scala-cli --power package macro --library -o macro.jar -f
scala-cli run usage --jar macro.jar --dep com.kubuszok::hearth:0.4.5
# Error: Unexpected error when compiling usage: java.lang.AssertionError: assertion failed: missing outer accessor in class Suite
```

Declaring the hierarchy in an `object` (or at the top level) instead of `class Suite` makes it compile.

## Workaround for users

Declare the sealed hierarchy at the top level or inside an `object` (for test suites: in the suite's companion object
or a separate `object Fixtures`). Documented in the FAQ.

## Kindlings test

`circe-derivation/src/test/scala/hearth/kindlings/circederivation/NestedHierarchySpec.scala` covers the hierarchy nested
in an object. The class-nested variant cannot be kept as a failing test: it crashes the compiler, which fails every
test of the module. When Hearth is fixed, move the fixtures into the spec class.
