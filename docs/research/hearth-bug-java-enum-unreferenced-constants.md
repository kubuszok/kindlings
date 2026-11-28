# Hearth: on Scala 3 the children of a Java enum read from a class file are the constants the compiler has completed

**Status:** OPEN in Hearth (`0.4.5-18-g99ac2ae-SNAPSHOT`, Scala 3.8.4). Worked around in Kindlings
(`derivation-commons`: `JavaEnumSupport.parseEnum`, `JavaEnumConstants`, `StableChildrenOrder`). Scala 2.13 is not
affected.

Two symptoms of the same cause:

1. a Java enum none of whose constants is referred to by the code being compiled is **not an enum** for Hearth
   (`Enum.parse` fails with "is not sealed, not an enumeration, and not a union type"),
2. the children of a Java enum some of whose constants are referred to come in the order in which the compiler
   completed the constants - **the referred ones first** - not in declaration order. This is the "order varied between
   compilations" observation of `hearth-java-enum-children-order.md`.

Both affect every JDK enum and every Java enum from a dependency, and a Java enum of the same sbt module whenever the
Scala sources are recompiled without the Java source (the enum is then read from javac's class file).

## Reproducer

Two files, run with `scala-cli test .` (the test scope is compiled separately from the macro):

```scala
// EnumChildren.scala
//> using scala 3.8.4
//> using dep com.kubuszok::hearth:0.4.5-18-g99ac2ae-SNAPSHOT
//> using repository https://central.sonatype.com/repository/maven-snapshots
//> using test.dep org.scalameta::munit:1.1.1
package repro

import scala.quoted.*

object EnumChildren {

  /** The children Hearth's `Enum.parse` reports for `A`, or why `A` is not an enum. */
  inline def of[A]: String = ${ ofImpl[A] }

  def ofImpl[A: Type](using q: Quotes): Expr[String] = new EnumChildrenImpl(q).childrenOf[A]
}

final class EnumChildrenImpl(q: Quotes) extends hearth.MacroCommonsScala3(using q) {

  def childrenOf[A: Type]: Expr[String] =
    Expr(Enum.parse[A].toEither.fold(reason => s"not an enum: $reason", _.directChildren.keys.mkString(", ")))
}
```

```scala
// EnumChildren.test.scala
package repro

class EnumChildrenTest extends munit.FunSuite {

  test("a JDK enum none of whose constants is referred to in this compilation") {
    // nothing in this compilation mentions AccessMode.READ, WRITE or EXECUTE
    println(s"AccessMode: ${EnumChildren.of[java.nio.file.AccessMode]}")
  }

  test("a JDK enum one of whose constants is referred to in this compilation") {
    val referred = java.time.format.TextStyle.SHORT_STANDALONE
    println(s"TextStyle (SHORT_STANDALONE referred to): ${EnumChildren.of[java.time.format.TextStyle]}")
    println(s"TextStyle.values(): ${java.time.format.TextStyle.values().mkString(", ")} ($referred)")
  }
}
```

Output:

```
AccessMode: not an enum: java.nio.file.AccessMode is not sealed, not an enumeration, and not a union type
TextStyle (SHORT_STANDALONE referred to): SHORT_STANDALONE, FULL, FULL_STANDALONE, SHORT, NARROW, NARROW_STANDALONE
TextStyle.values(): FULL, FULL_STANDALONE, SHORT, SHORT_STANDALONE, NARROW, NARROW_STANDALONE (SHORT_STANDALONE)
```

## Cause

Hearth's Scala 3 `directChildrenList` lists a Java enum's children as `classificationSymbol(tpe).children`. dotc's
`ClassfileParser` registers a constant of a Java enum as a child (`enumClass.addAnnotation(Annotation.Child(sym,
NoSpan))`) when the constant's **info is completed**, and `SymDenotation.children` returns the `@Child` annotations in
the order they were added. `children` tries to complete the possible children first (`completeChildrenIn(owner)`,
`completeChildrenIn(companionClass)`), but that does not reach the static members of a class-file enum, so:

- if nothing completed a constant, the enum has no children (`Enum.unapply` filters out an empty list),
- if the compiled code referred to `SHORT_STANDALONE`, it was registered first; the others follow when something else
  completes them.

(A Java enum compiled from Java source in the same run goes through `Namer.addEnumConstants`/`addChild`, which sorts the
children by position: declaration order.)

## Kindlings workaround

- `JavaEnumConstants` (`derivation-commons`, Scala 3): completes the infos of the static fields of the enum's companion
  module (`module.termRef.memberType(field)`), which registers all constants, and returns their names in declaration
  order (the order of the fields in the class file).
- `JavaEnumSupport.parseEnum[A]`: that completion, then `Enum.parse[A]`. Every derivation module calls it instead of
  `Enum.parse` (mixed in through `DerivationPolicy`, which every derivation macro bundle has). It must run before the
  first `Enum.parse` of the type: Hearth caches `Enum.unapply` per type.
- `StableChildrenOrder.inStableOrder`: puts the children of a Java enum in that declaration order.

The workaround first lived in jsoniter-derivation only (`CodecMacros.completeJavaEnum`, from the jsoniter-scala parity
work, `hearth-findings-jsoniter-parity-2026-11.md` item 4). Tests: `UnreferencedJavaEnum*Spec` in the JVM test sources
of circe, jsoniter, yaml, ubjson, xml, tapir-schema, avro, pureconfig, sconfig, scalacheck, cats, fast-show-pretty and
diff derivation (`java.nio.file.AccessMode`, whose constants none of these modules' tests refer to), and the cats
`Order` of `java.time.format.TextStyle` with `SHORT_STANDALONE` referred to.

## Suggested fix in Hearth

In the Scala 3 `directChildrenList` (and `isJavaEnum`-based paths) of a Java-defined enum: complete the static fields of
the companion module first, and list the children in the order of those fields (or sort them by `ordinal` when the
constants have no positions).
