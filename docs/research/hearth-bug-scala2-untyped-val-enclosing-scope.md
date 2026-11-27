# Hearth bug: on Scala 2, `Environment.enclosingScope` fails for an untyped `val`/`def` holding the macro call

Found by kindlings-di while porting macwire's test cases (`di/src/test/scala*/hearth/kindlings/di/upstream/`).

## Symptom

On Scala 2.13 a macro that reads `Environment.enclosingScope` cannot be the right-hand side of a `val`, `lazy val` or
`def` without an explicit type:

```scala
object Module {
  val a = new A()
  val c = wire[C]          // error: recursive value c needs type
  def make(b: B) = wire[D] // error: recursive method make needs result type
}
```

With a type ascription (`val c: C = wire[C]`) it works. macwire's own documentation and test cases write untyped
`lazy val x = wire[X]` everywhere, so every macwire module ported to kindlings-di on Scala 2 needs the ascriptions.
Scala 3 is not affected for members; on Scala 3 the same happens for an untyped **local** `val` in a method body
(`val errors = compileErrors(...)` in a test).

## Cause

`hearth/src/main/scala-2/hearth/EnvironmentsScala2.scala`, `toEnclosure`:

- an enclosing `val`/`var`/`lazy val` becomes `Enclosure.Value(..., symbol.typeSignature.finalResultType)`: the type of
  the very definition being typed is forced while its right-hand side (the macro call) is being typed - a cyclic
  reference, reported by scalac as "recursive value c needs type";
- an enclosing class/object lists `UntypedMethod.methods(tpe)` with their signatures, which forces the types of the
  sibling members as well (an untyped sibling `lazy val b = wire[B]` is typed, expanding its own `wire`, which reads the
  enclosing scope again).

## Expected fix in Hearth

Make `Enclosure.Value.tpe` lazy (or `Option`, `None` while the symbol is being typed, as `Enclosure.Method.tpe` already
is), and skip - or describe lazily - members whose signature is not computed yet (`sym.rawInfo.isComplete == false`),
instead of completing them. macwire does the latter: it never forces the member under definition, and reads the type of
an untyped sibling whose right-hand side is a `wire[X]` call as `X` without typing it.

## In Kindlings

The ported macwire cases carry the type ascriptions (the generator adds them, see the header of
`MacwireTestCasesSpec`), and `docs/user-guide/di.md` ("Differences from macwire") tells Scala 2 users to annotate the
members holding a `wire`. `StrictValsInObjectSpec` checks the annotated form.
