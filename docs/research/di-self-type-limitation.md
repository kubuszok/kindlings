# DI module: self-type providers — Scala 3 yes, Scala 2 no

## Summary

macwire supports drawing wiring candidates from a trait's **self-type** requirement:

```scala
trait AProvider { def a: A }

trait Module { this: AProvider =>
  lazy val c = wire[C] // `a` is pulled from the AProvider self-type
}
```

The Kindlings `di` module supports this **on Scala 3 but not on Scala 2**. This is a
platform difference rooted in Hearth's `enclosingScope`, not a di bug.

- **Scala 3**: `DI.wire[C]` inside such a trait wires `a` from the self-type correctly.
  See the positive test `di/src/test/scala-3/.../SelfTypeSpec.scala`.
- **Scala 2**: `DI.wire[C]` fails with `Cannot find a value of type: [A]`. See the
  negative (documented-limitation) test `di/src/test/scala-2/.../SelfTypeSpec.scala`.

## Root cause (Hearth, not di)

`WiringMacrosImpl.collectScopes` reads candidate members off `Enclosure.Class.members`,
which Hearth populates from the enclosing class symbol's own type:

- Scala 2 — `EnvironmentsScala2.toEnclosure`: `val tpe = symbol.asClass.toType`, then
  `UntypedMethod.methods(tpe)`. A trait's `toType` does **not** fold in its self-type
  requirement (`this: AProvider =>`); on Scala 2 the self-type lives on
  `thisSym.typeOfThis`, which is never consulted. So `AProvider#a` is invisible.
- Scala 3 — the analogous derivation surfaces the self-type members, so wiring succeeds.

Because the difference is purely in how each platform's `enclosingScope` is built, the
tests are split by platform (per the project rule that legitimate platform differences
belong in `scala-2/` / `scala-3/` dirs) rather than hidden.

## What would close the Scala 2 gap

Hearth would need to include the self-type members of an enclosing `trait` in
`Enclosure.Class.members` on Scala 2 (e.g. by reading `symbol.asClass.typeOfThis` /
`thisSym.typeOfThis` members in addition to `toType`), matching the Scala 3 behavior.
Once available, the Scala 2 negative test should be promoted to the same positive
assertion as the Scala 3 one.
