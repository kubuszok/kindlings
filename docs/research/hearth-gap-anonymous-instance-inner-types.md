# Hearth gap: `AnonymousInstance` overrides use a type projection for inner types of the parent

Found by kindlings-mock while porting ScalaMock's `MockTest` ("mock an embedded trait").

## Symptom

```scala
trait WithEmbedded {
  trait Embedded { def m(x: Int, y: Double): String }
  def referencesEmbedded(): Embedded
}

Mock.mock[WithEmbedded]
```

fails on Scala 3 with

```
error overriding method referencesEmbedded in trait WithEmbedded of type (): Embedded;
  method referencesEmbedded of type (): WithEmbedded#Embedded has incompatible type
```

and on Scala 2 (in `refchecks`, so `compileErrors` does not see it) with

```
incompatible type in overriding def referencesEmbedded(): this.Embedded (defined in trait WithEmbedded);
 found   : (): WithEmbedded#Embedded
 required: (): this.Embedded
```

## Cause

`AnonymousInstance.construct` declares each override with the member's type as seen from the parent type
(`asSeenFrom(WithEmbedded)`), so a path-dependent `this.Embedded` becomes the projection `WithEmbedded#Embedded`. The
override must keep the type relative to the new instance's `this` (the anonymous class' `this.Embedded`), as the
compiler itself does for `new WithEmbedded { def referencesEmbedded() = ... }`.

## Expected fix in Hearth

Substitute the parent's `this` type with the anonymous class' `this` (`ThisType` of the new class symbol) in the
override signatures, instead of seeing them from the parent type.

## In Kindlings

`mock/src/test/scala-3/hearth/kindlings/mock/EmbeddedTraitMockSpec.scala` asserts the error (Scala 3); documented in
`docs/user-guide/mock.md` and `feature-parity.md` (mock).
