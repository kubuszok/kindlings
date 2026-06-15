# DI module: by-name (`=> A`) constructor parameters — Scala 2 yes, Scala 3 pending

## Summary

macwire wires by-name constructor parameters (`class B(aProvider: => A)`) by resolving a
strict `A` from scope and letting the call site thunk it. The Kindlings `di` module does
this **on Scala 2 but not yet on Scala 3**.

- **Scala 2**: `DI.wire[B]` for `class B(a: => A)` wires `a` from scope. Positive test
  `di/src/test/scala-2/.../SelfTypeSpec.scala`.
- **Scala 3**: the same `DI.wire[B]` fails with
  `Cannot find a value of type: [ => A]`. Documented-limitation test
  `di/src/test/scala-3/.../SelfTypeSpec.scala`.

## How it works (and where Scala 3 falls short)

`WiringMacrosImpl.resolveParameter` computes an "effective" parameter type via
`effectiveParamType`: when `parameter.isByName`, it strips the by-name wrapper down to
the underlying `A` so a strict value from scope satisfies it.

- On **Scala 2**, a by-name parameter's type is the applied wrapper `<byname>[A]`, so
  `Type.typeArguments[<byname>[A]]` yields `List(A)` and the unwrap succeeds.
- On **Scala 3**, the parameter type is a `ByNameType(A)`, which is **not** an applied
  type — `Type.typeArguments` returns `Nil`, so the unwrap can't recover `A`. Hearth's
  typed API does not currently surface the underlying type of a `ByNameType` (the
  `isByName` flag is derived by inspecting the owning method's signature, see
  `UntypedMethodsScala3`, but the underlying type is not exposed as a typed `??`).

## What would close the Scala 3 gap

Hearth would need to expose the underlying type of a by-name parameter on Scala 3 — e.g.
a `Parameter.byNameUnderlying: Option[??]` that returns the `tp` from `ByNameType(tp)`,
or normalize `Parameter.tpe` to the underlying type (as it already does for varargs,
where `tpe` is normalized to `Seq[A]`). Once available, `effectiveParamType` would use it
and the Scala 3 limitation test should be promoted to the Scala 2 positive assertion.

The macro-side resolution (resolve a strict `A`, let the application thunk it) already
works on both platforms; only the type decomposition is missing on Scala 3.
