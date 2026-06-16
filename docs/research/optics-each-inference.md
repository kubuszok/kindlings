# Optics `.each` — path parsing probe + cross-platform inference findings (Phase 2)

Status: resolved (Phase 2 `.each`/`.eachWhere` green on Scala 2.13 + 3 JVM). This note records the
path-parsing probe result and the two non-obvious inference issues that shaped the final design, so a
future maintainer (or Phase 3) does not re-discover them.

## Path-parsing probe (the prompt's "do this FIRST")

A throwaway macro probe (`abort` printing the parsed steps for `_.people.each.name`) confirmed
`DestructuredExpr.parse` recovers everything Phase 2 needs:

```
S=ProbeBox  A=String
steps=[ Field(people),
        Each(container=List[Person], elem=Person, pred=false, isMap=false),
        Field(name) ]
```

- (a) the `.each` receiver sub-expression (`s.people`, a `List[Person]`) — recovered via the `.each`
  `MethodCall`'s instance (after stripping the Scala 2 `EachOps` wrapper);
- (b) that an `each` call occurred — `MethodCall.method.name == "each"`;
- (c) the element type `A` — taken from the **container** type (`List[Person]` → `Person` via
  `Type.decompose1`), NOT from the marker's result type (see issue 2);
- (d) a handle to summon the functor — `Type.decompose1`/`decompose2` give the constructor, then
  `Type.CtorK1.of[QuicklensFunctor].apply(using ctor)` / `Type.Ctor1.of[QuicklensMapFunctor.ForMap]`
  builds the type to `Expr.summonImplicit`. No Hearth gap. (Mirrors cats-derivation
  `ConsKMacrosImpl.summonConsKForFieldType`.)

So **no Hearth API gap was hit** — the anticipated gaps #1/#2 in `optics-port-plan.md` (extension-method
receiver recovery) are fully covered by `DestructuredExpr` + `Type.decompose{1,2}` + `Type.CtorK1`.

## Issue 1 — Scala 2: `.each` path widens `modify`'s `A` to `Any`

With the natural `def modify[A](path: S => A)` and a marker `each` defined on `EachOnFunctor[F[_], A](fa: F[A])`,
an **inline** path lambda whose body goes through the `.each` implicit conversion infers `A = Any`
(`using` then demands `Any => Any`). Root cause: `List[Int] <: List[Any]` (covariance) lets the conversion
pick the *widest* `A`, and the underscore-lambda / result-type expected type pushes it to `Any`.

Fix: recover the element type **invariantly** with an `IsElementOf.Aux[C, A]` evidence resolved against the
*exact* container type `C` (so `A` is pinned, no covariant widening). The marker becomes
`EachOps[C, A](c: C)(implicit ev: IsElementOf.Aux[C, A])` with `each: A`. `IsElementOf` instances:
`map[K, V]`, `array[A]`, and `fromFunctor[F[_], A](implicit QuicklensFunctor[F])` (so user functors extend
`.each` for free). With this, `_.xs.each` infers `A = Int` and the natural `modify[A]` signature works — no
whitebox macro needed.

## Issue 2 — the marker result type is unreliable; derive the element from the container

Even with `A` pinned, do **not** trust the `.each` call's result type for the element. The macro always
re-derives the element from the parsed container (`Type.decompose1`/`decompose2`). The codegen never depends
on the call-site `A` inference being perfect.

## Issue 3 — Scala 3: implicit-class `.each` does not destructure; use `extension`

On Scala 3, `DestructuredExpr` could not resolve `.each` when it was an **implicit class** method
(`EachOps(...).each` collapsed to `NonDestructurable`). The fix is to define the markers per platform:
Scala 2 keeps the `implicit class EachOps`, Scala 3 uses
`extension [C, A](c: C)(using IsElementOf.Aux[C, A]) def each: A`. Hearth's `DestructuredExpr` has a
dedicated, well-tested `Flags.ExtensionMethod` path, so the Scala 3 extension parses cleanly.

A consequence: on Scala 3 the `using IsElementOf` evidence appears as a **leading value argument** of the
`eachWhere` call, so the predicate is recovered as the `Function1`-typed value argument
(`predicateArg`), not the first one.

## Issue 4 — `Array.each` and `ClassTag`

`QuicklensFunctor[Array]` cannot be summoned for the erased constructor if it requires a per-element
`ClassTag` (the element type is not known at the summon site). The runtime `arrayQuicklensFunctor` is a
single `val` that rebuilds the result array reflectively from the **input** array's component type
(`java.lang.reflect.Array.newInstance` + `Array.set`, which unboxes into primitive arrays), so no `ClassTag`
is needed.

## Codegen shape

For an `Each` step over `prefix: F[A]` the macro emits, via a `LambdaBuilder` element lambda (the sanctioned
collection-iteration use):

```scala
QuicklensRuntime.eachFunctor(functor, prefix, (x: Any) => <rest-of-path on x.asInstanceOf[A]>).asInstanceOf[F[A]]
```

All erased-`Any` boundaries (`functor`, the element lambda, the result) use **in-tree** `.asInstanceOf`
casts (a real cast node), not macro-side `Expr` reinterpretations — otherwise the generated tree stays typed
`Any` / `Int => Int` and fails to conform to the erased runtime-helper signatures.
