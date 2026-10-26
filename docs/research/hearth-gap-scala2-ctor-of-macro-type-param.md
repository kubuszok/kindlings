# Hearth gap: `Type.Ctor1.of[F]` / `Type.CtorK1.of[Alg]` for a Scala 2 macro method's own type parameter

Status: **RESOLVED** by kubuszok/hearth#389 (Hearth `0.4.3-17-gbd1e277-SNAPSHOT`, 2026-10-26). Found 2026-10-25 with
Hearth 0.4.3, while moving `cats-derivation` and `cats-tagless-derivation` off per-platform reflection. Scala 3 was not
affected.

**Resolution.** On Scala 2, `Type.Ctor1.of[F]` / `Type.Ctor2.of[F]` / `Type.CtorK1.of[Alg]` called where `F`/`Alg` is a
type parameter of the enclosing macro method now use that method's implicit `c.WeakTypeTag[F[Any]]` (or `Type[F[Any]]`,
`c.WeakTypeTag[Alg[Option]]`, ...) parameter, matched by the type parameter's name. The result is the tag type's
`typeConstructor`, *not* dealiased, so it agrees with Scala 3 for type aliases. Without such an implicit parameter it
aborts with an explanatory error instead of yielding an abstract constructor or crashing.

**In Kindlings**, all 26 one-line Scala 2 entry points now call `of` directly (the implicit tag parameters stay, since
`of` reads them):

- `cats-derivation`: 19 × `derive<TC>[F](Type.Ctor1.of[F])` / `derive<TC>[F](Type.Ctor2.of[F])`;
- `cats-tagless-derivation`: 6 × `derive<TC>[Alg](Type.CtorK1.of[Alg])`;
- `di-cats` (`ResourceWiringMacros`): 1 × `Type.Ctor1.of[F]`.

The rest of this document is the original report, kept for history.

## What is wrong

A higher-kinded type class derivation has an entry point whose type parameter is the type constructor being derived
for: `Functor.derived[F[_]]`, `FunctorK.derived[Alg[_[_]]]`. The Scala 2 macro impl receives it as a
`c.WeakTypeTag[F[Any]]` / `c.WeakTypeTag[Alg[Option]]`. Hearth has no cross-platform way to turn that into a
`Type.Ctor1[F]` / `Type.CtorK1[Alg]`:

| entry code (Scala 2 macro bundle, `MacroCommonsScala2`) | Scala 2.13.18 | Scala 3 (`using scala.quoted.Type[F]`) |
|---|---|---|
| `Type.Ctor1.of[F]` with `implicit ft: c.WeakTypeTag[F[Any]]` | **wrong result**: the constructor of the *abstract* `F`, not of the user's `Box` | correct |
| `Type.CtorK1.of[Alg]` with `implicit at: c.WeakTypeTag[Alg[Option]]` | **crash** at expansion: `NoSuchElementException: last of empty list` | correct |

The `Ctor1` case fails silently. Derivation goes on with the abstract `F`, and fails much later with an unrelated
message.

Likely cause: the Scala 2 `CtorN.of` / `CtorK1.of` macros expand to
`$ctx.weakTypeTag[Type.CtorK1.Stub[HKT]].tpe.typeArgs.last`, where `$ctx` is a fresh `CrossQuotes.ctx` val. The macro
impl's own implicit `c.WeakTypeTag[Alg[Option]]` belongs to a different path (`c.` rather than `$ctx.`), so it is not
eligible. scalac then materializes a weak tag over a free type `Alg`, and that tag carries neither the user's type nor
its type arguments.

## Reproducer

Take the current Scala 2 entry points and swap the `fromUntyped` glue for `of`.

`cats-tagless-derivation/src/main/scala-2/.../FunctorKMacros.scala`:

```scala
def deriveFunctorKImpl[Alg[_[_]]](implicit at: c.WeakTypeTag[Alg[Option]]): c.Expr[cats.tagless.FunctorK[Alg]] =
  deriveFunctorK[Alg](Type.CtorK1.of[Alg]) // instead of Type.CtorK1.fromUntyped[Alg](at.tpe.typeConstructor)
```

Running `sbt --client "catsTaglessDerivation2_13/clean ; catsTaglessDerivation2_13/testOnly hearth.kindlings.catstaglessderivation.*"` gives:

```
[error] .../cats-tagless-derivation/src/test/scala/hearth/kindlings/catstaglessderivation/examples.scala:13:68: exception during macro expansion:
[error] java.util.NoSuchElementException: last of empty list
[error] 	at scala.collection.immutable.Nil$.last(List.scala:655)
[error] 	at scala.collection.immutable.Nil$.last(List.scala:651)
[error] 	at hearth.kindlings.catstaglessderivation.internal.compiletime.FunctorKMacros$$anon$1.<init>(FunctorKMacros.scala:14)
[error] 	at hearth.kindlings.catstaglessderivation.internal.compiletime.FunctorKMacros.deriveFunctorKImpl(FunctorKMacros.scala:14)
[error]     implicit val functorK: FunctorK[DirectAlg] = KindlingsFunctorK.derived
```

`cats-derivation/src/main/scala-2/.../FunctorMacros.scala`:

```scala
def deriveFunctorImpl[F[_]](implicit ft: c.WeakTypeTag[F[Any]]): c.Expr[cats.Functor[F]] =
  deriveFunctor[F](Type.Ctor1.of[F]) // instead of Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor)
```

Running `sbt --client "catsDerivation2_13/clean ; catsDerivation2_13/testOnly *FunctorSpec*"` gives:

```
[error] .../cats-derivation/src/test/scala/hearth/kindlings/catsderivation/examples.scala:88:63: Macro derivation failed:
[error]   - Cannot parse F[Any] as an enum: F[scala.Any] is not sealed, not an enumeration, and not a union type
[error]     implicit val functorBox: cats.Functor[Box] = cats.Functor.derived
```

The Scala 3 entry points use the same `Type.Ctor1.of[F]` / `Type.CtorK1.of[Alg]`, given a `scala.quoted.Type[F]`, and
all their tests pass.

## Why the Hearth `UntypedType` primitives are not a drop-in either

`Type[A]` is `c.WeakTypeTag[A]` on Scala 2, so `UntypedType.typeConstructor(UntypedType.fromTyped[F[Any]])` compiles
and avoids `c.universe`. But `UntypedType.typeConstructor` **dealiases first**: its documented contract is
"after dealiasing", and the Scala 2 implementation is `widenSingletons(untyped).dealias.typeConstructor`. For a type
alias as `F` (e.g. `type Twice[A] = (A, A)`) that gives `Tuple2`, which has the wrong arity. The Kindlings test
suite does not cover this case; the conclusion comes from reading the Hearth source. `ft.tpe.typeConstructor` and the
Scala 3 `Type.Ctor1.of[F]` both keep the alias constructor. The two platforms would then diverge, so the entry glue keeps
`.tpe.typeConstructor`.

## Former workaround (removed after kubuszok/hearth#389)

Each Scala 2 entry point is one line of glue. Everything after it is shared code that uses only Hearth APIs:

```scala
derive<TC>[F](Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor))         // cats-derivation, F[_]
derive<TC>[F](Type.Ctor2.fromUntyped[F](ft.tpe.typeConstructor))         // cats-derivation, F[_, _]
derive<TC>[Alg](Type.CtorK1.fromUntyped[Alg](at.tpe.typeConstructor))    // cats-tagless-derivation
```

## Wanted

Any one of these:

- On Scala 2, make `Type.CtorN.of[F]` / `Type.CtorK1.of[HKT]` pick up the macro impl's own `c.WeakTypeTag[F[...]]` /
  `Type[F[...]]` (for example by resolving the tag against `c` instead of the fresh `$ctx` val), or fail with a clear
  error instead of producing an abstract constructor or crashing.
- A cross-platform constructor-from-applied-type that does **not** dealias, such as
  `Type.Ctor1.fromApplied[F](using Type[F[Any]])` / `Type.CtorK1.fromApplied[Alg](using Type[Alg[Option]])`, so that
  Scala 2 entry points can be written without `c.universe`.

When either one ships, the 19 + 6 one-line Scala 2 entry points in `cats-derivation` and `cats-tagless-derivation` can
call it directly.
