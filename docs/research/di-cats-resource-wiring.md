# di-cats: F-agnostic `Resource[F, _]` wiring (design + macwire autocats analysis)

Status: design captured 2026-06-14. Implements the `di-cats` module — an F-agnostic
re-imagining of macwire's `autocats` (`com.softwaremill.macwire.autocats.autowire`).

## Goal vs macwire

macwire's autocats hardcodes the effect:

```scala
// com.softwaremill.macwire.autocats.package
def autowire[T](dependencies: Any*): Resource[IO, T] = macro MacwireAutoCatsMacros.autowire_impl[T]
```

Our version abstracts over `F[_]` (no `IO`, no `Sync`/`Async` constraint — see "Why no
typeclass constraints" below):

```scala
def wireResource[F[_], T](dependencies: Any*): Resource[F, T]
```

## macwire's algorithm (reverse-engineered)

Sources cloned at `/tmp/macwire/macrosAutoCats/.../internals/`:
`CatsProviders.scala`, `CatsProvidersGraphContext.scala`, `GraphBuilderUtils.scala`.

### Provider model (`CatsProviders.scala`)

Each input dependency expression is classified by its **type** into a `Provider`:

| Provider        | Matched when dep type is | `resultType` | How it contributes to the fold |
|-----------------|--------------------------|--------------|--------------------------------|
| `Effect`        | `IO[X]` (effect)         | `X`          | wrapped as `Resource.eval[F, X](io)`, then `.flatMap` |
| `Resource`      | `Resource[IO, X]`        | `X`          | used directly via `.flatMap` |
| `Instance`      | anything else (plain `X`)| `X`          | inlined directly — **no** flatMap, just its tree as ident |
| `FactoryMethod` | a `FunctionN` lambda     | return type  | applied to resolved arg idents; result re-wrapped per its return type (`Resource`/`Effect`/plain→`Resource.pure`) |
| `NotResolved`   | (internal) unmet dep     | —            | drives the "missing dependency" error |

Detection helpers:
- `Effect.isEffect`: `tpe.typeSymbol.fullName.startsWith("cats.effect.IO") && typeArgs.size == 1`,
  underlying = `typeArgs(0)`.
- `Resource.isResource`: `fullName.startsWith("cats.effect.kernel.Resource") && typeArgs.size == 2`,
  underlying = `typeArgs(1)`.
- `FactoryMethod.deconstruct`: matches `Function(params, Apply(fun, _))` (and the
  two-clause `Apply(Apply(fun, _), _)` implicit-params shape).

### Graph build (`CatsProvidersGraphContext.buildGraph`)

1. Classify all input deps into `providers` + lazy `FactoryMethodTree`s.
2. `resolveFactoryMethods` — DFS-resolve every factory method's params from the context.
3. Resolve the **root** type `T`: try a matching `FactoryMethod` first, else
   `findResolvableCreator` = primary constructor **or** companion `apply`, resolving each
   param from the context. Unresolved-but-wireable params recurse via `findResolvableCreator`
   (this is the `wireRec`-like behaviour). Truly unmet params become `NotResolvedProvider`.
4. `topologicalSort()` — stable DFS post-order over `provider.dependencies`, root appended last.
   Then `failOnMissingDependencies()` (collects `NotResolvedProvider` paths into a macwire-style
   error: `Missing dependency of type [X]. Path [Root].field -> ...`) and `verifyOrder`
   (aborts on **unused** providers: `Not used providers for the following types [...]`).
5. `resolve(tpe)` ambiguity: more than one provider `<:< tpe` ⇒ abort
   `Ambiguous instances of types [...]`.

### Codegen (`MacwireAutoCatsMacros.autowire_impl`)

```scala
sortedProviders
  .map { case fm: FactoryMethod => fm.result; case p => p }
  .collect { case p @ (_: Effect | _: Resource) => p }     // Instances are inlined, not flatMapped
  .foldRight(q"Resource.pure[IO, T](${graph.root.ident})") {
    case (resource, acc) => q"${resource.value}.flatMap((${resource.ident}: ${resource.resultType}) => $acc)"
  }
```

Key trick: `resource.ident` is a **fresh `Ident`** used both as the flatMap lambda param
**and** wherever that value is referenced in downstream constructions. That back-reference
is the crux of the codegen.

## Hearth (macro-agnostic) port plan

### Entry points (3-layer recipe, same as `di`)

- Shared: `ResourceWiringMacrosImpl { this: MacroCommons => }` with
  `def wireResource[F[_]: Type.Ctor1, T: Type](dependencies: List[Expr_??]): Expr[Resource[F, T]]`.
- Scala 2 bundle — HKT type param pattern (copied from `FunctorMacros`):
  ```scala
  def wireResourceImpl[F[_], T](dependencies: c.Tree*)(implicit
      ft: c.WeakTypeTag[F[Any]], tt: c.WeakTypeTag[T]): c.Expr[Resource[F, T]] = {
    val fCtor = Type.Ctor1.fromUntyped[F](ft.tpe.typeConstructor)
    wireResource[F, T](dependencies.toList.map(t => c.Expr[Any](t).as_??))(fCtor, ...)
  }
  ```
  Macro def: `def wireResource[F[_], T](dependencies: Any*): Resource[F, T] = macro ...wireResourceImpl[F, T]`.
  (Varargs ⇒ ONE method, not 23 overloads.)
- Scala 3: `inline def wireResource[F[_], T](inline dependencies: Any*): Resource[F, T] =
  ${ ...wireResourceImpl[F, T]('dependencies) }`; bridge splits the `Varargs` and forwards.

### HARD PART 1 — constructing `Resource[F, X]`/`F[X]` types where `F` is a type ctor

`Resource[F[_], A]` is kind `(*->*) -> * -> *`, so `Type.Ctor2.of[Resource]` (two proper
args) does **not** fit. Build the applied types via Hearth's **untyped** type API:
`F` applied to `X` = `fCtor.apply(X)` (gives `Type[F[X]]`); `Resource[F, X]` must be built by
applying the `Resource` type constructor to the untyped `(F, X)` — investigate
`UntypedType.appliedType` / equivalent in Hearth (`hearth/src/main/scala/hearth/typed/Types.scala`
and the `*Untyped*` layer). On Scala 2 the escape hatch is `c.universe.appliedType` (as
`FunctorMacros` does for `Functor[F]`); cross-platform needs the Hearth untyped equivalent.
**Report to Hearth if no portable `appliedType` exists** — that is the blocker to surface.

### HARD PART 2 — `Resource.flatMap(x => body)` back-reference lambdas

Generate the monadic chain with `LambdaBuilder` (this is a legitimate use — a genuine
`Function1` passed to `flatMap`, NOT the forbidden sibling-splice workaround):

```scala
// for a Resource provider `r: Expr[Resource[F, X]]` and continuation `k: Expr[X] => Expr[Resource[F, T]]`
LambdaBuilder.of1[X]().buildWith { (x: Expr[X]) => k(x) }   // : Expr[X => Resource[F, T]]
// then: Expr.quote { Expr.splice(r).flatMap(Expr.splice(lambda)) }
```

Fold right over the ordered Resource/Effect providers, base = `Resource.pure[F, T](rootCtor)`.
Instance providers are spliced directly into the construction (no flatMap), exactly as macwire
`.collect`s them out.

### Why no typeclass constraints

In cats-effect 3, `Resource#flatMap` is structural (no `MonadCancel` needed),
`Resource.pure[F, A](a)` and `Resource.eval[F, A](fa)` need at most `Applicative[F]`/nothing
depending on version — verify against cats-effect 3.6.3. If `Resource.pure` needs
`Applicative[F]` we either (a) summon it implicitly inside the macro and splice, or (b) add a
`(implicit F: Applicative[F])` to `wireResource`. Prefer (a) so the call site stays clean;
fall back to (b) if summoning at macro time is unreliable. **Decision pending first compile.**

## Test plan (`ResourceWiringSpec`, JVM 2.13+3 first)

- plain instances only → `Resource.pure`
- one `Resource[F, X]` dep → single flatMap
- one `F[X]` effect dep → `Resource.eval(...).flatMap`
- mix: 2 resources + 1 instance + recursive intermediate ctor
- ambiguity → compile error parity
- missing dep → compile error parity
Use `F = cats.effect.SyncIO` (no runtime needed) so `.use`/`.allocated` run in-test.
