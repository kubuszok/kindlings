# di-cats: F-agnostic `Resource[F, _]` wiring (design + macwire autocats analysis)

Status: design captured 2026-06-14; **V2 implemented 2026-06-15** (factory methods,
recursive intermediate construction with sharing, topological ordering, unused-provider
error, macwire-style missing-dependency path messages). Implements the `di-cats` module —
an F-agnostic re-imagining of macwire's `autocats`
(`com.softwaremill.macwire.autocats.autowire`).

## V2 implementation notes (2026-06-15)

The construction core was rewritten from the V1 "classify + fold monadic providers"
shape into a memoized graph walk (modelled on `di`'s `autowire`), emitting a
`Resource[F, T]` chain:

- **Resolution phase** (compile-time, no tree building): DFS from the root `T`. For each
  needed type, find the single provider whose `resultType <:< type` (Instance / Resource /
  Effect / **Factory**), or — if none and the type is wireable (not `java.*`/`scala.*`) —
  **recursively construct** it from the graph. Each type is memoized by fqcn, so a type
  feeding several params is built **once and shared**. Missing deps are collected with a
  breadcrumb path; unmet `[constructor/method Owner].param` crumbs become the macwire
  message `Failed to create an instance of [T].\nMissing dependency of type [X]. Path …`.
- **Emission phase**: monadic nodes (Resource / Effect / Factory-result / constructed
  intermediate wrapped in `Resource.pure`) are folded **child-before-parent** (a valid
  topological order, since a param's node is registered before its owner's) into nested
  `flatMap`s; instances are spliced directly. Innermost body = `Resource.pure[F, T](root)`
  (or the bound root provider when a provider matches `T` directly).
- **Factory (FunctionN) deps**: classified by `Type.fqcn[D].startsWith("scala.Function")`
  (the same reliable check `di`'s `autowire` uses — simpler and more robust across 2.13/3
  than `Type.Ctor2.fromUntyped[Function1]` decomposition, which is only needed when you
  must split the *function type's* arg/return; here we read the SAM `apply`'s params and
  `knownReturning` directly). The factory's own params are resolved from the graph; its
  result is wrapped per its return shape (`Resource`/`F[_]`/plain). Verified on 2.13 + 3,
  JVM + JS.

### Deliberate divergence from macwire (`constructInputProvider`)

macwire NEVER reuses a directly-passed root instance — it always rebuilds the root via a
creator, so `autowire[B](new B(new A("s")))` FAILS on the missing `String`.

**This is not a correctness safeguard — it is a self-acknowledged macwire wart.** Verified
against the macwire source: the behaviour comes from one rule, *"We assume that we cannot use
input provider directly, so we create a result object with available constructors. It's a
mimic of `wire`'s property"* (`autocats/internals/CatsProvidersGraphContext.scala`), i.e.
`autowire[T]` is modelled as "always **construct** T from the supplied dependency pieces", and
passing a `T` itself falls outside that model. The macwire test for this case
(`constructInputProvider.failure`) is even annotated `// TODO we should add a warning in this
case.` — the authors consider the resulting confusing missing-dependency error *suboptimal*,
not desirable. No bug was ever caused by reusing an instance.

**di-cats deliberately diverges** (confirmed the right call): a provided instance whose type
matches the root is reused as the root (wrapped in `Resource.pure`). Rationale: least-
surprising ("if you hand me a value of the type I'm building, I use it"), consistent with how
every other parameter resolves to a provided value by type, and it avoids exactly the
confusing failure macwire flagged as a TODO. The only consequence (expected): a fully-
constructed root's *own* transitive dependencies are not lifecycle-managed — the caller
constructed them eagerly. Tested in `ResourceWiringSpec` ("deliberate divergence from
macwire").

### Deferred

- `taggingParameters` (softwaremill `@@`-tagged disambiguation) — needs a tagging-type
  provider; left cleanly unimplemented (no failing test).

---

Original design (V1) follows.

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
