---
name: hearth-expr-parsing-dsl
description: >
  Parsing selector/path lambdas (`_.a.b.each.when[T]`) and building optics-like marker DSLs
  with Hearth. DestructuredExpr step-chain walking, per-platform markers + invariant
  evidence, CaseClass copy-with-modification, non-exhaustive MatchCase, runtime-typeclass
  delegation. Reference: the `optics` module (quicklens reimplemented).
paths:
  - "optics/**/*.scala"
  - "**/syntax.scala"
  - "**/internal/compiletime/*.scala"
user-invocable: false
---

# Skill: Parsing expressions & building optics-like DSLs

How to take a user-written **selector lambda** like `_.address.city`,
`_.people.each.name`, `_.xs.at(0)`, or `_.animal.when[Dog].name`, parse it at compile time,
and emit a transformation. This is the machinery behind the `optics` module (a Hearth
reimplementation of SoftwareMill quicklens) and applies to any "path DSL" (lenses, query
builders, the mock `.expects` DSL).

This is a **non-derivation macro module** — no rule chain, no `MIO`, no `ValDefsCache`, no
standard extensions. Use the 3-layer recipe from [the non-derivation modules](#module-recipe)
(`di`, `mock`, `optics`), NOT [kindlings-new-module](../kindlings-new-module/SKILL.md) (which
is for type-class derivation).

## 1. Parse the selector lambda

`DestructuredExpr` turns an `Expr` tree into **semantic** structure (not a raw AST mirror).
Key entry points (verify signatures in `hearth/typed/Exprs.scala` and via MCP):

- `DestructuredExpr.parse(expr): DestructuredExpr` — full parse; `.collect { case ... }` walks
  it pre-order.
- `DestructuredExpr.extractLambda(expr)` — strip `Inlined`/`Block(DefDef…)` wrappers to the
  lambda body.
- `DestructuredExpr.extractFieldPath[S, A](path): Either[String, FieldPath]` — **field-only**
  paths (`_.a.b.c`) → `FieldPath { def fieldNames: List[String] }`. Purpose-built and simple;
  it `Left`s the moment a step is a method call (`.each`, `.at`, ...). Use it for Phase-1
  field-only lenses; use the `MethodCall` walker (below) once `.each`/`.at`/`.when` enter.
- `DestructuredExpr.MethodCall` — a resolved method/field call with
  `method: Method` and `applied: List[MethodCall.Applied]`, where `Applied` is one of:
  - `AppliedInstance(value: DestructuredExpr)` — the receiver,
  - `AppliedTypes(typeArgs: List[??])` — a `[T]` clause,
  - `AppliedValues(args: List[DestructuredExpr])` — a `(...)` clause.

**Extract the referenced method name** (e.g. for the mock `.expects` DSL): the OUTERMOST
`MethodCall` in the parsed tree is the referenced method —
`parse(f).collect { case mc: DestructuredExpr.MethodCall => mc }.headOption.map(_.method.name)`.

**Recover a precise existential** from a parsed node: `toTypedExpr(node) =
node.toUntypedExpr.asTyped[node.tpe.Underlying].as_??` (see di's `preciseExpr`).

## 2. Walk the step chain (mixed fields + markers)

A path like `_.people.each.name` is a chain of steps. Walk the `MethodCall` chain and classify
each node by `mc.method.name` into a step ADT — `Field(name)`, `Each`, `At`, `When`, ... — and
recover each step's data from its `applied` clauses:

- a plain field access → `Field(mc.method.name)`;
- a marker call (`each`/`at`/`when`/...) → the corresponding step, with:
  - the **receiver/prefix** from `AppliedInstance.value` (e.g. `s.people: List[Person]`),
  - **value args** (`.at(i)`, `.atOrElse(i, default)`, `.eachWhere(pred)`) from
    `AppliedValues.args`,
  - **type args** (`.when[T]`) from `AppliedTypes.typeArgs`.

⚠️ **Scala 3 desugars an `extension [C] def when[T <: C]` into TWO `AppliedTypes` clauses** —
the extension's `[C]` then the method's `[T]`. Take the **last** type-arg clause's last arg,
or you pick the parent type and break narrowing.

⚠️ Filter out **evidence value-args**: a marker that takes a `using`/implicit evidence (see §3)
exposes it as a leading value arg on Scala 3. Drop args whose type `<:<` a shared
`trait PathStepEvidence` marker (not `sealed` — the per-platform evidences extend it from their
own files) before reading the real `i`/`default`/`pred`.

## 3. Build the marker DSL (per platform) with invariant evidence

The `.each`/`.at`/`.when` "methods" are **markers** that only let the path type-check; the
macro rewrites them and never runs their bodies. They are **per-platform** (a genuine language
difference, like the `di`/`mock` bridges — NOT hidden failures):

- **Scala 2**: an `EachOps[C, A]` class plus an implicit **conversion** `toEachOps[C](c: C)(implicit
  ev: IsElementOf[C]): EachOps[C, ev.Elem]` — NOT an `implicit class`. See the whitebox note below.
- **Scala 3**: `extension [C, A](c: C)(using IsElementOf.Aux[C, A]) def each: A`

Each marker body is `sys.error("...only usable inside modify...")` and carries
`@scala.annotation.compileTimeOnly(...)`.

⚠️ **`@compileTimeOnly` fires on the Scala 3 `extension` but NOT the Scala 2 `implicit class`**
— so `.each` outside `modify` is compile-rejected on S3 but only `sys.error`s at runtime on S2.
Do not assert its compile message cross-platform.

⚠️ **Invariant evidence is mandatory** to pin the element type. A bare `extension [F[_], A](fa: F[A])`
marker lets Scala 2 widen `List[Int].each` to `A = Any` (since `List[Int] <: List[Any]`). Fix:
key the marker on an **invariantly-parameterised** evidence on the exact container type:

```scala
sealed trait IsElementOf[C] extends PathStepEvidence { type Elem }
object IsElementOf {
  type Aux[C, A] = IsElementOf[C] { type Elem = A }
  def witness[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]
  // ONE materializer macro that consults Hearth's IsCollection/IsMap/IsOption SPI and returns the refined Aux:
  implicit def derived[C]: IsElementOf[C] = macro internal.compiletime.ModifyMacros.isElementOfImpl[C]   // Scala 2 (whitebox)
  // Scala 3:  transparent inline given derived[C]: IsElementOf[C] = ${ ModifyMacros.isElementOfImpl[C] }
}
```

The evidence is **materialized by a macro** (`deriveIsElementOf` consults the SPI), so any type
with an `IsCollection`/`IsMap`/`IsOption` provider — built-ins plus anything on the classpath
(cats `NonEmpty*` via `kindlings-cats-integration`, java collections, …) — gets `.each` with no
extra module. The macro must return a type **more specific** than its signature (the refined
`Aux[C, Elem]`), which on Scala 2 requires a **whitebox** macro: declare `val c: whitebox.Context`
on the Hearth bundle (`whitebox.Context <: blackbox.Context`, so `MacroCommonsScala2` is satisfied).
On Scala 3 a `transparent inline given` does the same.

⚠️ **Scala 2 consumption needs an implicit CONVERSION, not an `implicit class`.** Scala 2 can't
infer a class type param (`A` in `EachOps[C, A]`) from a whitebox expansion, and a class *field*
`val ev` would widen the refinement away. Instead summon the evidence as a **method** implicit and
project its refined member into the ops type param: `implicit def toEachOps[C](c: C)(implicit ev:
IsElementOf[C]): EachOps[C, ev.Elem]`. `ev.Elem` reduces to the concrete element type at the call
site and is baked into `A`. Mirror with `IsIndexedElementOf`/`IsSingleElementOf`/`IsEither` for
`.at`/Option/`Either` (the latter shares a `sealed`-free `PathStepEvidence` supertype so the parser
can recognise a synthesized evidence value-arg with one `Type.isSubtypeOf[_, PathStepEvidence]`).

⚠️ **Name clash inside the impl:** the std SPI mixes in an `IsEither` extractor that shadows the
optics `IsEither` evidence; alias the optics one (`import …optics.{IsEither => OpticsEither}`) for
the signature, and reference it by its **full `_root_` path inside the quote** (an import rename is
not a real object name, so emitting it produces an unresolvable symbol).

## 4. Emit copy-with-modification (case classes)

To rebuild `S` with one field replaced (`.copy(field = …)`):

```scala
CaseClass.parse[S] match {
  case ClassViewResult.Compatible(cc) =>
    val fields: Map[String, Expr_??] = cc.caseFieldValuesAt(sExpr) // read every field
    val rebuilt = cc.construct[Id](p => fields(p.name))            // primary ctor, by name
  case ClassViewResult.Incompatible(reason) => Environment.reportErrorAndAbort(...)
}
```

- `construct` needs an `Applicative`+`DirectStyle` `F`. `Either[String, *]` has no `Applicative`
  in hearth-micro-fp; use `Id` and abort errors directly via `Environment.reportErrorAndAbort`.
- Recurse into the focused field via a **helper method with a regular type parameter** so the
  field's path-dependent `Underlying` never enters an `Expr.quote` (the Scala 2 leak — see
  [cross-compilation pitfall #3](../hearth-cross-compilation/pitfalls.md)). Pattern (optics
  `recurseInto`): `import focused.Underlying as F; buildModify[F](focused.value, rest)(...)`.

## 5. Non-exhaustive subtype match (`.when[Subtype]`)

`.when[Sub]` is `v match { case s: Sub => f(s); case other => other }` — NOT exhaustive enum
derivation. Build it with two `MatchCase` arms (no wildcard primitive needed):

```scala
MatchCase.matchOn[S, S](scrutinee)(NonEmptyVector(
  MatchCase.typeMatch[Sub](...),  // arm: narrow + recurse
  MatchCase.typeMatch[S](...)     // catch-all: match the PARENT type, return unchanged
))
```

Matching the parent type as the second arm is the fall-through. On Scala 3 `matchOn` annotates
the scrutinee `@unchecked`, so the non-exhaustive match is warning-free on both platforms.

## 6. Open-code collection steps through the SPI (no runtime typeclasses)

`.each`/`.eachWhere`, `.at`/`.index`/`.atOrElse` and `.eachLeft`/`.eachRight` are emitted by the
macro directly via Hearth's `IsCollection`/`IsMap`/`IsOption`/`IsEither` SPI — there are **no**
runtime functor type classes. The macro dispatches on the container type
(`Type[S] match { case IsMap(m) => …; case IsCollection(c) => …; case IsOption(o) => …; case _ => abort }`),
rebuilds the container via the provider's `factory`/`build`, and recurses into the focused element.
The indexed steps are the same iterate-and-rebuild at one position/key (a `Seq` keyed by `Int`, a
`Map` by key, an `Option`'s single value) — **no positional-update SPI** is needed (`.at` has no
bespoke `IsIndex` SPI). `.each` therefore works over EVERY container any provider supports — built-ins
plus anything on the classpath — and a new provider jar turns it on with no code change here.

Use `LambdaBuilder` ONLY for the actual collection/Optional iteration lambda
([LambdaBuilder](../hearth-lambda-builder/SKILL.md) — that IS collection iteration). When rebuilding
a `Seq`, restrict positional `.at` to `immutable.Seq` so it is not offered on unordered collections.

## <a name="module-recipe"></a>7. Non-derivation 3-layer module recipe

Same shape as `di`/`mock`/`optics`:

- **Shared** `private[mod] trait XMacrosImpl { this: MacroCommons => def x[...]: Expr[...] }`.
- **Per-platform bridge** `internal/compiletime/XMacros.scala`: S2
  `class XMacros(val c: blackbox.Context) extends MacroCommonsScala2 with XMacrosImpl`; S3
  `class XMacros(q: Quotes) extends MacroCommonsScala3(using q), XMacrosImpl` + an object with
  `xImpl[...](...): Expr[...] = new XMacros(q).x[...]`.
- **Per-platform `syntax.scala`** for the DSL surface: S2 `implicit class … def x = macro …`;
  S3 `extension (inline obj) inline def x = ${ … }`. A `package object` mixes the same trait in
  for bare imports.

## Reference

- `optics/` — the full path DSL: `internal/compiletime/ModifyMacrosImpl.scala` (parser + copy
  gen), per-platform `IsElementOf.scala` / `PathStepEvidences.scala` (the whitebox-S2 /
  transparent-inline-S3 evidence macros) with shared `PathStepEvidence.scala` (their marker
  supertype), per-platform `syntax.scala` (markers). Plan: `docs/research/optics-port-plan.md`; findings:
  `docs/research/optics-each-inference.md`.
- `mock/.../MockMacrosImpl.scala` `extractMethodRef` — the `.expects` method-name extraction.
- `di/.../WiringMacrosImpl.scala` `preciseExpr` / `autowireWithMembers` — `DestructuredExpr`
  type recovery + marker (`autowireMembersOf`) detection.

## Related skills

- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) — path-dependent leaks,
  invariant-evidence, `@compileTimeOnly`, `classOf`/`ClassTag`, context-function pitfalls.
- [hearth-api-reference](../hearth-api-reference/SKILL.md) — `DestructuredExpr`/`MatchCase`/
  `CaseClass` signatures.
- [hearth-lambda-builder](../hearth-lambda-builder/SKILL.md) — element-lambda construction.
