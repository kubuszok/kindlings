# `di` — `wiredInModule` / `Wired` implementation plan

Status: **IMPLEMENTED 2026-06-16** (option 2 — cross-platform lookup/registry). `DI.wiredInModule`
+ `Wired` (`di/src/main/scala/.../Wired.scala`) ship the registry/lookup half on all platforms,
green on Scala 2.13 + 3 (`WiredSpec`, 4 tests). The reflective dynamic-instantiation half
(`wireClassInstance*`) is intentionally **not** ported (JVM-only `java.lang.reflect`).

Implementation notes:
- Subtype-aware lookup uses only `java.lang.Class.isAssignableFrom` (portable); registry keyed by
  `java.lang.Class[Any]`, entries are `() => Any` factories (a `def` member re-reads per lookup).
- The class key is obtained from a summoned **`ClassTag[M]`** (`.runtimeClass`), NOT a `classOf[M]`
  literal: `classOf[M]` cannot be written for the abstract macro type parameter (checked at
  macro-definition time), and reifying `classOf` from a `Type` is not exposed uniformly by Hearth
  — its `ClassExprCodec` reifies the type on Scala 2 but value-lifts on Scala 3. Summoning
  `ClassTag[M]` (materialised by the compiler where `M` is concrete) sidesteps both. *(Possible
  Hearth follow-up: a cross-platform `Type[A] => Expr[Class[A]]`.)*
- `Class` is shadowed in the macro by Hearth's cake `Class[A]`; use `java.lang.Class` explicitly.

The other two "missing macwire surface" items from the module audit:

- **`wireImplicit`** — does **not** exist in macwire under any name (verified against
  `macros/src/main/scala-{2,3}` — only `wire`/`wireSet`/`wireList`/`wireWith`/`wireRec`/
  `wiredInModule`/`autowire`/`autowireMembersOf`). macwire's implicit handling is just that
  plain `wire` routes implicit ctor params through implicit search — which `di` already does
  and tests (`WiringSpec` "resolves implicit parameters via implicit search"). Implementing a
  `wireImplicit` "for parity" would be inventing a phantom feature; **correctly dropped**.
- **`autowireMembersOf`** — **implemented** (cross-platform), see `DI.autowireMembersOf`,
  `WiringMacrosImpl.autowireWithMembers`/`membersOfMarkerInstance`/`expandMembersOf`, and the
  two ported tests in `WiringSpec` ("DI.autowire" group).

## What `wiredInModule` / `Wired` is

`def wiredInModule(in: AnyRef): Wired` (macwire `util` module). The macro enumerates `in`'s
public **nullary** members whose result `<: AnyRef` and emits a runtime
`Wired(Map[Class[_], () => AnyRef](classOf[T1] -> (() => in.m1), ...))`. Everything else is
runtime:

- `Wired extends InstanceLookup with DynamicInstantiate`:
  - `lookup[T](cls: Class[T]): List[T]` — ALL assignable instances incl. subtypes (the map is
    pre-expanded over each registered class's superclasses/interfaces).
  - `lookupSingleOrThrow[T](cls): T` — throws on 0 or >1.
  - `withInstances(AnyRef*) / withInstanceFactory[T](() => T)` — return an extended `Wired`.
  - `wireClassInstance[T](cls): T` / `wireClassInstanceByName(name): Any` — **JVM reflection**:
    read `cls.getConstructors()(0)`, `lookup` each param type (0 or >1 ⇒ `InstantiationException`),
    `ctor.newInstance(...)`; `byName` uses the thread context classloader.

## Why it is deferred — the platform decision

Every existing `di` feature is **cross-platform** (JVM + Scala.js + Scala Native). `Wired`'s
`DynamicInstantiate` is **inherently JVM-only** (`java.lang.reflect.Constructor.newInstance`,
`Thread.getContextClassLoader`, `Class.getConstructors`). macwire's `util` is JVM-first for
these. So shipping `wiredInModule` means choosing one of:

1. **JVM-only `Wired`** — put the reflective `wireClassInstance*` in `di/src/main/scalajvm`,
   and `InstanceLookup` (`lookup`/`lookupSingleOrThrow`, pure `Map`/`Class` ops) in shared
   source. `wiredInModule` macro + lookup work everywhere; instantiation is JVM-only. This
   breaks the "all features on all platforms" invariant for the instantiation half.
2. **Lookup-only cross-platform `Wired`** — ship `wiredInModule` + `lookup`/`withInstances`
   everywhere, and simply **omit** `wireClassInstance*` (the reflective part). Faithful for the
   registry/lookup use case (the common one); drops the dynamic-instantiate use case.
3. **Skip it** — document as intentionally not ported (like the JVM-proxy scopes already are).

Recommendation: **option 2** (cross-platform `wiredInModule` + `Wired` lookup/registry, no
reflective `wireClassInstance*`), with a `scalajvm`-only extension adding `wireClassInstance*`
if/when wanted. This keeps the cross-platform invariant while delivering the headline registry.

## Implementation sketch (when picked up)

Macro (reuses existing helpers):
- `WiringMacrosImpl.wiredInModule(instance: Expr[Any]): Expr[Wired]` — recover the precise type
  via `factory.tpe`/`preciseExpr` (the `wireWith` trick), enumerate members via the existing
  `membersAsValues(instance, Class[T].methods)`, and fold each `Candidate` into a `Map` entry
  `classOf[MemberType] -> (() => candidate.value)` (same fold style as `buildSet`/`buildList`).
- **Open primitive to verify**: emitting `classOf[MemberType]` from a member's `Type[T]`. Need
  a `Type[T] => Expr[Class[T]]` (e.g. `Expr.quote { classOf[T] }` with `T` bound, or a Hearth
  `Type.runtimeClass`-style helper). This is the one new macro primitive `wiredInModule` needs
  and the first thing to confirm via MCP / hearth source before starting.

Runtime: a new shared `di/src/main/scala/hearth/kindlings/di/Wired.scala` (+ `InstanceLookup`),
with `prepareLookupMap` expanding each registered `Class[_]` over `getSuperclass`/`getInterfaces`
for subtype-aware `lookup`. Reflective `wireClassInstance*` in `scalajvm` only (option 1/2).

Bridges: `DICompanionCompat` `def/inline def wiredInModule(in: AnyRef): Wired = …`; `WiringMacros`
`wiredInModuleImpl`.

Tests (port macwire's `WiredTest`): `lookup` returns all assignable instances incl. subtypes;
empty list when absent; `withInstances`/`withInstanceFactory` (factory re-invoked each lookup);
`wiredInModule` only captures public nullary `AnyRef` members. (Plus, if option 1,
`wireClassInstance` exact/subtype/multiple deps + `InstantiationException` on missing/ambiguous,
in a `scalajvm` test.)
