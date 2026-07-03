# Hearth gap: `Method.fold` drops an implicit clause that follows a type-parameter clause

**Status:** reproducer for an upstream Hearth bug — filed as
[kubuszok/hearth#331](https://github.com/kubuszok/hearth/issues/331).
**Discovered by:** the `di-cats` companion `resource[F]` auto-discovery work (kindlings PR #159).
**Hearth version:** see `versions.hearth` in `build.sbt`.

## Symptom

Given a polymorphic factory method whose *implicit* (context-bound / `using`) parameter clause
comes **after** a type-parameter clause, e.g.

```scala
object Widget {
  def resource[F[_]: Sync]: Resource[F, Widget] = ???
  //                ^^^^^  ->  desugars to  (implicit ev: Sync[F])  AFTER the [F[_]] clause
}
```

Hearth's `Method` API cannot build a call to it. When you `Method.fold` over the method and apply
the type argument `F` via `onTypes`, the trailing `implicit Sync[F]` clause is **never offered** to
`onValues` — the fold sees an *empty* value clause instead of one containing `ev: Sync[F]`. There is
therefore no way to supply the `Sync[F]` instance through the `Method` API, and the call cannot be
constructed.

The same happens for an *explicit* value clause after a type-param clause
(`def make[F[_]](config: Config): F[Widget]`): `onValues` is handed `List.empty`, so the `config`
parameter is invisible.

## Minimal reproducer

A method shape that triggers it (type-param clause, then a value/implicit clause that references the
type parameter):

```scala
trait Sync[F[_]]

final case class Widget()
object Widget {
  // implicit clause AFTER the type-param clause — `Sync[F]` mentions the just-introduced `F`
  def resource[F[_]](implicit ev: Sync[F]): F[Widget] = ???
}
```

Macro-side (pseudo-code against the Hearth `Method` API):

```scala
val method: Method = /* the `resource` method of Widget's companion */
method.fold(
  onInstance = ...,
  onTypes    = clause => /* apply F := SomeEffect */ ...,
  onValues   = clause => {
    // BUG: for the `(implicit ev: Sync[F])` clause, `clause` is EMPTY here.
    //      Expected: a single parameter `ev: Sync[SomeEffect]` to be summoned & supplied.
    ...
  }
)
```

`onValues` is invoked with an empty clause, so the derivation cannot summon `Sync[SomeEffect]` and
apply it. A value clause that appears *before* any type-param clause is handled correctly — the gap
is specifically "a value/implicit clause that follows a type-parameter clause."

## Root cause (Hearth source)

`hearth/src/main/scala-3/hearth/untyped/UntypedMethodsScala3.scala`.

`UntypedMethod.methodExpectations` (~L279) walks `paramSymss` **in order** and correctly emits the
trailing clause as `NeedsValues(...)` with the (still-abstract) parameter types — e.g. `Sync[F]`
where `F` is the not-yet-applied type parameter (L306–320).

The information is then **thrown away** in `UntypedMethod.toTyped` (~L764), which maps the untyped
expectations to typed ones:

```scala
var seenTypeParams = false
val typedExpectations = untypedExpectations.map {
  case NeedsInstance   => MethodExpectation.NeedsInstance
  case NeedsTypes(utp) => seenTypeParams = true; MethodExpectation.NeedsTypes(...)
  case NeedsValues(up) =>
    if (seenTypeParams) MethodExpectation.NeedsValues(List.empty)   // <-- the gap (L781-783)
    else                MethodExpectation.NeedsValues(up.asTyped[Instance])
}
```

Once a type-parameter clause has been seen, any following value clause is collapsed to
`NeedsValues(List.empty)`. The reason it is dropped rather than typed is that `up.asTyped[Instance]`
would try to resolve the parameter types (`Sync[F]`) against `Instance` while `F` is still an
un-applied abstract type parameter — Hearth does not substitute the type arguments applied via
`onTypes` into the param types of subsequent clauses, so it cannot produce a well-typed parameter and
bails out to an empty list.

The Scala 2 path has the analogous limitation.

## Ideal fix (upstream)

When `onTypes` applies type arguments for a `NeedsTypes` clause, **substitute** those arguments into
the parameter types of all subsequent `NeedsValues` clauses, so that `toTyped`/`asTyped` can present
`Sync[SomeEffect]` (concrete) to `onValues`. Equivalently, expose a mode where the remaining
implicit clauses are resolved by the compiler's own implicit search after the type arguments are
fixed.

## Current workaround in kindlings (di-cats)

`di-cats` bypasses the `Method` API entirely for `resource[F]` and builds the call by **raw
per-platform reflection**, applying `F` first and then filling the now-`F`-substituted clauses:

- Scala 3 — `ResourceWiringMacros.companionResourceCall`
  (`di-cats/src/main/scala-3/.../ResourceWiringMacros.scala`): `companionTerm.select(sym)
  .appliedToType(fRepr)` to apply `F`, then a recursive `applyClauses` that walks each remaining
  `MethodType`, runs `Implicits.search(paramType)` on the (already `F`-substituted) param types, and
  `Apply`s the results. An explicit (non-implicit) clause makes it bail with `None`.
- Scala 2 — `ResourceWiringMacros.companionResourceCall`
  (`di-cats/src/main/scala-2/.../ResourceWiringMacros.scala`): builds `q"($companion.$method[$F]):
  $expected"` and lets `c.typecheck` insert the implicit `Sync[F]` clause via its own implicit search.

If/when Hearth substitutes applied type args into subsequent clause param types, both
`companionResourceCall` implementations can be simplified back to a single `Method.fold`.

## Related

- `docs/research/HANDOFF-next-session.md` issue #3.
- kindlings `di-cats` `resource[F]` support (PR #159).
</content>
</invoke>
