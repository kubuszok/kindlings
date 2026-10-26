# Fact-check: "should MacWire 3.x use Hearth?" (2026-10-25)

An AI agent evaluating Hearth for MacWire 3.x (Scala 3 only, JVM only, Ox capabilities) read kindlings `di`/`di-cats`
at `e4bdffa` (Hearth 0.4.3) and advised against Hearth. This page checks each claim it made about Hearth or about
kindlings, with evidence that can be re-run. Verdicts:

- **TRUE (kindlings)**: true, but it is a kindlings bug or design choice, not a Hearth limitation;
- **TRUE (Hearth gap)**: a real Hearth gap; a reproducer is linked;
- **FALSE**: refuted, with the evidence that refutes it;
- **STALE**: true of the code it read, but caused by a Hearth bug that had already been fixed.

## Claims about Hearth's API

| # | Claim | Verdict | Evidence |
|---|---|---|---|
| 1 | "Hearth's method helpers flatten parameters into one map where implicit is just a flag" | **FALSE** | `Method.Parameters = List[ListMap[String, Parameter]]`, one `ListMap` per parameter list (`hearth/src/main/scala/hearth/typed/Methods.scala:143`). `Method.expectations` lists `NeedsTypes`/`NeedsValues` per step, and `ApplyTypes`/`ApplyValues` are separate builder steps. Hearth's own test pins `make[F[_]](config: String)(implicit ev: Sync[F])` to `NeedsValues(config: String)(ev: Sync[F])` (`hearth-tests/.../MethodsSpec.scala`, added by the fix for kubuszok/hearth#331). The flat map in kindlings came from kindlings' own `.totalParameters.flatten`. Only `Arguments` (the values you pass in) is flat, keyed by parameter name, which is unique across lists. |
| 2 | "di-cats still has to drop to `quotes.reflect` on Scala 3 for `using` parameter lists" | **STALE** | The cited workaround was for kubuszok/hearth#331, closed 2026-07-04 and fixed in **0.4.1** (`git tag --contains b2a99194` → 0.4.1, 0.4.2, 0.4.3). Kindlings was on 0.4.3 and never removed it. It is now removed: `companion.resource[F]` (explicit + `using` lists) goes through `Class[C].method("resource")` → `OnInstance` → `ApplyTypes(F)` → `fold` in shared code (`di-cats/.../ResourceWiringMacrosImpl.scala`, `companionResourceMethod`/`tryCompanionResource`). Both platform bridges lost the reflection code. Proof: `ResourceMethodSpec` ("uses a context-bound companion resource[F: Sync]", "resolves an explicit value parameter of resource[F: Sync](config)") passes on 2.13 and 3. |
| 3 | (found while doing 2) the return type of a generic method after `ApplyTypes` | **TRUE (Hearth gap + Scala 2 bug)** | `knownReturning` is `None` until every list is applied (both platforms), and the final `Result` is `Nothing` on Scala 2. Reproducer: [hearth-gap-method-generic-return-type.md](hearth-gap-method-generic-return-type.md). Kindlings test "ignores a companion resource[F] that does not return a Resource of the type" fails on 2.13 only. |
| 4 | "Hearth's main payoff is cross-compilation; for a Scala 3/JVM-only project it only adds a layer" | **Partly FALSE** | The layer also provides things a Scala 3-only project would otherwise write itself: the `Method` builder chain (instance/type/value lists, defaults, accessibility), `Type.Cache` (`=:=`-keyed memoisation; see #5), `ValDefs`/`ValDefsCache` let-blocks, `DestructuredExpr` (reading lambda/block literals), `Environment.enclosingScope`, and `MIO` error aggregation and logging. Whether that is worth a dependency is a judgement call, but "only cross-compilation" is not accurate. |

## Claims about kindlings `di`

| # | Claim | Verdict | Evidence |
|---|---|---|---|
| 5 | "Generic types collide: graph nodes are keyed by `Type.fqcn`, so `Box[A]` and `Box[B]` share one node" | **TRUE (kindlings)**, now fixed | Reproduced. Scala 3 generated `new TwoTags(tag$macro$1, tag$macro$1)` and `autowire[TwoBoxes](intBox, stringBox)` failed with "unused dependencies: stringBox". The cause was misuse of `Type.fqcn` (`plainPrint.takeWhile(_ != '[')`) as an identity, while Hearth ships `Type.Cache`, which compares with `=:=`. Fixed with `WiringKeys` (`macro-commons/.../WiringGraphLogging.scala`) in `autowire`, `DI.plan` and di-cats. Tests: `WiringSpec` "keeps distinct applications of a generic type…", "constructs distinct applications…", `DIPlanSpec` (including a per-type storage override), `ResourceMethodSpec` "keeps distinct applications…". |
| 6 | "Context functions aren't treated as factories (`fqcn.startsWith("scala.Function")`)" | **TRUE (kindlings) + small Hearth gap** | The check is in kindlings (`WiringMacrosImpl.isFunctionExpr`, di-cats `isFunction`). Hearth has no public predicate for `FunctionN`/`ContextFunctionN` types; one exists only privately in Scala 3 (`UntypedTypesScala3.isContextFunctionType`). |
| 7 | "An enclosing `Ox` would be picked up by implicit search" | **TRUE (kindlings design)** | Kindlings resolves implicit parameters with `Expr.summonImplicit` at the call site, as macwire does. Nothing in Hearth forces this: a macro can fill the `using` list from the members of an explicit scope term (`Class[S].methods` + `Method` application), as #2 now does for its explicit parameters. |
| 8 | "Method-local vals are visible only on Scala 2" | **TRUE, deliberately unsupported** | The Scala 3 compiler gives `DefDef.rhs = None` for the method being expanded (`EnvironmentsScala3.scala` `localValuesOf`, documented on `Enclosure.Method`). A MacWire written without Hearth has the same limit. |
| 9 | (not claimed, found here) enclosing-method **parameters** are not exposed | **TRUE (Hearth gap)** | `Enclosure.Method` has no parameters field, although both compilers know them. Documented in `docs/user-guide/di.md` ("not currently exposed cross-platform"). |
| 10 | "di-cats only chains provided `Resource`s; there's no ownership model" | **TRUE (scope)** | By design: acquisition and release are supplied as `Resource[F, X]`. Unrelated to Hearth. |
| 11 | "The build disables `-Wunused:imports` because of `import x.Underlying as T`" | **TRUE** | `build.sbt:130`. The cause is Scala 3 compiler bugs (scala/scala3 #18564, #19252, #19657, #19912), not Hearth itself, but it is a real cost of the `Underlying` import style. |
| 12 | "`DI.plan`'s `def` storage clashes with lifecycle" | **TRUE (design)** | `asDefs` re-creates on every use, by design (`DIPlanSpec` "asDefs re-creates a dependency on each use"). |

## Audit of every module: hand-written platform code vs existing Hearth API

| Module | Was | Now | Verdict |
|---|---|---|---|
| di / di-cats | `fqcn` node keys; per-platform `companionResource*` reflection (#331 workaround); Scala 3 `splitDeps` with hand `widen` | `Type.Cache` keys; `Method` chain; shared `preciseDependency` via `DestructuredExpr` | replaceable, done |
| cats-tagless | `TaglessKBridgeScala{2,3}`, `mkXKType`, Scala 2 `appliedType` | shared `UntypedType.typeConstructor`/`applyTypeArgs`, `Type.CtorK1.of[TC].apply` (bridges deleted; 81 files, −565 lines net together with cats-derivation) | replaceable, done; `hearth-gap-hkt-ctor-primitives.md` was stale (#284 closed) |
| cats-derivation | Scala 2 `appliedType(weakTypeOf[TC[Any]]…)`, Scala 3 `mk<TC>Type` | `Type.CtorK1.of[TC].apply[F]`, `applyTypeArgs` for `Bi*` | replaceable, done |
| optics | Scala 3 `peelContext` (betaReduce + strip `Inlined`) | `DestructuredExpr.skipContextualWrappers` (0.4.3) | replaceable, done |
| optics | Scala 3 `Varargs` match | kept | **not** replaceable: `VarArgs.toIterable` silently returns `Nil` for `paths*` (proven by `ModifyAllVarargsSpec`), see `hearth-gap-varargs-non-literal.md` |
| fast-show-pretty | Scala 2 `q"$config.startLevel"` | shared `Expr.quote(Expr.splice(config).startLevel)` | replaceable, done (also stops evaluating the config twice on 2.13) |
| mock | Scala 2 `q"Seq(..$args)"` | `VarArgs.from` | replaceable, done |
| refined / iron | `Ctor2.of(...).asUntyped` → `fromUntyped` "for cross-compilation boundaries" | plain `Type.Ctor2.of` | the workaround was unnecessary (integration tests 222 + 148 pass) |
| neotype | raw reflection for underlying type, companion, `make` | `Ctor1.of[Newtype].unapply`, `Expr.singletonOf`, `Method` | mostly replaceable, done; prefix/owner lookup was a real gap, closed by `Type.pseudoCompanionObject` (kubuszok/hearth#389) |
| newtype | raw reflection for prefix + type members | `Type.pseudoCompanionObject` + `unsafeUnwrap` via `Method` | was a real gap (`hearth-gap-type-prefix-and-members.md`), closed by kubuszok/hearth#389 |
| Scala 2 `c.prefix` unwrap (jsoniter, yaml, xml, mock, optics, di `DI.plan`) | — | unchanged | necessary entry-point glue |
| Scala 2 `WeakTypeTag[F[Any]].tpe.typeConstructor` entry recovery (cats, cats-tagless, di-cats) | — | unchanged | necessary: `Type.Ctor1.of[F]`/`CtorK1.of[Alg]` on a macro's own type parameter fails on 2.13, see `hearth-gap-scala2-ctor-of-macro-type-param.md` |

Results with everything applied: all JVM suites, 2.13 3328 passed / 1 failed (the intentional Hearth-bug test below)
and Scala 3 3691 passed / 0 failed.

## Hearth feedback (reproducers in Hearth's own test suite)

Drafts: `docs/research/hearth-feedback-2026-10/*.md`. The failing tests are in
`hearth-feedback-2026-10/hearth-reproducer-tests.patch`, which applies to Hearth `aded1a31` (0.4.3-4).

| # | Item | Verdict | Platforms |
|---|---|---|---|
| 1 | Generic method return type after `ApplyTypes` | gap (both), bug (`Result.Returned = Nothing` on 2.13) | 2.13, 3 |
| 2 | Cross-quotes: `Type.of` before `import x.Underlying as T` | bug; fix verified (`2-cross-quotes-import-order.fix.patch`, full suite green) | 3 |
| 3 | `decompose1/2`, `typeArguments` on singleton types | bug (`Ctor*.unapply` already OK) | 2.13, 3 |
| 4 | Enclosing-method parameters | gap; prototype `Environment.enclosingMethodParameters` passes on both | 2.13, 3 |
| 5 | No `isFunction` / `isContextFunction` predicate | API request | 2.13, 3 |
| 6 | `Type.fqcn` has no scaladoc warning that it is not a type identity | doc request | — |
| 7 | `Ctor2.of[Function1].unapply` | exact types fixed; still wrong for SUBTYPES on Scala 3; "SPI boundary" claim never true | 3 |
| 8 | `VarArgs.toIterable` silently drops non-literal varargs | API gap (`toIterableOption`) | 3 |
| 9 | Path-dependent type prefix / type members / owner | API gap | 2.13, 3 |
| 10 | `Ctor*.of` on a Scala 2 macro's own type parameter | bug (crash / silent abstract `F`) | 2.13 |
| 11 | No `Ctor` for mixed kinds like `Resource[F[_], A]` | API gap (minor) | 2.13, 3 |

## Other findings while porting

- On Scala 3, `Type.of[...]` placed before `import x.Underlying as T` in the same block fails with "Not found: type T".
  See [hearth-gap-cross-quotes-import-order.md](hearth-gap-cross-quotes-import-order.md).
- `Resource[F[_], A]` has a mixed-kind constructor that no `Type.Ctor*` extractor models (`Ctor2` is `(*, *) -> *`;
  scalac rejects `Type.Ctor2.of[Resource]` with "kinds of the type arguments do not conform"). di-cats therefore still
  recognises `Resource` by FQCN.
