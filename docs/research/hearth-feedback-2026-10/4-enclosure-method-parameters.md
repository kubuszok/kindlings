# `Enclosure.Method` / `enclosingScope` exposes no parameters of the enclosing method

**Verdict:** confirmed gap on both platforms. It is implementable on both without experimental APIs, and a prototype in
this worktree passes on Scala 2.13 and 3.

**Platforms:** Scala 2.13 and Scala 3 (JVM tested). Hearth `0.4.3-4-gaded1a31`.

## Why

macwire treats the parameters of the enclosing method as wiring candidates:
`def make(db: Db, cfg: Config) = wire[Service]`. A Hearth-based macro has no way to find them. `Enclosure.Method` has
only `name, fullName, position, tpe, localValues`. `localValues` holds local `val`s on Scala 2 only, and never
parameters. Both compilers know the parameters: Scala 2 through the owner method symbol's `paramLists`, Scala 3 through
`Symbol.paramSymss`. Neither is experimental, and neither needs the method body. That matters because on Scala 3 the
body is unavailable during the method's own expansion, which is why `localValues` is empty there.

## Reproducer / proof of the gap

Spec `KindlingsFeedbackSpec`, group `(4)`. The macros are expanded inside plain methods of `object ParamNest`, for
example `def params(a: Int, b: String)(implicit c: Long): Data`.

With the 0.4.3 public API, the test "0.4.3 public API: the enclosing method's view has localValues but no parameters"
renders `productElementNames` of the immediately-enclosing `Enclosure.Method`. It passes on both platforms:
`[name, fullName, position, tpe, localValues]`, so there is no parameters field.

The wanted API does not exist. Compiling the fixture against unmodified 0.4.3 (Scala 2.13) fails:

```
[error] .../KindlingsFeedbackFixturesImpl.scala:214:32: value enclosingMethodParameters is not a member of KindlingsFeedbackFixturesImpl.this.EnvironmentModule
[error]     Expr(Data.list(Environment.enclosingMethodParameters.map(p => Data(s"${p.name}: ${p.tpe.plainPrint}"))*))
```

## Prototype (in this worktree, uncommitted)

`hearth/src/main/scala/hearth/Environments.scala` (nested `EnvironmentModule`):

```scala
/** Value parameters (all clauses, incl. implicit/using) of the IMMEDIATELY-enclosing method, each with a `ref` usable
  * in generated code. Empty when not inside a method. Type parameters are not included. */
def enclosingMethodParameters: List[Enclosure.LocalValue]
```

- Scala 3 (`EnvironmentsScala3.scala`): walk `Symbol.spliceOwner.maybeOwner…`, skipping the hidden owners (`macro`,
  `$anonfun`, synthetic macro defs) and local vals, and stopping at a class or package. On the first `isDefDef`, take
  `paramSymss.flatten.filter(_.isTerm)`, then `LocalValue(p.name, p.termRef.widen, Ref(p))`.
- Scala 2 (`EnvironmentsScala2.scala`): the same walk from `c.internal.enclosingOwner`. On the first non-synthetic
  method, take `asMethod.paramLists.flatten`, then
  `LocalValue(name, p.typeSignature, internal.gen.mkAttributedRef(p))`.

Tests (all pass on both 2.13 and 3 with the prototype):

```scala
ParamNest.params(1, "x")(2L)          <==> Data.list(Data("a: scala.Int"), Data("b: java.lang.String"), Data("c: scala.Long"))
ParamNest.genericParams[String](1, "s") <==> Data.list(Data("n: scala.Int"), Data("s: java.lang.String"))   // type params excluded
ParamNest.sum(10, 32, "ignored")      ==> 42    // refs spliced into generated code: a + b
```

Run with the prototype:

- Full `hearthTests2/test`: `Failed: Total 1244, Failed 7` (only the other kindlings-feedback reproducers).
- Full `hearthTests/test`: `Failed: Total 1386, Failed 7` (same). No regressions.

MiMa (`hearth2/mimaReportBinaryIssues`, `hearth/mimaReportBinaryIssues` against 0.4.3):

```
* abstract method enclosingMethodParameters()scala.collection.immutable.List in interface hearth.Environments#EnvironmentModule is present only in current version
  filter with: ProblemFilters.exclude[ReversedMissingMethodProblem]("hearth.Environments#EnvironmentModule.enclosingMethodParameters")
```

This is the nested-scope case from `docs/contributing/binary-compatibility-and-mixins.md`. `EnvironmentModule` is
implemented only by Hearth's own `object Environment`, so the change is not user-observable and can be suppressed.

## API options

1. `Environment.enclosingMethodParameters: List[Enclosure.LocalValue]` (the prototype). It is binary-safe and needs no
   change to `Enclosure.Method`.
2. `Enclosure.Method.parameters: List[List[Enclosure.LocalValue]]`, which keeps the clause structure and
   implicit-ness. Adding a field to the case class changes `apply/copy/unapply`, and that breaks users' compiled
   pattern matches. A `lazy val parameters` member that calls a nested `private[hearth]` hook keeps the shape.
3. Either way, consider a `Parameter`-like record (`isImplicit`, `isByName`, clause index) rather than reusing
   `LocalValue`, because macwire needs to know about implicit-ness. `LocalValue` was reused only to keep the prototype
   small. By-name parameters (`=> T`) are typed `=> T` on Scala 2 in the prototype and still need normalizing.

Also to decide: parameters of OUTER enclosing methods, which are lexically visible in nested defs/lambdas, and lambda
parameters (`$anonfun` owners are skipped today).
