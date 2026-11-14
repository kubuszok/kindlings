# Hearth gap: `semiEval` and named arguments (Scala 3)

Status: **RESOLVED**. Named arguments and inline proxies: Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (kubuszok/hearth#401).
`semiEval` evaluates named arguments on Scala 3 and follows the `$proxy` vals of inline methods itself, as the whole
argument and inside an argument (`Configuration(.., f$proxy1, ..)`), including `Inlined` bindings. The
"already defined" failure that followed: Hearth `0.4.3-90-gc3f9223-SNAPSHOT` (kubuszok/hearth#402). A `LinkageError`
from reflection is now a `Left` on both Scala versions. Both are released in 0.4.4. The only thing still done in
Kindlings is following a lazy local val (a local `given Configuration = ...`), which Hearth skips by design. See the
last two sections.

Found while making circe-derivation evaluate its `Configuration` at compile time (audit round 3, item 5g).

## Symptom

On Scala 3, `Expr.semiEval` fails on an application with named arguments:

```scala
// hearth.kindlings.circederivation.Configuration is a case class with default parameters
KindlingsEncoder.encode[Shape](Circle(1.0))(using Configuration(enumAsStrings = true))
```

The argument reaches the macro as a proxy `val` (Scala 3 binds every non-inline argument of an inline method to a
local proxy `val`, and `semiEval` does not follow local vals; documented by the jsoniter-derivation work in
`hearth-gap-semieval-local-vals-and-vars.md`, worked around by following immutable local vals to their right-hand side,
as circe-derivation now does too). The proxy's right-hand side is

```scala
Configuration.apply(
  Configuration.$lessinit$greater$default$1,
  ...,
  enumAsStrings = true,            // a NamedArg tree
  Configuration.$lessinit$greater$default$7
)
```

The arguments are already in positional order (the defaults are filled in), but the named ones keep their `NamedArg`
wrapper, and `semiEval` reports `Cannot semi-evaluate expression: enumAsStrings = true`. Every configuration built
with named arguments (`Configuration(discriminator = Some("type"))`, `Configuration(strictDecoding = true)`, ...) is
therefore only known at runtime, so compile-time checks (e.g. `enumAsStrings` on a sum with non-singleton cases) never
fire.

Scala 2 is not affected: the typer rewrites named/default arguments into a block of local vals
(`{ val x$1 = true; ...; Configuration.apply(x$2, ..., x$1, ...) }`), which `semiEval` evaluates.

## Reproducer

Any Scala 3 macro calling `semiEval` on `'{ Configuration(enumAsStrings = true) }`-like trees (a case class or a
method with default parameters, called with at least one named argument):

```scala
final case class Cfg(a: Boolean = false, b: Int = 1)
inline def probe(inline cfg: Cfg): Unit = ${ probeImpl('cfg) } // probeImpl reports cfg.semiEval
probe(Cfg(b = 2)) // Left(Cannot semi-evaluate expression: b = 2)
probe(Cfg(false, 2)) // Right(Cfg(false, 2))
```

## Related limitation (Scala 2)

A local `implicit val config = Configuration(...)` is not evaluated on Scala 2: `semiEval` does not resolve local vals
and a Scala 2 macro cannot read a local val's right-hand side from its symbol. Only an inline expression
(`KindlingsEncoder.derived[A](using Configuration(...))`, `Configuration.default.withX`, a stable module member) is
known at compile time there. `semiEval` also reads `var`s by reflection from previously compiled classes, so
configurations referencing a `var` are refused (treated as runtime ones) on both versions.

## Former workaround in Kindlings (removed with 0.4.3-79)

`SemiEvalConfigScala3.semiEvalConfig` (circe-derivation, scala-3) retries `semiEval` on the tree with every
`NamedArg(_, arg)` replaced by `arg` (a `TreeMap`), after following local vals like jsoniter-derivation does.

## Suggested Hearth fix

`semiEval` (Scala 3) should unwrap `NamedArg` (its argument is already in positional order).

## Related limitation: arguments that are inline-method proxies (Scala 3)

A configuration built inside an inline method from its parameters (`inline def deriveEnum[A](f: String => String) =
derived[A](using Configuration(transformConstructorNames = f, enumAsStrings = true))`) reaches the macro as
`Configuration(.., f$proxy1, ..)`: `semiEval` cannot resolve the local proxy `val`. (Historical: Hearth follows
proxies since 0.4.3-79, and the cause of the failure below is corrected in the next section.)
`SemiEvalConfigScala3` also replaced references to `$proxy` `val`s inside the tree with their right-hand sides (from `Symbol.tree`), refusing trees that then
contain a block (a lambda of the call site) or a default-argument getter of another method. Evaluating the latter
(`KindlingsEncoder.derive$default$4[A]`, the default of an inline method's parameter) broke the compilation of the
enclosing class (`CirceScala3Spec is already defined`), so `deriveEnum` is an overload rather than taking a default
argument.

Suggested Hearth fix: `semiEval` (Scala 3) could follow inline proxies (`Inlined` bindings) itself.

## The "already defined" failure (Hearth `0.4.3-79`, resolved in `0.4.3-90`)

With 0.4.3-79, `KindlingsEncoder.derive[Shape](discriminator = Some("type"))` (and the `KindlingsDecoder` /
`KindlingsCodecAsObject` variants) broke the compilation of the enclosing class:
`CirceScala3Spec is already defined as class CirceScala3Spec`, with no macro position. Kindlings worked around it in
`SemiEvalConfigScala3`, which refused every configuration calling another method's default getter
(`derive$default$N`).

**Corrected cause: not about default getters.** To evaluate `KindlingsEncoder.derive$default$1`, `semiEval` calls
`getClass.getMethods` on the companion. Scala 3 compiled `KindlingsEncoderCompanionCompat` with inline accessors such
as `inline$EncoderMacros$i1(x$0: hearth.kindlings.circederivation.internal.compiletime)`. They exist because an inline
method of the trait calls a macro on the `private[circederivation] object internal.compiletime.EncoderMacros`, which is
in a subpackage. The accessors take the PACKAGE as a parameter, which erases to a class that does not exist, so
`getMethods` threw `NoClassDefFoundError: hearth/kindlings/circederivation/internal/compiletime`. Nothing in `semiEval`
caught it. When a throwable escapes a macro, dotty's splicer treats it as a class compiled in the same run: it suspends
the compilation unit and fails to compile it later, with "already defined". 0.4.3-24 had the same unguarded
`getMethods`, but it returned `Left` earlier, because it did not follow the inline proxies. Hearth reproducer:
`ExprsScala3Spec` "does not throw when the class of the object owning a default argument cannot be reflected on"
(types `hearth.examples.brokenaccessors`).

**With 0.4.3-90**, `semiEval` returns
`Left("... reflection failed with java.lang.NoClassDefFoundError: hearth/kindlings/circederivation/internal/compiletime")`
for these configurations. The refusal in `SemiEvalConfigScala3` is removed. The named-argument factories compile, and
their configuration is a runtime one, as it was with the workaround. They cannot be evaluated at compile time, because
the companions cannot be listed by reflection. (A Hearth lookup that does not list every method, e.g. `getMethod` with
the exact parameter types, could evaluate them.) For the same reason, `deriveEnum` stays an overload
(`deriveEnum[A]()`) rather than taking a default argument. This was checked: with
`transformConstructorNames: String => String = Configuration.default.transformConstructorNames`,
`KindlingsCodecAsObject.deriveEnum[Shape]()` is no longer a compile-time error ("enumAsStrings is enabled but ...").

## Still done in Kindlings: lazy local vals

Hearth's `semiEval` skips lazy local vals ("its right-hand side may refer to itself"), and a local
`given Configuration = Configuration(enumAsStrings = true)` is one (`CirceScala3Spec` "a compile-time configuration in
a local val is evaluated"). With 0.4.3-90 Hearth still returns
`Left("Cannot semi-evaluate expression: given_Configuration")`, so `SemiEvalConfigScala3` follows such a reference to
its right-hand side. That is all the class does now.
