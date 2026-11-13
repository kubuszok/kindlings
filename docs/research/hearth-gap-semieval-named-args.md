# Hearth gap: `semiEval` and named arguments (Scala 3)

Status: **RESOLVED** in Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (kubuszok/hearth#401, to be released as 0.4.4): `semiEval`
evaluates named arguments on Scala 3 and follows the `$proxy` vals of inline methods itself, as the whole argument
and inside an argument (`Configuration(.., f$proxy1, ..)`), including `Inlined` bindings. `SemiEvalConfigScala3` no
longer strips `NamedArg`s or inlines proxies. Two things are still done in Kindlings (see the end of this document):
refusing the default arguments of another inline method (evaluating them still breaks the compilation of the
enclosing class), and following a lazy local val (a local `given Configuration = ...`), which Hearth skips.

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

## Workaround in Kindlings

`SemiEvalConfigScala3.semiEvalConfig` (circe-derivation, scala-3) retries `semiEval` on the tree with every
`NamedArg(_, arg)` replaced by `arg` (a `TreeMap`), after following local vals like jsoniter-derivation does.

## Suggested Hearth fix

`semiEval` (Scala 3) should unwrap `NamedArg` (its argument is already in positional order).

## Related limitation: arguments that are inline-method proxies (Scala 3)

A configuration built inside an inline method from its parameters (`inline def deriveEnum[A](f: String => String) =
derived[A](using Configuration(transformConstructorNames = f, enumAsStrings = true))`) reaches the macro as
`Configuration(.., f$proxy1, ..)`: `semiEval` cannot resolve the local proxy `val`. `SemiEvalConfigScala3` also replaces
references to `$proxy` `val`s inside the tree with their right-hand sides (from `Symbol.tree`), refusing trees that then
contain a block (a lambda of the call site) or a default-argument getter of another method. Evaluating the latter
(`KindlingsEncoder.derive$default$4[A]`, the default of an inline method's parameter) broke the compilation of the
enclosing class (`CirceScala3Spec is already defined`), so `deriveEnum` is an overload rather than taking a default
argument.

Suggested Hearth fix: `semiEval` (Scala 3) could follow inline proxies (`Inlined` bindings) itself.

## Still worked around with Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (2026-11-13)

`SemiEvalConfigScala3` now only inspects the configuration with its local vals inlined, and calls Hearth's `semiEval`
on the original expression unless:

- **it calls the default argument of another (inline) method.** `KindlingsEncoder.derive[Shape](discriminator =
  Some("type"))` builds `Configuration(derive$default$1[Shape], ..., discriminator$proxy1, ...)`. Hearth now follows the
  proxies itself and evaluates such a default getter, and the compilation of the enclosing class fails with
  `CirceScala3Spec is already defined as class CirceScala3Spec` (reported at the class, no macro position). Verified by
  disabling the check: `circeDerivation/testFull` then fails to compile `CirceScala3Spec` (tests 427-433:
  `KindlingsCodecAsObject.derive`, `KindlingsEncoder.derive`, `KindlingsDecoder.derive` with named arguments).
  Reproducer (not minimized outside Kindlings): an `inline def derive[A](x: Int = 1, y: Int = 2) =
  macroCall[A](using Cfg(x, y))` whose macro calls `semiEval` on the configuration, called as `derive[A](y = 3)` from a
  test class. `deriveEnum` therefore stays an overload rather than taking a default argument.
- Hearth's `semiEval` returns `Left` and the configuration is a reference to a lazy local val, e.g. a local
  `given Configuration = Configuration(enumAsStrings = true)` (`CirceScala3Spec` "a compile-time configuration in a
  local val is evaluated"): Hearth skips lazy local vals ("its right-hand side may refer to itself"), so it is followed
  to its right-hand side here.

The `NamedArg` retry and the `$proxy` inlining are removed (Hearth does both). `SemiEvalConfigScala2` (which refused
`var`s) is removed too.
