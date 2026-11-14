# Hearth gap: `semiEval` and local `val`s / `var`s

Status: **RESOLVED** in Hearth `0.4.3-79-gc2c1ffe-SNAPSHOT` (kubuszok/hearth#401, released in 0.4.4), items 1-4:
`semiEval` follows immutable local vals on Scala 3 (including inline-method proxies), refuses `var`s, refuses members
of classes/objects compiled together with the macro call (also when an earlier incremental run left their class files,
so clean and incremental builds agree), and returns `Left` for lambdas it cannot run (bodies using `match`, `if`,
loops, `try`, `return`, assignments or local definitions). Scala 2 still cannot read a local val (a Scala 2 macro has
no right-hand side for it). jsoniter- and ubjson-derivation's `semiEvalConfig` overrides are removed; the workarounds
described below are history.

Found while making the jsoniter-derivation tests drive the optimized (compile-time config) codegen.

## 1. Local `val`s are not followed (Scala 3 inline proxies included)

`Expr.semiEval` resolves module members, literals and applications, but an `Ident` of a local `val` falls through to
`resolveStableRef` (`hearth/src/main/scala-3/hearth/typed/ExprsScala3.scala`), which only handles modules. On Scala 3
this also affects every non-inline argument of an inline method: the argument is bound to a synthetic proxy `val`, so

```scala
KindlingsJsonValueCodec.derived[A](using JsoniterConfig.default.withSkipUnexpectedFields(false))
```

reaches the macro as `Inlined(None, Nil, Ident("config$proxy1"))` and is not evaluable, while the same call on Scala
2.13 (`derived[A](JsoniterConfig.default.withSkipUnexpectedFields(false))`) is. The proxy's right-hand side is
available via `ident.symbol.tree` (`ValDef(_, _, Some(rhs))`), as is the right-hand side of a user's local
`implicit val config = ...`.

Workaround in Kindlings: `CodecMacros.semiEvalConfig` (jsoniter-derivation, scala-3) follows immutable local `val`s to
their right-hand side (up to 3 levels).

## 2. `var`s are evaluated

`Select(module, getterOfVar)` is evaluated by reflection when the module's class can be loaded, which happens when it
was compiled by an earlier (incremental) compilation run. The value read at compile time is not the runtime value, and
whether the expression is evaluable depends on the incremental-compilation state. Reproducer: a test-side

```scala
object RuntimeConfigs { @volatile var strict: JsoniterConfig = JsoniterConfig.default.withSkipUnexpectedFields(false) }
KindlingsJsonValueCodec.derived[A](using RuntimeConfigs.strict)
```

is treated as a runtime config after `clean`, but as a compile-time one after an incremental recompilation.

Workaround in Kindlings: both `CodecMacros.semiEvalConfig` overrides (scala-2, scala-3) refuse expressions that
reference a `var` (Scala 3: a `Ref` with `Flags.Mutable`; Scala 2: a var or a getter with a setter).

Suggested Hearth fix: `semiEval` should (1) follow immutable local `val`s whose tree is available and (2) refuse
mutable fields / `var` getters.

## 3. Members compiled in the same run

`semiEval` also reads `val`s of `object`s (and calls methods) by reflection. That only works when the object's class
was compiled by an EARLIER compilation run, so `object Cfgs { val c = JsoniterConfig.default.withX }` used from the same
module was a compile-time config after an incremental compilation that did not recompile `Cfgs`, but a runtime config
after `clean`.

Workaround in Kindlings (`CodecMacros.semiEvalConfig`):

- Scala 3: references to class/object members `isDefinedInCurrentRun` are never evaluated reflectively; immutable
  `val`s that cannot be overridden (local vals, `final` vals and `object` vals) are followed to their right-hand side
  (`symbol.tree`), which gives the same value in both cases. Non-final class/trait `val`s are not followed (an override
  would change their value).
- Scala 2: there is no `symbol.tree` for definitions; references to members of the module being compiled make the
  config a runtime config: members compiled in this run (symbols with a source position) and members compiled by an
  earlier incremental run (their `associatedFile` is a class file in one of this compilation's output directories,
  `Global.settings.outputDirs`, read reflectively because the macro API does not expose them). Reproducer:
  `Scala2InlineConfigSpec` "a config from an object of the same module" with `SameModuleConfigs` (jsoniter-derivation,
  scala-2 tests): before the fix it compiled after `clean` but failed after recompiling only the spec.

Suggested Hearth fix: `semiEval` should refuse symbols of the current run (or evaluate their trees), so that the result
does not depend on the incremental state.

## 4. Lambdas evaluate to unusable functions

Found while porting jsoniter-derivation's `PartialFunction` name mappers to ubjson-derivation. On Scala 3, a config
whose function-valued field is a user lambda, e.g. (followed to the local `val`'s right-hand side by
`CodecMacros.semiEvalConfig`)

```scala
implicit val config: UBJsonConfig = UBJsonConfig().withFieldNameMapper { case "x" => "X" }
UBJsonValueCodec.derived[Point]
```

is reported as evaluable (`Right(config)`), but calling `config.fieldNameMapper("x")` at macro time throws
`java.lang.reflect.UndeclaredThrowableException` (the function is a reflective stand-in that cannot run the lambda's
body). Reproducer: `UBJsonJsoniterParitySpec` "name mappers accept partial functions" (shared test).

Workaround in Kindlings (ubjson-derivation `CodecMacrosImpl.semiEvalConfig`): an evaluated config whose mappers throw
when probed is treated as a runtime config.

Suggested Hearth fix: `semiEval` should return `Left` for lambdas it cannot evaluate rather than a value that fails
when used.
