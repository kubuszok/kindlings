# AGENTS

This file guides coding agents working in this repository.

## Project Overview

**Kindlings** provides type classes (re)implemented with [Hearth](https://github.com/MateuszKubuszok/hearth/) to kick-off the ecosystem. Each module demonstrates how to use Hearth's macro-agnostic APIs to implement cross-compiled (Scala 2.13 + 3) type class derivation.

- **Language**: Scala (pure Scala library)
- **Scala Versions**: 2.13.18, 3.7.4
- **Platforms**: JVM, Scala.js, Scala Native
- **Build Tool**: SBT (but see restrictions below)
- **License**: Apache 2.0

## Using MCP for compilation context

This project has Metals MCP available at `.metals/mcp.json` as **`kindlings-metals`**. Use MCP to:
- Query available definitions, types, and symbols before writing code
- Validate that APIs you're about to use actually exist (prevents hallucinations)
- Check compilation errors before running sbt

**Always verify APIs via MCP before using them.** If MCP reports issues with your code, fix them before running sbt to avoid long feedback loops and context pollution.

## Global Rules

 - **Always use `sbt --client`** — never run bare `sbt` without `--client`. Starting sbt without `--client`
   launches a new JVM every time, which is extremely expensive and kills the feedback loop. `sbt --client`
   connects to a running sbt server instead.

 - **Redirect sbt output to a temporary file** when running long compilation/test cycles — this avoids
   re-running the same expensive command just to inspect a different part of the output:
   ```bash
   sbt --client "fastShowPretty/clean ; test-jvm-2_13" 2>&1 | tee /tmp/sbt-output.txt
   ```
   Then use `grep`, `tail`, `head`, etc. on `/tmp/sbt-output.txt` to inspect results. Only re-run
   sbt if code was actually modified.

 - Only run `sbt compile` or `sbt test` after MCP shows no compilation issues

 - **Do not modify `dev.properties`** — it's primarily used by the developer to focus on one platform
   in their IDE.

 - **Do not add yourself as co-author** — when making commits, agents should not add themselves
   (e.g. `Co-Authored-By: Claude ...`) to commit messages. Only the human author and Happy
   attribution should appear.

### Project matrix structure

This project uses **sbt-projectmatrix** (not sbt-crossproject). Scala version is determined by project name suffix, not by `++` commands.

**Do NOT use:**
- `++2.13.18`, `++3.7.4` or any `++` version switching
- `scalaCrossVersion` settings
- Any cross-building commands from sbt-crossproject

**Project naming convention:**
- No suffix = Scala 2.13 JVM (e.g., `fastShowPretty`)
- `3` suffix = Scala 3 JVM (e.g., `fastShowPretty3`)
- `JS` suffix = Scala 2.13 JS (e.g., `fastShowPrettyJS`)
- `JS3` suffix = Scala 3 JS (e.g., `fastShowPrettyJS3`)
- `Native` suffix = Scala 2.13 Native (e.g., `fastShowPrettyNative`)
- `Native3` suffix = Scala 3 Native (e.g., `fastShowPrettyNative3`)

**Examples:**
```bash
sbt --client "fastShowPretty/test"       # Scala 2.13 JVM
sbt --client "fastShowPretty3/test"      # Scala 3 JVM
sbt --client "fastShowPrettyJS/compile"  # Scala 2.13 JS
sbt --client "fastShowPretty3/compile"   # Scala 3 JVM
```

### Test aliases

| Command | What it does |
|---|---|
| `sbt --client test-jvm-2_13` | Test Scala 2.13 on JVM |
| `sbt --client test-jvm-3` | Test Scala 3 on JVM |
| `sbt --client test-js-3` | Test Scala 3 on Scala.js |
| `sbt --client test-js-2_13` | Test Scala 2.13 on Scala.js |
| `sbt --client test-native-3` | Test Scala 3 on Scala Native |
| `sbt --client test-native-2_13` | Test Scala 2.13 on Scala Native |

## Incremental Compilation Gotchas

 - **Always clean after macro changes** — incremental compilation does NOT re-expand macros when their
   implementation changes. You WILL get stale results if you skip cleaning.

 - **Clean the specific module before testing:**
   ```bash
   sbt --client "fastShowPretty/clean ; fastShowPretty3/clean ; test-jvm-2_13 ; test-jvm-3"
   ```

 - **Nuclear option** (if behavior seems wrong despite clean):
   ```bash
   sbt --client clean
   sbt --client "test-jvm-2_13 ; test-jvm-3"
   ```

## Cross-Quotes and Macro-Agnostic APIs

Code in kindlings uses `Expr`, `Type`, etc. from **Hearth's own API**, NOT `scala.quoted.Expr` or `c.Expr`.
Specifically:

 - `Expr.quote { ... Expr.splice { ... } }` is Hearth's cross-platform alternative to Scala 3 quotes/splices
   and Scala 2 quasiquotes
 - `Type.of[A]`, `Type.Ctor1.of[F]`, etc. are Hearth's cross-platform type representations
 - Implementation traits mix in `MacroCommons` and `StdExtensions` to get the full API
 - The Scala 3 companion uses `inline def` + `${ ... }` with a `MacroCommonsScala3`-based class
 - The Scala 2 companion uses `def ... = macro` with a `MacroCommonsScala2`-based class

## Cross-Compilation Pitfalls

Common issues encountered when writing cross-compiled macros. See also
`docs/contributing/type-class-derivation-skill.md` for detailed patterns and solutions.

### Path-dependent types inside `Expr.quote` (Scala 2)

**Problem:** Using `import param.tpe.Underlying as Field` and then referencing `Field` inside
`Expr.quote { ... }` does NOT work on Scala 2. The generated quasiquote code references the path
variable (e.g., `param`) which doesn't exist at the expansion site.

```scala
// BROKEN on Scala 2 — generates a reference to `param` in the output tree
import param.tpe.Underlying as Field
Expr.quote { someExpr.asInstanceOf[Field] }  // "not found: value param"
```

**Solutions:**
1. **Runtime type witness** — use a runtime utility where a value-level argument provides type
   inference, avoiding explicit path-dependent type references:
   ```scala
   // Runtime utility
   def unsafeCast[A](value: Any, witness: Decoder[A]): A = value.asInstanceOf[A]
   // In macro — decoderExpr carries the type info
   Expr.quote { unsafeCast(arrayExpr(idx), Expr.splice(decoderExpr)) }
   ```
2. **Recursive flatMap chain** — build nested `flatMap` calls where each level uses
   `LambdaBuilder.of1[Field]` to obtain a properly-typed `Expr[Field]`, then wrap it
   via `as_??` into the arguments map for `primaryConstructor`. See the decoder skill for details.
3. **`LambdaBuilder`** — use `LambdaBuilder.of1[Field]("name").traverse { expr => ... }` which
   properly handles the type parameter inside the builder closure.

### Macro-internal types inside `Expr.quote` leak on Scala 2

**Problem:** On Scala 2, expressions inside `Expr.quote` have their types captured by reification —
even inside `Expr.splice` blocks. If an expression involves macro-internal type aliases (like `??`,
`Expr_??`, `UntypedType`), the reified tree includes path-dependent references that fail at the
expansion site.

**Solution:** Extract expressions with macro-internal types to a `val` **before** the `Expr.quote` block:
```scala
// CORRECT — computed outside Expr.quote
val selfType: Option[??] = Some(Type[A].as_??)
Expr.quote { Expr.splice { doSomethingWith(selfType) } }

// BROKEN — type leaks into reified tree
Expr.quote { Expr.splice { doSomethingWith(Some(Type[A].as_??)) } }
```

### `Array` operations require `ClassTag` in macros

**Problem:** `Array.empty[T]` and `+:` inside `Expr.quote` require `ClassTag[T]`, which may not
be available at the expansion site on Scala 2, producing "not found: value ClassTag" errors.

**Solution:** Use `List.empty[T]` and `::` instead — they don't require `ClassTag`.

### `Expr.upcast` only widens

`expr.upcast[B]` requires `A <:< B` (compile-time subtype proof). It cannot narrow types
(e.g., `Any` → `String`). For narrowing, use `.asInstanceOf` inside `Expr.quote` or a runtime
type-witness utility.

### Macro methods require concrete types at call site

**Problem:** A generic helper like `def roundTrip[A](value: A)` that internally calls
`KindlingsEncoder.encode(value)` won't work — the macro sees `A` as abstract, not the concrete type.

**Solution:** Call macro methods directly with concrete types at each call site. Don't wrap them
in generic helpers.

### Hearth source in neighbor directory

The hearth library source is available at `../hearth/` (relative to the kindlings repo root). When
documentation is insufficient, read the actual hearth source code — especially:
- `hearth/src/main/scala/hearth/typed/Classes.scala` — `CaseClass`, `Enum`, `Parameter`
- `hearth/src/main/scala/hearth/typed/Exprs.scala` — `Expr`, `LambdaBuilder`, `Expr_??`
- `hearth/src/main/scala/hearth/typed/Methods.scala` — `Method.NoInstance`, `primaryConstructor`
- `hearth/src/main/scala/hearth/typed/Existentials.scala` — `Existential`, `as_??`
- `hearth/docs/user-guide/cross-quotes.md` — cross-quotes limitations per Scala version

## Hearth documentation

This project depends on the `hearth` library. To find documentation:

1. Check `build.sbt` for the hearth version (look for `versions.hearth`)
2. If the version contains `-SNAPSHOT`, use:
   ```
   https://scala-hearth.readthedocs.io/en/latest/
   ```
3. If the version is a stable release (e.g., `0.2.0`), use:
   ```
   https://scala-hearth.readthedocs.io/en/0.2.0/
   ```

**Always use MCP to verify which hearth definitions are available** before writing code that uses them.

## Skills routing

### Type class derivation

When implementing or modifying type class derivation macros, follow:
**`docs/contributing/type-class-derivation-skill.md`**

Use `FastShowPrettyMacrosImpl.scala` as the reference for **encoder-style** derivation (reading fields).
Use `circe-derivation/DecoderMacrosImpl.scala` as the reference for **decoder-style** derivation
(constructing types from decoded data). This skill covers:
- Defining type classes in shared code
- Cross-compilable macro structure
- Context-based parameter passing
- Rule-based derivation architecture
- Logging, caching, and recursive derivation
- Decoder patterns: `primaryConstructor(fieldMap)`, `LambdaBuilder`, `Expr_??`, recursive flatMap chains

### Implementing a new type class module

When adding a new type class module (like `fast-show-pretty`), follow these steps:

#### 1. Build configuration

Add a new `projectMatrix` in `build.sbt`, add it to the `root` aggregate, and add it to the `al` command generator.

#### 2. File structure (3-layer pattern)

Each type class follows a 3-layer pattern:

```
my-type-class/src/main/
├── scala/hearth/kindlings/mytypeclass/
│   ├── MyTypeClass.scala                          # Public API: trait + companion
│   ├── debug/package.scala                        # LogDerivation import for debugging
│   └── internal/
│       ├── compiletime/
│       │   └── MyTypeClassMacrosImpl.scala         # Core macro logic (shared)
│       └── runtime/
│           └── MyTypeClassUtils.scala              # Runtime helpers (no macros)
├── scala-2/hearth/kindlings/mytypeclass/
│   ├── MyTypeClassCompanionCompat.scala            # Scala 2 companion (macro defs)
│   └── internal/compiletime/
│       └── MyTypeClassMacros.scala                 # Scala 2 macro bridge
└── scala-3/hearth/kindlings/mytypeclass/
    ├── MyTypeClassCompanionCompat.scala            # Scala 3 companion (inline defs)
    └── internal/compiletime/
        └── MyTypeClassMacros.scala                 # Scala 3 macro bridge
```

#### 3. Testing

Tests extend `MacroSuite` (from `hearth-munit`) and use `group()` / nested `test()`:

```scala
import hearth.MacroSuite

final class MyTypeClassSpec extends MacroSuite {
  group("MyTypeClass") {
    group("render") {
      test("some test") { ... }
    }
  }
}
```

Use `compileErrors("...").check(...)` for testing compile-time error messages.

Scala 3-only tests go in `src/test/scala-3/`.

### Debugging macro derivation

To see how a macro derivation unfolds:

1. **Import the debug logger** in the scope of the macro call:
   ```scala
   import hearth.kindlings.fastshowpretty.debug.logDerivationForFastShowPretty
   ```

2. **Or set a scalac option** globally:
   ```
   -Xmacro-settings:fastShowPretty.logDerivation=true
   ```

This will print the derivation log at compile time, showing which rules matched or were skipped.

### Fixing a bug

1. **Write a failing test first** that reproduces the bug
2. **Clean the affected modules** (incremental compilation does not re-expand macros):
   ```bash
   sbt --client "fastShowPretty/clean ; fastShowPretty3/clean"
   ```
3. **Verify the test fails** as expected
4. **Apply the fix** in the appropriate layer (runtime utils, macro impl, or bridge)
5. **Clean again and verify** all tests pass:
   ```bash
   sbt --client "fastShowPretty/clean ; fastShowPretty3/clean ; test-jvm-2_13 ; test-jvm-3"
   ```
6. **Cross-platform verification** (if relevant):
   ```bash
   sbt --client "test-js-3 ; test-native-3"
   ```

### Syncing changes from Hearth

When syncing changes from hearth's `hearth-tests` demo modules back to kindlings:

1. **Package adaptation**: `hearth.demo.allfeatures` -> `hearth.kindlings.<module>`
2. **Scope modifier**: `private[allfeatures]` -> `private[<module>]`
3. **FQN references**: Update fully-qualified names in `Types` object
4. **Test imports**: `hearth.examples.ExampleValueClass` -> define locally in test file
5. **Test base class**: Use `MacroSuite` (from `hearth-munit`)
6. **Source set dirs**: hearth uses `scala-newest` / `scala-newest-2` / `scala-newest-3`; kindlings uses `scala` / `scala-2` / `scala-3`

### Hearth library usage

When working with hearth APIs (MIO, Type, Expr, etc.), follow:
**`docs/contributing/hearth-documentation-skill.md`**

This skill covers how to find and use hearth documentation correctly.
