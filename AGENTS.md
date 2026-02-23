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

### Test aliases

| Command | What it does |
|---|---|
| `sbt --client test-jvm-2_13` | Test Scala 2.13 on JVM |
| `sbt --client test-jvm-3` | Test Scala 3 on JVM |
| `sbt --client test-js-3` | Test Scala 3 on Scala.js |
| `sbt --client test-js-2_13` | Test Scala 2.13 on Scala.js |
| `sbt --client test-native-3` | Test Scala 3 on Scala Native |
| `sbt --client test-native-2_13` | Test Scala 2.13 on Scala Native |

**JVM-only by default** — unless debugging a platform-specific failure, run tests only on JVM
platforms (`test-jvm-2_13`, `test-jvm-3`). JS and Native builds are slow and the sbt server
can run out of memory on Native builds. CI covers all platforms.

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

Code uses `Expr`, `Type`, etc. from **Hearth's API**, NOT `scala.quoted.Expr` or `c.Expr`:
- `Expr.quote { ... Expr.splice { ... } }` — cross-platform quotes/splices
- `Type.of[A]`, `Type.Ctor1.of[F]` — cross-platform type representations
- Traits mix in `MacroCommons` and `StdExtensions` for the full API
- Scala 3: `inline def` + `${ ... }` with `MacroCommonsScala3`; Scala 2: `def ... = macro` with `MacroCommonsScala2`

## Cross-Compilation Pitfalls

Common issues when writing cross-compiled macros — one-line summaries below, full details in
`docs/contributing/type-class-derivation-skill.md` § "Cross-compilation pitfalls":

- **Path-dependent types in `Expr.quote`** — fails on Scala 2; use `LambdaBuilder` or runtime type witness
- **Macro-internal types leak** — `??`, `Expr_??` inside `Expr.quote` cause reification failures; extract to `val` before quote
- **`Array` needs `ClassTag`** — use `List` and `::` instead of `Array` in `Expr.quote`
- **`Expr.upcast` only widens** — use `.asInstanceOf` inside `Expr.quote` for narrowing; also needs `Type[A]` in scope for the source type
- **Macro methods need concrete types** — don't wrap macro calls in generic helpers
- **Sibling `Expr.splice` isolation (Scala 3)** — each `Expr.splice` in an `Expr.quote` gets its own `Quotes` context; types/exprs can't be shared between sibling splices. For combined codecs, pre-derive with `LambdaBuilder` in one `runSafe` call
- **`IsMap`/`IsCollection` path-dependent types** — `import isMap.{Key, Value, CtorResult}` before `Expr.quote` to bring `Type` instances into scope
- **`ValDefsCache` wrapping scope** — `vals.toValDefs.use` must wrap the outermost expression containing all references, not individual sub-expressions

Hearth source is at `../hearth/` when documentation is insufficient. See
`docs/contributing/hearth-documentation-skill.md` § "Hearth source as reference" for key files.

## Hearth documentation and API reference

- **`docs/contributing/hearth-documentation-skill.md`** — finding docs, verifying APIs, hearth source reference
- **`docs/contributing/hearth-api-knowledge.md`** — quick-reference table of commonly used hearth API signatures
- Check `build.sbt` for `versions.hearth` — use `https://scala-hearth.readthedocs.io/en/latest/` for SNAPSHOT, `/en/<version>/` for stable

## Skills routing

### Type class derivation

When implementing or modifying type class derivation macros, follow:
**`docs/contributing/type-class-derivation-skill.md`**

Use `FastShowPrettyMacrosImpl.scala` as the reference for **encoder-style** derivation (reading fields).
Use `circe-derivation/DecoderMacrosImpl.scala` as the reference for **decoder-style** derivation
(constructing types from decoded data).
Use `jsoniter-derivation/CodecMacrosImpl.scala` as the reference for **combined codec derivation**
(encoder + decoder in one type class, `LambdaBuilder` pattern for Scala 3 cross-splice safety).

### Other guides in `type-class-derivation-skill.md`

- **"Implementing a new module"** — build config, 3-layer file structure, testing patterns
- **"Debugging derivation"** — debug logger import and scalac option for compile-time logs
- **"Syncing from Hearth"** — adaptation checklist for syncing from hearth's demo modules

### Fixing a bug

1. Write a failing test that reproduces the bug
2. Clean the affected modules (incremental compilation does not re-expand macros)
3. Verify the test fails as expected
4. Apply the fix in the appropriate layer (runtime utils, macro impl, or bridge)
5. Clean again and verify all tests pass
6. Cross-platform verification (if relevant): `sbt --client "test-js-3 ; test-native-3"`

