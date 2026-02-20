# AGENTS

This file guides coding agents working in this repository.

## Using MCP for compilation context

This project has Metals MCP available at `.metals/mcp.json` as **`kindlings-metals`**. Use MCP to:
- Query available definitions, types, and symbols before writing code
- Validate that APIs you're about to use actually exist (prevents hallucinations)
- Check compilation errors before running sbt

**Always verify APIs via MCP before using them.** If MCP reports issues with your code, fix them before running sbt to avoid long feedback loops and context pollution.

## sbt usage

- **Always use `sbt --client`** to avoid slow JVM startup on each command
- Only run `sbt compile` or `sbt test` after MCP shows no compilation issues

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

Use `FastShowPrettyMacrosImpl.scala` as the reference implementation. This skill covers:
- Defining type classes in shared code
- Cross-compilable macro structure
- Context-based parameter passing
- Rule-based derivation architecture
- Logging, caching, and recursive derivation

### Hearth library usage

When working with hearth APIs (MIO, Type, Expr, etc.), follow:
**`docs/contributing/hearth-documentation-skill.md`**

This skill covers how to find and use hearth documentation correctly.
