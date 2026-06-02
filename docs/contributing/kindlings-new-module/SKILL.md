---
name: kindlings-new-module
description: >
  Bootstrapping a new Kindlings derivation module. Build configuration, 3-layer file structure,
  entry points (type class + inlined body), implementation requirements REQ-1 through REQ-12,
  test coverage checklist, error hierarchy, syncing from Hearth.
paths:
  - "build.sbt"
  - "**/compiletime/**Macros*.scala"
  - "**/compiletime/rules/*.scala"
---

# Skill: Implementing a New Kindlings Module

Guide for bootstrapping a new type class derivation module in the Kindlings project.

## Before writing code

1. **Use MCP to verify available APIs** -- query the `kindlings-metals` MCP server
2. **Read the reference implementation** -- study `FastShowPrettyMacrosImpl.scala` and
   the `rules/` trait files
3. **Check hearth documentation** -- see
   [hearth-documentation](../hearth-documentation/SKILL.md) for docs version

## Build configuration

Add a new `projectMatrix` in `build.sbt`, add it to the `root` aggregate, and add it to
the `al` command generator.

## File structure (3-layer pattern)

```
my-type-class/src/main/
+-- scala/hearth/kindlings/mytypeclass/
|   +-- MyTypeClass.scala                          # Public API: trait + companion
|   +-- debug/package.scala                        # LogDerivation import for debugging
|   +-- internal/
|       +-- compiletime/
|       |   +-- MyTypeClassMacrosImpl.scala         # Core macro logic (shared)
|       +-- runtime/
|           +-- MyTypeClassUtils.scala              # Runtime helpers (no macros)
+-- scala-2/hearth/kindlings/mytypeclass/
|   +-- MyTypeClassCompanionCompat.scala            # Scala 2 companion (macro defs)
|   +-- internal/compiletime/
|       +-- MyTypeClassMacros.scala                 # Scala 2 macro bridge
+-- scala-3/hearth/kindlings/mytypeclass/
    +-- MyTypeClassCompanionCompat.scala            # Scala 3 companion (inline defs)
    +-- internal/compiletime/
        +-- MyTypeClassMacros.scala                 # Scala 3 macro bridge
```

## Testing

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

## Workflow summary

1. **Verify APIs with MCP** before writing any code
2. **Create context class** with all needed types, expressions, and cache
3. **Define rules** as objects extending a base rule trait
4. **Log at every decision point** using `Log.info` and `Log.namedScope`
5. **Cache definitions** using `ValDefsCache` to avoid duplication
6. **Derive recursively** using `ctx.nest(...)` for nested types
7. **Ignore the derivation macro** when summoning implicits
8. **For decoder-style derivation**, use `primaryConstructor(fieldMap)` with `Expr_??`
9. **Avoid path-dependent types in `Expr.quote`** -- use `LambdaBuilder` or runtime type witnesses
10. **Validate against requirements** (REQ-1 through REQ-12) -- see [requirements.md](requirements.md)
11. **Test in your module** after MCP confirms compilation

## Syncing from Hearth

When syncing changes from hearth's `hearth-tests` demo modules back to kindlings:

1. **Package adaptation**: `hearth.demo.allfeatures` -> `hearth.kindlings.<module>`
2. **Scope modifier**: `private[allfeatures]` -> `private[<module>]`
3. **FQN references**: Update fully-qualified names in `Types` object
4. **Test imports**: `hearth.examples.ExampleValueClass` -> define locally in test file
5. **Test base class**: Use `MacroSuite` (from `hearth-munit`)
6. **Source set dirs**: hearth uses `scala-newest` / `scala-newest-2` / `scala-newest-3`;
   kindlings uses `scala` / `scala-2` / `scala-3`

## Implementation requirements and test coverage

See [requirements.md](requirements.md) for the full REQ-1 through REQ-12 checklist, test
coverage tables, test file organization, and error type hierarchy.

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture
- [hearth-case-class-rules](../hearth-case-class-rules/SKILL.md) -- case class derivation
- [hearth-enum-rules](../hearth-enum-rules/SKILL.md) -- enum derivation
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- cross-compilation pitfalls
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching pattern
- [kindlings-debugging](../kindlings-debugging/SKILL.md) -- debugging derivation
- [kindlings-factory-instance](../kindlings-factory-instance/SKILL.md) -- factory instance pattern
