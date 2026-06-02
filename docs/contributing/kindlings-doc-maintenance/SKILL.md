---
name: kindlings-doc-maintenance
description: >
  Writing and testing executable code snippets in the Kindlings user guide. Expected output
  verification syntax, running snippet tests (just test-snippets), template variables.
paths:
  - "docs/user-guide/**/*.md"
user-invocable: false
---

# Documentation Maintenance

How to write, test, and maintain executable code snippets in the Kindlings user guide.

## Snippet testing overview

Documentation code snippets in `docs/user-guide/*.md` are tested as part of CI using [scala-cli-md-spec](https://github.com/kubuszok/scala-cli-md-spec). The test runner at `scripts/test-snippets.scala` wraps the library with Kindlings-specific configuration (template interpolation, default Scala version, SNAPSHOT repo).

A code block is recognized as a runnable snippet when it contains `//> using` directives. Code blocks without them are treated as pseudocode and skipped.

## Output verification

Use `// expected output:` to make the spec verify that a snippet's `println` output matches expectations:

```scala
println(json.noSpaces)
// expected output:
// {"name":"Alice","age":30}
```

Multi-line output:

```scala
println(FastShowPretty.render(person, RenderConfig.Default))
// expected output:
// Person(
//   name = "Alice",
//   age = 30
// )
```

Multiple `println` calls in a single snippet each get their own `// expected output:` block. The blocks concatenate in order and are matched against the full stdout.

Related directives (same syntax):
- `// expected error:` -- runtime errors (matched against stderr)
- `// expected compile error:` -- compilation failures

**Important**: if a snippet has multiple `println` calls and you add `// expected output:` to one, you must add it to ALL of them (or none). The spec matches the concatenated expected output against the entire stdout.

## Printing Scala objects with FastShowPretty

When a snippet prints a Scala case class, use `FastShowPretty.render` instead of relying on `toString` or adding `pprint` as a dependency. This showcases Kindlings' own module and produces readable, field-named output:

```scala
//> using dep com.kubuszok::kindlings-fast-show-pretty:{{ kindlings_version() }}

import hearth.kindlings.fastshowpretty._

val person = readFromArray[Person](bytes)
println(FastShowPretty.render(person, RenderConfig.Default))
// expected output:
// Person(
//   name = "Bob",
//   age = 25
// )
```

Use FastShowPretty for:
- Direct case class values (jsoniter `readFromArray`, avro `decode`, cats `|+|` results)
- Any case class printed via `toString` that would benefit from field names and type suffixes

Keep `toString` for:
- JSON/XML/YAML string output (already formatted)
- `Right(...)` / `Left(...)` wrappers (the wrapper is informative and the inner error type may not be derivable)
- Boolean, Int, and other primitive results

Sealed trait values render with type annotation: `(Circle(\n    radius = 5.0d\n  )): Shape`

## Template variables

Snippets use Jinja2-style template variables interpolated from `docs/mkdocs.yml` extra section:

| Variable | Example value | Description |
|----------|--------------|-------------|
| `{{ kindlings_version() }}` | `0.1.2` | Current Kindlings version |
| `{{ scala.2_13 }}` | `2.13.18` | Scala 2.13 version |
| `{{ scala.3 }}` | `3.8.2` | Scala 3 version |
| `{{ libraries.circe }}` | `0.14.15` | Circe version |
| `{{ libraries.jsoniterScala }}` | `2.38.12` | Jsoniter Scala version |
| `{{ libraries.cats }}` | `2.13.0` | Cats version |

Full list is in `docs/mkdocs.yml` under `extra:`.

## Running tests locally

From the project root:

```bash
# Publish all modules locally (required before running snippets)
sbt --client "publish-local-for-tests"

# Run all snippet tests
scala-cli run scripts/test-snippets.scala -- \
  --extra "kindlings-version=$(sbt -batch -error 'print fastShowPretty/version' 2>/dev/null | grep -oE '^[0-9][^ \[]*')" \
  "$PWD/docs/user-guide"

# Run tests for a specific file
scala-cli run scripts/test-snippets.scala -- \
  --extra "kindlings-version=0.1.2-40-g35f482d-SNAPSHOT" \
  --test-only "fast-show-pretty.md*" \
  "$PWD/docs/user-guide"
```

Or using the Justfile (from `docs/`):

```bash
cd docs && just test-snippets
```

## Ignored snippets

Some snippets are in the ignore list in `scripts/test-snippets.scala`:

- **Dep-only snippets**: Installation sections showing only dependency lines, no runnable code
- **Runtime edge cases**: Snippets that compile but fail at runtime in script mode (e.g., recursive Arbitrary stack overflow, initialization order issues)

To add a snippet to the ignore list, use its stable name format: `"filename.md#Section title[N]"` where N is the 1-based index of the snippet within that section.

## When to skip expected output

Do not add `// expected output:` when:

- Output is **non-deterministic** (ScalaCheck random values)
- Output includes **error messages** whose format may change between library versions (`Left(DecodingFailure(...))`)
- Output is from **complex internal objects** (Tapir schema types, Avro schema JSON with non-deterministic field ordering)
- Output includes **HOCON render** or similar format with version-dependent comments
- The snippet is **pseudocode** (no `//> using` directives) -- the spec won't test it anyway

## Adding a new documentation page

1. Create `docs/user-guide/your-module.md` following existing module structure
2. Add `//> using scala {{ scala.2_13 }}` and `//> using dep` directives to make snippets testable
3. Add `// expected output:` blocks for all deterministic `println` calls
4. Use `FastShowPretty.render` for case class output
5. Run `just test-snippets` from `docs/` to verify
6. If any snippet must be skipped, add its stable name to the ignore list in `scripts/test-snippets.scala`

## Admonition syntax

Snippets use MkDocs Material admonition blocks:

```markdown
??? example "Title"        <!-- collapsible (closed by default) -->

    ```scala
    //> using scala {{ scala.2_13 }}
    // code here
    ```

!!! example "Title"        <!-- non-collapsible (always open) -->

    ```scala
    // code here
    ```
```

Note: the 4-space indent inside admonitions is required.

## Related skills

- [`../hearth-documentation/`](../hearth-documentation/SKILL.md) — Hearth API documentation and verification workflow
