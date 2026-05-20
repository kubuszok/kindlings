# Diff Derivation

Structural comparison of nested types using the Myers diff algorithm — derives `Diff[A]` instances that produce rich, hierarchical diff results for case classes, sealed traits, collections, maps, sets, options, and strings.

Inspired by [difflicious](https://github.com/jatcwang/difflicious) and [diffx](https://github.com/softwaremill/diffx), but with key improvements:

- **Myers diff for collections** — optimal sequence alignment (shortest edit script), not greedy matching
- **Hierarchical string diff** — line → word → character drill-down with Myers at each level
- **Neil Fraser optimizations** — pre-processing (common prefix/suffix, single-edit fast-path) and post-processing (semantic cleanup, boundary alignment)
- **Lazy type names** — 4 print modes (pretty/plain/simple/short), computed on demand, runtime-composable for generic types
- **Configurable rendering** — name style, color mode, indentation

## Installation

!!! example "sbt"

    ```scala
    libraryDependencies += "com.kubuszok" %% "kindlings-diff-derivation" % "{{ kindlings_version() }}"
    ```

    Cross-platform (JVM / Scala.js / Scala Native):

    ```scala
    libraryDependencies += "com.kubuszok" %%% "kindlings-diff-derivation" % "{{ kindlings_version() }}"
    ```

!!! example "Scala CLI"

    ```scala
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}
    ```

## Quick start

??? example "Comparing two case classes"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}

    import hearth.kindlings.diffderivation._

    case class Person(name: String, age: Int)

    implicit val diffPerson: Diff[Person] = Diff.derived[Person]

    val result = diffPerson.diff(
      Person("Alice", 30),
      Person("Alice", 31)
    )

    println(result.isIdentical) // false
    println(DiffRenderer.render(result, RenderConfig.plain))
    // Person(
    //   name = "Alice",
    //   age = 30 -> 31,
    // )
    ```

??? example "Sealed trait / enum diffing"

    ```scala
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val diffShape: Diff[Shape] = Diff.derived[Shape]

    // Same variant — shows field changes
    val r1 = diffShape.diff(Circle(1.0), Circle(2.0))
    println(DiffRenderer.render(r1, RenderConfig.plain))
    // Shape.Circle(
    //   radius = 1.0 -> 2.0,
    // )

    // Different variants — shows type mismatch
    val r2 = diffShape.diff(Circle(1.0), Rectangle(2.0, 3.0))
    println(DiffRenderer.render(r2, RenderConfig.plain))
    // Shape: Circle -> Rectangle
    //   - ...
    //   + ...
    ```

## Type class

The `Diff[A]` type class provides:

```scala
trait Diff[A] {
  // 4 lazy type name variants — computed on demand
  def prettyName: String   // ANSI-colored fully qualified
  def plainName: String    // fully qualified (e.g. "scala.Option[java.lang.String]")
  def simpleName: String   // short with type args (e.g. "Option[String]")
  def shortName: String    // short without args (e.g. "Option")

  // Compare two values
  def diff(left: A, right: A): DiffResult

  // Structural snapshot of a single value (used for inserts/deletes in collections)
  def snapshot(value: A): DiffResult
}
```

## DiffResult AST

Every `DiffResult` node carries 4 lazy type name strings. The renderer chooses which to display based on `NameStyle`.

| Node | Meaning | `isIdentical` |
|------|---------|----------------|
| `Identical` | Both values are the same | `true` |
| `ValueChanged` | Primitive/opaque values differ | `false` |
| `Record` | Case class — field-by-field diff | all fields identical |
| `Variant` | Same sealed trait variant | body is identical |
| `TypeMismatch` | Different sealed trait variants | `false` |
| `SeqDiff` | Ordered collection — Myers-aligned edits | all edits are Equal + identical |
| `MapDiff` | Map — key-matched entries | all entries identical |
| `SetDiff` | Set — element-matched entries | all entries identical |
| `OptionalDiff` | Option handling | BothPresent + identical, or BothAbsent |
| `StringDiff` | Hierarchical string diff (line→word→char) | all chunks are EqualLine |

### Collection edits (Myers)

`SeqDiff` contains `Vector[Edit[DiffResult]]` where:

- `Edit.Equal(subDiff)` — element in both sequences, recursively sub-diffed (may have field-level changes)
- `Edit.Insert(snapshot)` — element only in the right (new) sequence
- `Edit.Delete(snapshot)` — element only in the left (old) sequence

Unlike difflicious (zip-by-index) and diffx (greedy matching), Myers finds the **shortest edit script** — the minimum number of insertions and deletions to transform the left sequence into the right.

### String chunks (hierarchical Myers)

`StringDiff` contains `Vector[StringChunk]` with three levels:

**Line level:**

- `EqualLine(text)` — unchanged line
- `InsertLine(text)` — line added
- `DeleteLine(text)` — line removed
- `ChangedLine(words)` — paired delete+insert drilled down to word level

**Word level** (inside `ChangedLine`):

- `EqualWord(text)` — unchanged word
- `InsertWord(text)` — word added
- `DeleteWord(text)` — word removed
- `ChangedWord(chars)` — paired words drilled down to character level

**Character level** (inside `ChangedWord`):

- `EqualChar(text)` — unchanged character(s)
- `InsertChar(text)` — character(s) added
- `DeleteChar(text)` — character(s) removed

## Rendering

```scala
val result: DiffResult = diff.diff(left, right)

// Default: simple names, ANSI colors, 2-space indent
println(DiffRenderer.render(result))

// Plain text with +/- markers (no ANSI)
println(DiffRenderer.render(result, RenderConfig.plain))

// Custom config
val config = RenderConfig(
  nameStyle = NameStyle.FullyQualified,
  colorMode = ColorMode.Plain,
  indent = Indent.Spaces(4)
)
println(DiffRenderer.render(result, config))
```

### Name styles

| Style | Example | TypeName method |
|-------|---------|----------------|
| `NameStyle.Simple` | `Option[String]` | `simpleName` |
| `NameStyle.Short` | `Option` | `shortName` |
| `NameStyle.FullyQualified` | `scala.Option[java.lang.String]` | `plainName` |
| `NameStyle.Pretty` | ANSI-colored FQN | `prettyName` |

### Color modes

| Mode | Behavior |
|------|----------|
| `ColorMode.Ansi` | ANSI escape codes: red for removed, green for added, yellow for changed |
| `ColorMode.Plain` | No color codes; uses `+`/`-` markers |

### Indentation

| Style | Behavior |
|-------|----------|
| `Indent.Spaces(n)` | `n` spaces per level (default: 2) |
| `Indent.Tab` | One tab character per level |

## Supported types

### Automatically derived

| Type | Diff behavior |
|------|--------------|
| Case classes | Field-by-field recursive diff → `Record` |
| Sealed traits / enums | Subtype dispatch → `Variant` or `TypeMismatch` |
| Primitives (`Int`, `Boolean`, `Double`, etc.) | Equality check → `Identical` or `ValueChanged` |
| `String` | Hierarchical Myers (line→word→char) → `StringDiff` |
| `BigDecimal`, `BigInt` | Equality check → `Identical` or `ValueChanged` |
| `Option[A]` | Some/None handling → `OptionalDiff` |
| `List[A]`, `Vector[A]`, `Seq[A]`, etc. | Myers alignment → `SeqDiff` |
| `Map[K, V]` | Key matching → `MapDiff` |
| `Set[A]` | Element matching → `SetDiff` |
| Value types / opaque types | Unwrap and diff inner type |
| Singletons (`case object`) | Always identical |
| Recursive types (`Tree`) | Cached def pattern prevents infinite loops |

## Myers diff algorithm

The core algorithm is Eugene Myers' O(ND) "An O(ND) Difference Algorithm and Its Variations" (1986), finding the shortest edit script between two sequences.

### Neil Fraser pre-optimizations

Applied before running Myers to reduce input size:

1. **Equality check** — skip if both sequences are identical
2. **Common prefix/suffix stripping** — trim matching head and tail elements
3. **Single edit detection** — if one side is empty after stripping, the answer is obvious

### Neil Fraser post-cleanups

Applied to Myers output for more human-readable results:

1. **Merge** — consolidate adjacent same-type edits
2. **Semantic cleanup** — remove trivially small equalities surrounded by large change blocks (strings)
3. **Semantic lossless** — slide edits to align with word/line boundaries (strings)
4. **Efficiency cleanup** — remove small equalities where rendering overhead exceeds savings (strings)

## Debugging derivation

Enable derivation logging to see which rules are applied:

```scala
import hearth.kindlings.diffderivation.debug.logDerivationForDiffDerivation

implicit val diffPerson: Diff[Person] = Diff.derived[Person]
```

### Configurable timeout

Override the default 5-second derivation timeout:

```
scalacOptions += "-Xmacro-settings:diffDerivation.timeout=30s"
```

Supported formats: `30` (seconds), `30s`, `5000ms`, `1m`.
