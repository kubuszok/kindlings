# Diff Derivation

Structural comparison of nested types using the Myers diff algorithm -- derives `Diff[A]` instances that produce rich, hierarchical diff results for case classes, sealed traits, collections, maps, sets, options, and strings.

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

    println(result.isIdentical)
    // expected output:
    // false

    println(DiffRenderer.render(result, RenderConfig.plain))
    // expected output:
    // Person(
    //   name = "Alice",
    //   age = 30 -> 31,
    // )
    ```

## API

### Derivation methods

| Method | Returns | Description |
|--------|---------|-------------|
| `Diff.derived[A]` | `Diff[A]` | Sanely-automatic (given/implicit) |

### Type class interface

The `Diff[A]` type class provides:

| Member | Signature | Description |
|--------|-----------|-------------|
| `diff` | `(left: A, right: A): DiffResult` | Compare two values |
| `snapshot` | `(value: A): DiffResult` | Structural snapshot of a single value (used for inserts/deletes in collections) |
| `prettyName` | `String` | ANSI-colored fully qualified name |
| `plainName` | `String` | Fully qualified name (e.g. `scala.Option[java.lang.String]`) |
| `simpleName` | `String` | Short name with type args (e.g. `Option[String]`) |
| `shortName` | `String` | Short name without args (e.g. `Option`) |

### Rendering

| Method | Description |
|--------|-------------|
| `DiffRenderer.render(result)` | Render with default config (simple names, ANSI colors, 2-space indent) |
| `DiffRenderer.render(result, config)` | Render with custom `RenderConfig` |

### Supported types

| Type | Diff behavior |
|------|--------------|
| Case classes | Field-by-field recursive diff |
| Sealed traits / enums | Subtype dispatch |
| Primitives (`Int`, `Boolean`, `Double`, etc.) | Equality check |
| `String` | Hierarchical Myers diff (line -> word -> char) |
| `BigDecimal`, `BigInt` | Equality check |
| `Option[A]` | Some/None handling |
| `List[A]`, `Vector[A]`, `Seq[A]`, etc. | Myers-aligned sequence diff |
| `Map[K, V]` | Key-matched entry diff |
| `Set[A]` | Element-matched diff |
| Value types / opaque types | Unwrap and diff inner type |
| Singletons (`case object`) | Always identical |
| Recursive types | Handled automatically |

## Configuration

Rendering is configured via `RenderConfig`:

```scala
import hearth.kindlings.diffderivation._

val config = RenderConfig(
  nameStyle = NameStyle.FullyQualified,
  colorMode = ColorMode.Plain,
  indent = Indent.Spaces(4)
)
```

Predefined configs:

| Config | Description |
|--------|-------------|
| `RenderConfig.default` | Simple names, ANSI colors, 2-space indent |
| `RenderConfig.plain` | Simple names, plain text with `+`/`-` markers, 2-space indent |

### Name styles

| Style | Example | Description |
|-------|---------|-------------|
| `NameStyle.Simple` | `Option[String]` | Short name with type args (default) |
| `NameStyle.Short` | `Option` | Short name without type args |
| `NameStyle.FullyQualified` | `scala.Option[java.lang.String]` | Fully qualified name |
| `NameStyle.Pretty` | *(ANSI-colored FQN)* | Colored fully qualified name |

### Color modes

| Mode | Description |
|------|-------------|
| `ColorMode.Ansi` | ANSI escape codes: red for removed, green for added, yellow for changed |
| `ColorMode.Plain` | No color codes; uses `+`/`-` markers |

### Indentation

| Style | Description |
|-------|-------------|
| `Indent.Spaces(n)` | `n` spaces per level (default: 2) |
| `Indent.Tab` | One tab character per level |

## Usage examples

??? example "Sealed trait with type mismatch"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}

    import hearth.kindlings.diffderivation._

    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    case class Rectangle(width: Double, height: Double) extends Shape

    implicit val diffShape: Diff[Shape] = Diff.derived[Shape]

    // Same variant -- shows field changes
    val r1 = diffShape.diff(Circle(1.0), Circle(2.0))
    println(DiffRenderer.render(r1, RenderConfig.plain))
    // expected output:
    // Shape.Circle(
    //   Circle(
    //     radius = 1.0 -> 2.0
    //   )
    // )

    // Different variants -- shows type mismatch
    val r2 = diffShape.diff(Circle(1.0), Rectangle(2.0, 3.0))
    println(r2.isIdentical)
    // expected output:
    // false
    ```

??? example "Collection diff with Myers alignment"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}

    import hearth.kindlings.diffderivation._

    implicit val diffInt: Diff[Int] = Diff.derived[Int]
    implicit val diffList: Diff[List[Int]] = Diff.derived[List[Int]]

    // Myers finds the shortest edit script
    val result = diffList.diff(List(1, 2, 3, 4), List(1, 3, 4, 5))
    println(result.isIdentical)
    // expected output:
    // false
    ```

??? example "Recursive data types"

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep com.kubuszok::kindlings-diff-derivation:{{ kindlings_version() }}

    import hearth.kindlings.diffderivation._

    case class TreeNode(value: Int, children: List[TreeNode])

    implicit val diffTree: Diff[TreeNode] = Diff.derived[TreeNode]

    val left = TreeNode(1, List(TreeNode(2, Nil), TreeNode(3, Nil)))
    val right = TreeNode(1, List(TreeNode(2, Nil), TreeNode(4, Nil)))

    val result = diffTree.diff(left, right)
    println(result.isIdentical)
    // expected output:
    // false
    ```

## DiffResult AST

Every `DiffResult` node carries 4 lazy type name strings. The renderer chooses which to display based on `NameStyle`.

| Node | Meaning | `isIdentical` |
|------|---------|----------------|
| `Identical` | Both values are the same | `true` |
| `ValueChanged` | Primitive/opaque values differ | `false` |
| `Record` | Case class -- field-by-field diff | all fields identical |
| `Variant` | Same sealed trait variant | body is identical |
| `TypeMismatch` | Different sealed trait variants | `false` |
| `SeqDiff` | Ordered collection -- Myers-aligned edits | all edits are Equal + identical |
| `MapDiff` | Map -- key-matched entries | all entries identical |
| `SetDiff` | Set -- element-matched entries | all entries identical |
| `OptionalDiff` | Option handling | BothPresent + identical, or BothAbsent |
| `StringDiff` | Hierarchical string diff (line -> word -> char) | all chunks are EqualLine |

### Collection edits (Myers)

`SeqDiff` contains `Vector[Edit[DiffResult]]` where:

- `Edit.Equal(subDiff)` -- element in both sequences, recursively sub-diffed (may have field-level changes)
- `Edit.Insert(snapshot)` -- element only in the right (new) sequence
- `Edit.Delete(snapshot)` -- element only in the left (old) sequence

Unlike difflicious (zip-by-index) and diffx (greedy matching), Myers finds the **shortest edit script** -- the minimum number of insertions and deletions to transform the left sequence into the right.

### String chunks (hierarchical Myers)

`StringDiff` contains `Vector[StringChunk]` with three levels:

**Line level:**

- `EqualLine(text)` -- unchanged line
- `InsertLine(text)` -- line added
- `DeleteLine(text)` -- line removed
- `ChangedLine(words)` -- paired delete+insert drilled down to word level

**Word level** (inside `ChangedLine`):

- `EqualWord(text)` -- unchanged word
- `InsertWord(text)` -- word added
- `DeleteWord(text)` -- word removed
- `ChangedWord(chars)` -- paired words drilled down to character level

**Character level** (inside `ChangedWord`):

- `EqualChar(text)` -- unchanged character(s)
- `InsertChar(text)` -- character(s) added
- `DeleteChar(text)` -- character(s) removed

## Myers diff algorithm

The core algorithm is Eugene Myers' O(ND) "An O(ND) Difference Algorithm and Its Variations" (1986), finding the shortest edit script between two sequences.

### Neil Fraser pre-optimizations

Applied before running Myers to reduce input size:

1. **Equality check** -- skip if both sequences are identical
2. **Common prefix/suffix stripping** -- trim matching head and tail elements
3. **Single edit detection** -- if one side is empty after stripping, the answer is obvious

### Neil Fraser post-cleanups

Applied to Myers output for more human-readable results:

1. **Merge** -- consolidate adjacent same-type edits
2. **Semantic cleanup** -- remove trivially small equalities surrounded by large change blocks (strings)
3. **Semantic lossless** -- slide edits to align with word/line boundaries (strings)
4. **Efficiency cleanup** -- remove small equalities where rendering overhead exceeds savings (strings)

## Debugging

Import the debug package to log the derivation process at compile time:

```scala
import hearth.kindlings.diffderivation.debug._
```

Or enable project-wide via scalac option:

```scala
// build.sbt
scalacOptions += "-Xmacro-settings:diffDerivation.logDerivation=true"
```

### Configurable timeout

Override the default 5-second derivation timeout:

```
scalacOptions += "-Xmacro-settings:diffDerivation.timeout=30s"
```

Supported formats: `30` (seconds), `30s`, `5000ms`, `1m`.

## Comparison with difflicious and diffx

### Feature differences

| Feature | difflicious | diffx | Kindlings |
|---------|-------------|-------|-----------|
| Same API on Scala 2.13 and 3 | Yes | Yes | Yes |
| Platforms | JVM + JS | JVM + JS | JVM + JS + Native |
| Case class derivation | Yes | Yes | Yes |
| Sealed trait / enum | Yes | Yes | Yes |
| String diff (structural) | No (equality only) | Yes (hierarchical Myers) | Yes (hierarchical Myers with Neil Fraser optimizations) |
| Collection diff algorithm | Greedy O(n*m) | Greedy O(n*m) | Myers O(ND) (optimal shortest edit script) |
| Map diff | Yes | Yes | Yes |
| Set diff | Yes | Yes | Yes |
| Option diff | Yes | Yes | Yes |
| Recursive types | Yes | Yes | Yes |
| Path-based configuration | Yes (`ignoreAt`, `replace`) | Yes (`modify(_.field).ignore`) | Not yet |
| Test framework integrations | MUnit, ScalaTest, Weaver | MUnit, ScalaTest, Weaver, Specs2, utest | Not yet |
| Cats collection support | Yes | Yes | Not yet |
| Custom equality (`useEquals`) | Yes | Yes | Not yet |
| Approximate numeric comparison | No | Yes | Not yet |
| Type name rendering | FQN + short (izumi-reflect) | Fixed string | 4 modes (simple/short/FQN/pretty), lazy, runtime-composable |
| Color modes | ANSI (fansi) | ANSI (4 themes) | ANSI + plain (configurable) |
| Configurable indentation | No (fixed 2 spaces) | No (fixed 5 spaces) | Yes (spaces or tab) |
