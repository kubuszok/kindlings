# Hearth Bug: `Expr.summonImplicit` fails in Scala-3-only source files

**Hearth version**: 0.3.0-94-gedb4f45-SNAPSHOT (0.4.0 branch)
**Affects**: Scala 3 only
**Discovered**: 2026-06-05

## Symptom

`Expr.summonImplicit[PreferSchemaConfig[X]]` finds the implicit in shared source files
(`src/test/scala/`) but NOT in Scala-3-only files (`src/test/scala-3/`), even when
the implicit is defined identically in both locations.

## Reproducer

### Working: shared file (`src/test/scala/...KindlingsSchemaSpec.scala`)

```scala
package hearth.kindlings.tapirschemaderivation

final class KindlingsSchemaSpec extends MacroSuite {
  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  test("derives schema") {
    val schema = KindlingsSchema.derived[SimplePerson].schema  // ✓ compiles
  }
}
```

### Failing: Scala-3-only file (`src/test/scala-3/...UnionTypeSchemaSpec.scala`)

```scala
package hearth.kindlings.tapirschemaderivation

final class UnionTypeSchemaSpec extends MacroSuite {
  implicit val config: Configuration = Configuration.default
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  test("derives schema for union type") {
    val schema = KindlingsSchema.derived[String | Int].schema  // ✗ fails
    // Error: Multiple JSON library configurations found: circe, jsoniter-scala.
  }
}
```

Both files:
- Same package (`hearth.kindlings.tapirschemaderivation`)
- Same implicit definitions (class-level `implicit val`)
- Same macro call (`KindlingsSchema.derived`)

The ONLY difference is the source directory: `scala/` (works) vs `scala-3/` (fails).

## Analysis

The error comes from `SchemaMacrosImpl.resolveJsonConfig`:

```scala
private def resolveJsonConfig(macroName: String): JsonSchemaConfig =
  JsonSchemaConfig.all match {
    case multiple =>
      val preferred = multiple.filter { provider =>
        implicit val cfgType: Type[ConfigTypeWitness] =
          UntypedType.toTyped[ConfigTypeWitness](provider.configType)
        implicit val pscType: Type[PreferSchemaConfig[ConfigTypeWitness]] = ctor[ConfigTypeWitness]
        Expr.summonImplicit[PreferSchemaConfig[ConfigTypeWitness]].isDefined  // ← fails here
      }
  }
```

The `Expr.summonImplicit` searches the call site's implicit scope. For `inline given`,
Scala 3 should expand at the use site and see its implicits. But the phantom type
witness pattern (`ConfigTypeWitness`) might interact differently with Scala 3's implicit
search depending on compilation unit ordering or source directory.

### Not a union type issue

The same failure happens for non-union types (`case class SimpleTest(x: Int)`) when
derived from the `scala-3/` directory. Union types just happen to require this directory.

### Not an implicit ambiguity issue

Removing the shared spec file and only keeping the `scala-3/` file still fails.

## Possible causes

1. **Compilation ordering**: `scala-3/` files might be compiled in a separate batch where
   `Expr.summonImplicit` uses a different implicit scope snapshot.

2. **Phantom type witness resolution**: The `ConfigTypeWitness` pattern constructs a
   `Type[PreferSchemaConfig[ConfigTypeWitness]]` at macro time using `UntypedType.toTyped`.
   On Scala 3, the constructed type might not match the user's `PreferSchemaConfig[Configuration]`
   if the type representation differs between compilation units.

3. **`inline given` expansion context**: The implicit search context for `inline given`
   might be restricted when the expansion originates from a `scala-3/` source directory
   (different compilation phase).

## Impact

Blocks testing Scala-3-only features (union types, named tuples with Scala 3 syntax)
in the tapir-schema-derivation module. The derivation infrastructure works — only the
test setup is affected.

## Workaround

Tests that use `KindlingsSchema.derived` can only be placed in shared `src/test/scala/`
files. Scala-3-only syntax (union types) cannot be tested until this is resolved.
