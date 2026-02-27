# Skill: Hearth Documentation

Use this skill when working with hearth library APIs (MIO, Type, Expr, ValDefsCache, etc.).

## Finding the correct documentation version

1. **Check the hearth version in `build.sbt`:**
   ```scala
   val versions = new {
     // ...
     val hearth = "0.2.0-142-g1a0ebb2-SNAPSHOT"  // example
   }
   ```

2. **Determine the documentation URL:**
   - If version contains `-SNAPSHOT` (like `0.2.0-142-g1a0ebb2-SNAPSHOT`):
     ```
     https://scala-hearth.readthedocs.io/en/latest/
     ```
   - If version is stable (like `0.2.0`):
     ```
     https://scala-hearth.readthedocs.io/en/0.2.0/
     ```

## Preventing hallucinations

**Always use the `kindlings-metals` MCP server (at `.metals/mcp.json`) to verify APIs before using them.**

Before writing code that uses hearth:
1. Query MCP to check what methods/types are available
2. Verify method signatures and return types
3. Confirm import paths

Example workflow:
```
# First, ask kindlings-metals MCP about available methods on Type
# Then verify the exact signature of Type.of[A]
# Only then write code using it
```

## Common hearth APIs used in this project

> **Quick reference:** See [`hearth-api-knowledge.md`](hearth-api-knowledge.md) for a comprehensive table of all hearth API signatures used in kindlings.

These are the main APIs used for type class derivation. **Always verify with MCP that they exist and have the expected signatures.**

### MIO (Macro IO effect)

```scala
import hearth.fp.effect.*

// Pure value
MIO.pure(value)

// Failed computation
MIO.fail(error)

// Lift a thunk
MIO { /* code */ }

// Sequencing
mio1 >> mio2
mio1.flatMap(a => mio2)

// Scoped execution (for converting MIO to Expr)
MIO.scoped { runSafe =>
  // runSafe: MIO[A] => A
}
```

### Log

```scala
import hearth.fp.effect.*

Log.info(s"message")
Log.debug(s"message")
Log.namedScope("scope name") { mio }
```

### Type

```scala
import hearth.std.*

Type.of[A]                          // Get Type[A]
Type[A].prettyPrint                 // String representation
Type[A] <:< Type[B]                 // Subtype check
Type[A].summonExprIgnoring(syms*)   // Summon implicit, ignoring specific symbols
Type.Ctor1.of[F]                    // Higher-kinded type constructor
```

### Expr

```scala
import hearth.std.*

Expr(value)                         // Lift literal
Expr.quote { /* code */ }           // Quote block
Expr.splice(expr)                   // Splice expression inside quote
expr.prettyPrint                    // String representation
expr.upcast[B]                      // Widen type (A <:< B required)
expr.as_??                          // Wrap into Expr_?? (existential)
```

### Expr_?? (existential expressions)

`Expr_??` is `Existential[Expr]` — wraps an `Expr[A]` with its `Type[A]` proof, erasing the concrete type. Use when storing heterogeneously-typed expressions in collections.

```scala
import hearth.std.*

// Wrapping
val existential: Expr_?? = someExpr.as_??

// Consuming
import existential.{Underlying as FieldType, value as expr}
// Now in scope: implicit Type[FieldType] and expr: Expr[FieldType]
```

**Important:** `Expr.upcast[B]` only widens (`A <:< B`). It cannot narrow (e.g., `Any` → `String`). For narrowing, use `.asInstanceOf` inside `Expr.quote` or a runtime type-witness utility.

### LambdaBuilder

Creates runtime lambda expressions from compile-time derivation. Use inside macro rules when you need to construct typed closures.

```scala
import hearth.std.*

// Build a lambda: InputType => OutputType
LambdaBuilder
  .of1[InputType]("argName")
  .traverse { (inputExpr: Expr[InputType]) =>
    deriveBody(inputExpr): MIO[Expr[OutputType]]
  }
  .map(_.build[OutputType])        // Expr[InputType => OutputType]

// Simple (non-MIO) version
LambdaBuilder
  .of1[InputType]("argName")
  .buildWith { (inputExpr: Expr[InputType]) =>
    makeBody(inputExpr): Expr[OutputType]
  }                                // Expr[InputType => OutputType]
```

**Caveat:** Always use `Expr.quote`/`Expr.splice` inside builder closures. Raw `'{ }` / `${ }` on Scala 3 captures the wrong `Quotes` context and causes `ScopeException`.

### ValDefsCache

```scala
import hearth.std.*

ValDefsCache.mlocal                           // Create MLocal[ValDefsCache]
cache.get0Ary[T](key)                         // Get cached value
cache.get1Ary[A, B](key)                      // Get cached function
cache.buildCachedWith(key, builder)(body)     // Build and cache

// Builders
ValDefBuilder.ofLazy[T](name)                 // lazy val
ValDefBuilder.ofDef1[A, B](name, argName)     // def with 1 arg
```

### CaseClass and Enum

```scala
import hearth.std.*

CaseClass.parse[A]                            // Option[CaseClass[A]]
caseClass.caseFieldValuesAt(expr)             // Get field name-value pairs (ListMap[String, Expr_??])
caseClass.primaryConstructor                  // Method.NoInstance[A] — the primary constructor
caseClass.primaryConstructor(fieldMap)        // Either[String, Expr[A]] — construct from Map[String, Expr_??]
caseClass.construct[F](makeArgument)          // F[Option[Expr[A]]] — construct via ConstructField callback

Enum.parse[A]                                 // Option[Enum[A]]
enumm.parMatchOn[F, R](expr)(handler)         // Pattern match on cases
```

**Choosing a construction method:**
- **`caseFieldValuesAt`** — for encoder-style derivation (reading fields from an existing value)
- **`primaryConstructor(fieldMap)`** — for decoder-style derivation (constructing from decoded data). Takes `Map[String, Expr_??]`, returns `Either[String, Expr[A]]`. Avoids path-dependent type issues.
- **`construct`** — uses `ConstructField` with dependent return type `Expr[field.tpe.Underlying]`. This has Scala 2 macro hygiene issues with path-dependent types. Prefer `primaryConstructor(fieldMap)` for cross-compiled decoder code.

### Rules

```scala
import hearth.std.*

abstract class Rule { def name: String }
Rule.matched(value)                           // Rule applies with result
Rule.yielded(reason)                          // Rule doesn't apply

Rules(rule1, rule2, ...)(f)                   // Try rules in order
```

### Type extractors

```scala
import hearth.std.*

Type[A] match {
  case IsCollection(isCollection) =>
    import isCollection.Underlying as Item
    isCollection.value.asIterable(expr)
  case IsMap(isMap) =>
    import isMap.value.{Key, Value}
    isMap.value.asIterable(expr)
}
```

## Workflow

1. **Check `build.sbt`** for hearth version
2. **Consult documentation** at the correct URL
3. **Verify with `kindlings-metals` MCP** that APIs exist before using them
4. **Only run sbt** after MCP shows clean compilation

## sbt project matrix

This project uses **sbt-projectmatrix**. Scala version is determined by project suffix:
- `yourModule` = Scala 2.13
- `yourModule3` = Scala 3

**Do NOT use** `++2.13.18` or `++3.8.2` to switch versions.

## Cross-quotes pitfalls

These are common issues when writing cross-compiled macros with `Expr.quote`:

| Issue | Symptom | Fix |
|---|---|---|
| Path-dependent types in `Expr.quote` | "not found: value param" on Scala 2 | Use `LambdaBuilder` or runtime type witness; avoid `Field` alias inside `Expr.quote` |
| `Array.empty[T]` / `+:` in quotes | "not found: value ClassTag" on Scala 2 | Use `List.empty[T]` and `::` instead |
| `expr.upcast[B]` for narrowing | Compile error: `A <:< B` not satisfied | Use `.asInstanceOf[B]` inside `Expr.quote` or a runtime utility |
| Raw `'{ }` inside `LambdaBuilder` | `ScopeException` on Scala 3 | Use `Expr.quote`/`Expr.splice` or `withQuotes { }` |
| Macro wrapper with generic `[A]` | "type A was not handled by any rule" | Call macro methods with concrete types at each call site |
| Macro-internal types (`??`, `Expr_??`) in `Expr.quote` | Reification captures path-dependent refs on Scala 2 | Extract to `val` before `Expr.quote` block |

For detailed patterns and solutions, see `type-class-derivation-skill.md`.

## Hearth source as reference

The hearth library source is available at `../hearth/` (relative to the kindlings repo root). Key files:
- `hearth/src/main/scala/hearth/typed/Classes.scala` — `CaseClass`, `Enum`, `Parameter`, `construct`
- `hearth/src/main/scala/hearth/typed/Exprs.scala` — `Expr`, `LambdaBuilder`, `Expr_??`
- `hearth/src/main/scala/hearth/typed/Methods.scala` — `Method.NoInstance`, `primaryConstructor`
- `hearth/src/main/scala/hearth/typed/Existentials.scala` — `Existential`, `as_??`
- `hearth/docs/user-guide/cross-quotes.md` — cross-quotes limitations per Scala version

## Troubleshooting

If MCP reports an unknown API:
1. Double-check the documentation URL matches the version
2. Search the hearth source for the actual API name
3. The API may have been renamed or moved between versions
4. Check if an import is missing
