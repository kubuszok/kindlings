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
expr.upcast[B]                      // Widen type
```

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
caseClass.caseFieldValuesAt(expr)             // Get field name-value pairs

Enum.parse[A]                                 // Option[Enum[A]]
enumm.parMatchOn[F, R](expr)(handler)         // Pattern match on cases
```

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

**Do NOT use** `++2.13.18` or `++3.7.4` to switch versions.

## Troubleshooting

If MCP reports an unknown API:
1. Double-check the documentation URL matches the version
2. Search the hearth source for the actual API name
3. The API may have been renamed or moved between versions
4. Check if an import is missing
