# Skill: Type Class Derivation

Use this skill when implementing or modifying type class derivation macros in this repository.

**Reference implementation:** `fast-show-pretty/src/main/scala/hearth/kindlings/fastshowpretty/internal/compiletime/FastShowPrettyMacrosImpl.scala`

This module demonstrates how to derive type classes using hearth. When creating derivations for different type classes in other modules, follow the same patterns and conventions shown here.

## Before writing code

1. **Use MCP to verify available APIs** - Query the `kindlings-metals` MCP server (at `.metals/mcp.json`) to confirm that types, methods, and imports you plan to use actually exist in hearth
2. **Read the reference implementation** - Study `FastShowPrettyMacrosImpl.scala` to understand the patterns
3. **Check hearth documentation** - See `hearth-documentation-skill.md` for how to find the right docs version

## Implementing a new module

When adding a new type class module (like `fast-show-pretty`), follow these steps:

### 1. Build configuration

Add a new `projectMatrix` in `build.sbt`, add it to the `root` aggregate, and add it to the `al` command generator.

### 2. File structure (3-layer pattern)

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

### 3. Testing

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

## Debugging derivation

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

## Core architecture

### Context-based parameter passing

Instead of passing many parameters through method signatures, bundle them into a context case class. This makes the code easier to modify and extend.

**From FastShowPrettyMacrosImpl.scala (lines 94-131):**

```scala
final case class DerivationCtx[A](
    tpe: Type[A],
    sb: Expr[StringBuilder],
    value: Expr[A],
    config: Expr[RenderConfig],
    level: Expr[Int],
    cache: MLocal[ValDefsCache],
    derivedType: Option[??]    // None for inline, Some for derived (see "Self-type skip" below)
) {

  def nest[B: Type](newValue: Expr[B]): DerivationCtx[B] = DerivationCtx(
    tpe = Type[B],
    sb = sb,
    value = newValue,
    cache = cache,
    config = config,
    level = level
  )

  def incrementLevel: DerivationCtx[A] = copy(
    level = Expr.quote(Expr.splice(level) + 1)
  )
}

object DerivationCtx {

  def from[A: Type](
      sb: Expr[StringBuilder],
      value: Expr[A],
      config: Expr[RenderConfig],
      level: Expr[Int]
  ): DerivationCtx[A] = DerivationCtx(
    tpe = Type[A],
    sb = sb,
    value = value,
    cache = ValDefsCache.mlocal,
    config = config,
    level = level
  )
}
```

**Key points:**
- Include the current type being derived (`tpe`)
- Include all expressions needed for code generation
- Include `MLocal[ValDefsCache]` for caching (see below)
- Provide `nest` method to derive nested types with new values
- Provide factory method to create initial context

### Rule-based derivation architecture

Define derivation as a sequence of rules. Each rule checks if it applies and returns either a matched result or yields with a reason.

**From FastShowPrettyMacrosImpl.scala (lines 137-140, 164-190):**

```scala
abstract class DerivationRule(val name: String) extends Rule {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]]
}

def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
  Log.namedScope(s"Deriving for type ${Type[A].prettyPrint}") {
    Rules(
      UseCachedDefWhenAvailableRule,
      UseImplicitWhenAvailableRule,
      UseBuiltInSupportRule,
      HandleAsMapRule,
      HandleAsCollectionRule,
      HandleAsCaseClassRule,
      HandleAsEnumRule
    )(_[A]).flatMap {
      case Right(result) =>
        Log.info(s"Derived result for ${Type[A].prettyPrint}: ${result.prettyPrint}") >>
          MIO.pure(result)
      case Left(reasons) =>
        val reasonsStrings = reasons.toListMap
          .removed(UseCachedDefWhenAvailableRule)
          .view
          .map { case (rule, reasons) =>
            if (reasons.isEmpty) s"The rule ${rule.name} was not applicable"
            else s" - The rule ${rule.name} was not applicable, for the following reasons: ${reasons.mkString(", ")}"
          }
          .toList
        Log.info(s"Failed to derive result for ${Type[A].prettyPrint}:\n${reasonsStrings.mkString("\n")}") >>
          MIO.fail(DerivationError.UnsupportedType(Type[A].prettyPrint, reasonsStrings))
    }
  }
```

**Key points:**
- Rules are tried in order; first match wins
- Use `Rule.matched(...)` when the rule applies
- Use `Rule.yielded(...)` when the rule doesn't apply (with reason)
- The `Rules(...)` combinator from hearth handles the orchestration

### Logging with MIO and Log

Log at every decision point using `Log` from `hearth.fp.effect`.

**From FastShowPrettyMacrosImpl.scala (lines 198, 227, 259, 294, 379, 414, 461):**

```scala
object UseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
      // ... rule logic
      ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
        case Some(instance) =>
          Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >>
            MIO.pure(Rule.matched(...))

        case None =>
          // ... try other cache entries
      }
    }
}

object UseBuiltInSupportRule extends DerivationRule("use built-in support") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
      // ... check types
    }
}
```

**Key points:**
- Use `Log.info(...)` before attempting each rule
- Use `Log.info(...)` when a rule matches or fails
- Use `Log.namedScope(...)` to create hierarchical log structure
- Logs help debug macro expansion issues

### Caching with ValDefsCache and MLocal

Cache generated definitions to avoid code duplication and enable recursive types.

**From FastShowPrettyMacrosImpl.scala (lines 194-217, 421-430):**

```scala
object UseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {
  implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      // Try to get a cached 0-ary instance
      ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
        case Some(instance) =>
          Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >>
            MIO.pure(Rule.matched(Expr.quote {
              Expr.splice(instance).render(Expr.splice(ctx.sb))(Expr.splice(ctx.value))
            }))

        case None =>
          // Try to get a cached 1-ary helper def
          ctx.cache.get1Ary[A, StringBuilder]("helper").flatMap {
            case Some(helperCall) =>
              Log.info(s"Found cached helper call for ${Type[A].prettyPrint}, using it") >>
                MIO.pure(Rule.matched(helperCall(ctx.value)))
            case None =>
              MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} does not have a cached definition"))
          }
      }
    }
}

// Building a cached helper def for collections (lines 421-430):
MIO.scoped { runSafe =>
  ctx.cache.buildCachedWith(
    helperCacheKey,
    ValDefBuilder.ofDef1[Item, StringBuilder]("renderItem", "item")
  ) { case (_, itemExpr) =>
    // Use runSafe to convert MIO[Expr[StringBuilder]] to Expr[StringBuilder]
    runSafe(deriveResultRecursively[Item](using ctx.nest(itemExpr)))
  }
}
```

**Key points:**
- Use `ValDefsCache.mlocal` in context creation
- Use `ctx.cache.get0Ary[T](key)` to retrieve cached values
- Use `ctx.cache.get1Ary[A, B](key)` to retrieve cached functions
- Use `ctx.cache.buildCachedWith(key, builder)(body)` to create and cache definitions
- Use `MIO.scoped { runSafe => ... }` when you need to convert `MIO[Expr[...]]` to `Expr[...]` inside a builder

### Recursive derivation

Call `deriveResultRecursively` for nested types, updating the context appropriately.

**From FastShowPrettyMacrosImpl.scala (lines 299-310, 386-395):**

```scala
// For case class fields (lines 299-310):
fieldValues
  .parTraverse { case (fieldName, fieldValue) =>
    import fieldValue.{Underlying as Field, value as fieldExpr}
    Log.namedScope(s"Deriving the value ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
      // Use incrementLevel so nested case classes are indented properly
      deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
        (fieldName, fieldResult)
      }
    }
  }

// For enum cases (lines 386-395):
enumm
  .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
    import matched.{value as enumCaseValue, Underlying as EnumCase}
    Log.namedScope(s"Deriving the value ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
      // Use incrementLevel so nested case classes in enum cases are indented properly
      deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue)).map { enumCaseResult =>
        Expr.quote {
          Expr.splice(ctx.sb).append("(")
          Expr.splice(enumCaseResult).append("): ").append(Expr.splice(name))
        }
      }
    }
  }
```

**Key points:**
- Use `ctx.nest(newExpr)` to create context for nested type
- Use `ctx.incrementLevel` when nesting increases indentation
- Wrap in `Log.namedScope(...)` for hierarchical logging
- Use `.parTraverse` for parallel derivation of independent items

### Ignoring implicits to prevent self-summoning

When deriving, prevent the macro from summoning itself (which would cause infinite recursion).

**From FastShowPrettyMacrosImpl.scala (lines 220-243):**

```scala
object UseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {

  // Collect method symbols to ignore - the `derived` method from the companion object
  lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
    case method if method.value.name == "derived" => method.value.asUntyped
  }

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use implicit support for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      // Use summonExprIgnoring to skip the derivation macro itself
      Type[FastShowPretty[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
        case Right(instanceExpr) =>
          Log.info(s"Found implicit ${instanceExpr.prettyPrint}, caching it and using a cached value")
          ctx.cache.buildCachedWith("instance", ValDefBuilder.ofLazy[FastShowPretty[A]]("instance"))(_ =>
            instanceExpr
          ) >> UseCachedDefWhenAvailableRule[A]
        case Left(reason) =>
          MIO.pure(
            Rule.yielded(
              s"The type ${Type[A].prettyPrint} is does not have an implicit FastShowPretty instance: $reason"
            )
          )
      }
    }
}
```

**Key points:**
- Get method symbols from the companion object that should be ignored
- Use `Type[TC[A]].summonExprIgnoring(symbols*)` to summon while skipping specific methods
- This prevents `derived` from summoning itself, allowing the rule-based logic to handle derivation instead

### Self-type skip in `derived` entrypoint (Chimney-style pattern)

When the user writes `implicit val tc: MyTypeClass[X] = MyTypeClass.derived[X]`, the macro expansion of `derived[X]` might find `tc` itself during implicit search (via `UseImplicitWhenAvailableRule`), generating code that calls itself infinitely.

The `ignoredImplicits` mechanism (REQ-3b above) ignores the `derived` **method** itself, but cannot prevent finding user-defined `implicit val`s that happen to be the value being defined.

**Solution:** Track the type being derived in the context and skip implicit search when the current type matches the derived type.

```scala
final case class DerivationCtx[A](
    tpe: Type[A],
    // ... other fields ...
    cache: MLocal[ValDefsCache],
    derivedType: Option[??]   // None for inline, Some for derived
)
```

In the **`derived`** entrypoint, set `derivedType = Some(Type[A].as_??)`:

```scala
def deriveTypeClass[A: Type]: Expr[MyTypeClass[A]] = {
  val selfType: Option[??] = Some(Type[A].as_??)
  // ... use selfType when creating the context
  fromCtx(DerivationCtx.from(..., derivedType = selfType))
}
```

In the **inline** entrypoint (e.g., `encode`, `decode`, `render`), set `derivedType = None` — we're not creating an implicit, so existing user-provided implicits should be found normally.

In the implicit rule, check before searching:

```scala
object UseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {
  def apply[A: DerivationCtx]: MIO[Rule.Applicability[...]] =
    Log.info(s"Attempting to use implicit for ${Type[A].prettyPrint}") >> {
      // Skip self type to prevent infinite recursion
      if (ctx.derivedType.exists(_.Underlying =:= Type[A]))
        MIO.pure(Rule.yielded(s"Skipping implicit search for self type ${Type[A].prettyPrint}"))
      else
        Type[TC[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
          case Right(instanceExpr) => ...
          case Left(reason)        => ...
        }
    }
}
```

**Scala 2 cross-compilation pitfall:** The expression `Some(Type[A].as_??)` must be extracted to a local `val` **before** the `Expr.quote` block. On Scala 2, expressions inside `Expr.quote` (even inside `Expr.splice`) have their types captured by reification. If `Option[??]` appears inside the quote body, the reified tree includes path-dependent type references (e.g., `FastShowPrettyMacrosImpl.??`) that cannot be resolved at the expansion site.

```scala
// CORRECT — selfType computed outside Expr.quote:
val selfType: Option[??] = Some(Type[A].as_??)
Expr.quote {
  new MyTypeClass[A] {
    def apply(...) = {
      Expr.splice {
        fromCtx(DerivationCtx.from(..., derivedType = selfType))
      }
    }
  }
}

// BROKEN on Scala 2 — Some(Type[A].as_??) inside quote body:
Expr.quote {
  new MyTypeClass[A] {
    def apply(...) = {
      Expr.splice {
        fromCtx(DerivationCtx.from(..., derivedType = Some(Type[A].as_??)))
      }
    }
  }
}
```

**Reference:** `FastShowPrettyMacrosImpl.scala` (lines 43-70), `EncoderMacrosImpl.scala` (lines 35-57), `DecoderMacrosImpl.scala` (lines 41-64).

### Handling built-in types

Check types using `<:<` (subtype check) and generate appropriate code.

**From FastShowPrettyMacrosImpl.scala (lines 246-289):**

```scala
object UseBuiltInSupportRule extends DerivationRule("use built-in support") {

  implicit val Boolean: Type[Boolean] = Types.Boolean
  implicit val Byte: Type[Byte] = Types.Byte
  // ... other type witnesses

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
      if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
        FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
      })
      else if (Type[A] <:< Type[Byte]) Rule.matched(Expr.quote {
        FastShowPrettyUtils.renderByte(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Byte]))
      })
      // ... other built-in types
      else Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a built-in type")
    }
}
```

**Key points:**
- Define type witnesses as `implicit val` for use in expressions
- Use `Type[A] <:< Type[X]` for subtype checks
- Use `ctx.value.upcast[X]` to safely cast the expression

### Handling collections and maps

Use pattern matching on types with extractors like `IsCollection` and `IsMap`.

**From FastShowPrettyMacrosImpl.scala (lines 410-455, 457-520):**

```scala
object HandleAsCollectionRule extends DerivationRule("handle as collection") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
      Type[A] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          // ... derive using ctx.nest for items
        case _ =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a collection"))
      }
    }
}
```

### Handling case classes

Use `CaseClass.parse[A]` to introspect case class structure.

**From FastShowPrettyMacrosImpl.scala (lines 291-374):**

```scala
object HandleAsCaseClassRule extends DerivationRule("handle as case class") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a case class") >> {
      CaseClass.parse[A] match {
        case Some(caseClass) =>
          val name = Expr(Type[A].shortName)
          NonEmptyList.fromList(caseClass.caseFieldValuesAt(ctx.value).toList) match {
            case Some(fieldValues) =>
              // derive each field recursively
            case None =>
              // handle zero-field case class
          }
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be a case class"))
      }
    }
}
```

### Handling enums/sealed traits

Use `Enum.parse[A]` and `parMatchOn` for exhaustive case handling.

**From FastShowPrettyMacrosImpl.scala (lines 376-408):**

```scala
object HandleAsEnumRule extends DerivationRule("handle as enum") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as an enum") >> {
      Enum.parse[A] match {
        case Some(enumm) =>
          enumm
            .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
              import matched.{value as enumCaseValue, Underlying as EnumCase}
              // derive each case recursively
            }
        case None =>
          MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is not considered to be an enum"))
      }
    }
}
```

## Decoder-style derivation: constructing types from decoded data

**Reference implementation:** `circe-derivation/src/main/scala/hearth/kindlings/circederivation/internal/compiletime/DecoderMacrosImpl.scala`

Encoder-style derivation (FastShowPretty) **reads** fields from an existing value via `caseClass.caseFieldValuesAt(expr)`. Decoder-style derivation **constructs** a value from separately decoded fields. This introduces additional challenges around type safety and Scala 2 macro hygiene.

### Two approaches to case class construction

#### Approach 1: Recursive flatMap chain (recommended)

Build nested `flatMap` calls where each level uses `LambdaBuilder.of1[Field]` to get a properly-typed expression, then pass it via `Expr_??` to the constructor.

```scala
def buildFlatMapChain(
    fields: List[(String, Parameter, Expr[Decoder[?]])],
    accumulatedArgs: Map[String, Expr_??]
): MIO[Expr[Either[DecodingFailure, A]]] = fields match {
  case Nil =>
    // All fields decoded — construct the case class
    caseClass.primaryConstructor(accumulatedArgs) match {
      case Right(constructExpr) =>
        MIO.pure(Expr.quote { Right(Expr.splice(constructExpr)): Either[DecodingFailure, A] })
      case Left(error) => MIO.fail(...)
    }
  case (fieldName, param, decoderExpr) :: rest =>
    import param.tpe.Underlying as Field
    // LambdaBuilder.of1[Field] gives a properly-typed Expr[Field] in the closure
    LambdaBuilder.of1[Field]("fieldValue").traverse { fieldValueExpr =>
      // as_?? wraps it for the arguments map
      buildFlatMapChain(rest, accumulatedArgs + (fieldName -> fieldValueExpr.as_??))
    }.map { builder =>
      val innerLambda = builder.build[Either[DecodingFailure, A]]
      Expr.quote {
        cursor.downField(config.transformMemberNames(Expr.splice(Expr(fieldName))))
          .as(Expr.splice(decoderExpr))
          .flatMap(Expr.splice(innerLambda))
      }
    }
}
```

**Why this works:** `LambdaBuilder.of1[Field]` properly handles the path-dependent `Field` type. Inside the builder closure, `fieldValueExpr` is already `Expr[Field]`, so no casts are needed. Calling `.as_??` wraps it into an existential for the untyped field map.

#### Approach 2: Collect-then-construct with runtime type witness

Decode all fields into `List[Either[DecodingFailure, Any]]`, sequence them into `Either[DecodingFailure, Array[Any]]`, then use a runtime utility to recover types.

```scala
// Runtime utility — the Decoder[A] argument provides type inference for A
@scala.annotation.nowarn("msg=unused explicit parameter")
def unsafeCast[A](value: Any, witness: Decoder[A]): A = value.asInstanceOf[A]

// In macro — decoderExpr carries the type, avoiding path-dependent references
val makeAccessor: Expr[Array[Any]] => (String, Expr_??) = { arrExpr =>
  val typedExpr = Expr.quote {
    CirceDerivationUtils.unsafeCast(
      Expr.splice(arrExpr)(Expr.splice(Expr(param.index))),
      Expr.splice(decoderExpr)  // type A inferred from Decoder[A]
    )
  }
  (fieldName, typedExpr.as_??)
}
```

This approach is used in `circe-derivation/DecoderMacrosImpl.scala`. It works but is more complex than the recursive flatMap chain.

### Using `primaryConstructor` directly

`CaseClass` provides two ways to construct instances:

1. **`caseClass.construct[F](makeArgument)`** — uses `ConstructField[F]` with a dependent return type `Expr[field.tpe.Underlying]`. This has the same path-dependent type issue on Scala 2.

2. **`caseClass.primaryConstructor(fieldMap: Map[String, Expr_??])`** — takes a `Map[String, Expr_??]` and returns `Either[String, Expr[A]]`. This avoids path-dependent types entirely because `Expr_??` is an existential.

**Always prefer `primaryConstructor(fieldMap)` for decoder-style derivation.** Use `construct` only when you already have properly-typed field expressions (e.g., encoder-style where `caseFieldValuesAt` gives you typed values).

### Key API: `Expr_??` and `as_??`

`Expr_??` is `Existential[Expr]` — it wraps an `Expr[A]` with its `Type[A]` proof, erasing the concrete type from the outer signature.

```scala
// Wrapping: any Expr[A] with Type[A] in scope can become Expr_??
val existential: Expr_?? = someExpr.as_??

// Consuming: import brings type and value back into scope
import existential.{Underlying as FieldType, value as expr}
// Now: implicit FieldType: Type[FieldType] and expr: Expr[FieldType]
```

Use `Expr_??` whenever you need to store heterogeneously-typed expressions in a collection (e.g., a field map for `primaryConstructor`).

### Key API: `LambdaBuilder`

`LambdaBuilder` creates runtime lambda expressions from compile-time derivation:

```scala
LambdaBuilder
  .of1[InputType]("argName")
  .traverse { (inputExpr: Expr[InputType]) =>
    // MIO computation that produces the body
    deriveBody(inputExpr): MIO[Expr[OutputType]]
  }
  .map { builder =>
    val lambda: Expr[InputType => OutputType] = builder.build[OutputType]
    // Use the lambda in generated code
    Expr.quote { someResult.map(Expr.splice(lambda)) }
  }
```

**Important:** Always use `Expr.quote`/`Expr.splice` inside builder closures, never raw `'{ }` / `${ }` — raw quotes capture the wrong `Quotes` context on Scala 3 and cause `ScopeException`.

## Cross-compilation pitfalls

### Path-dependent types in `Expr.quote` (Scala 2)

**This is the most common pitfall.** On Scala 2, `import param.tpe.Underlying as Field` and then referencing `Field` inside `Expr.quote` generates code that references the path variable (`param`), which doesn't exist at the expansion site.

```scala
// BROKEN on Scala 2:
import param.tpe.Underlying as Field
Expr.quote { someExpr.asInstanceOf[Field] }  // "not found: value param"
```

This works on Scala 3 but NOT on Scala 2. The `caseFieldValuesAt` pattern in FastShowPretty avoids this because it reads from an existing value (no construction needed). Decoder-style derivation hits this when constructing new instances.

**Solutions:**
1. Use `LambdaBuilder.of1[Field]` which handles the type parameter correctly
2. Use a runtime type-witness utility (`unsafeCast`) where a value-level argument provides type inference
3. Use `primaryConstructor(Map[String, Expr_??])` instead of `construct` with dependent types

### Macro-internal types inside `Expr.quote` leak on Scala 2

On Scala 2, expressions inside `Expr.quote` have their types captured by reification — even inside `Expr.splice` blocks. If an expression has a type involving macro-internal type aliases (like `??`, `Expr_??`, `UntypedType`), the reified tree includes path-dependent references (e.g., `MyMacroImpl.??`) that fail at the expansion site.

```scala
// BROKEN — Some(Type[A].as_??) has type Option[MyMacroImpl.this.??]
Expr.quote {
  Expr.splice {
    val opt: Option[??] = Some(Type[A].as_??)  // leaks into reified tree
    doSomethingWith(opt)
  }
}

// CORRECT — compute outside Expr.quote, reference by variable name
val opt: Option[??] = Some(Type[A].as_??)
Expr.quote {
  Expr.splice {
    doSomethingWith(opt)  // only the variable name appears in the reified tree
  }
}
```

**Rule of thumb:** Extract any expression involving macro-internal types to a `val` before the `Expr.quote` block.

### `Array` operations require `ClassTag` in macros

`Array.empty[T]` and `+:` inside `Expr.quote` require `ClassTag[T]`, which causes "not found: value ClassTag" errors on Scala 2.

**Solution:** Use `List.empty[T]` and `::` instead.

### `Expr.upcast` only widens (and needs source `Type` in scope)

`expr.upcast[B]` requires `A <:< B` (compile-time subtype proof). It cannot narrow types (e.g., `Any` → `String`). For narrowing, use `.asInstanceOf` inside `Expr.quote` or a runtime type-witness utility.

**Additional constraint:** `upcast` also requires `Type[A]` (the source type) to be in scope. If you're trying to upcast an expression whose type comes from a pattern matcher (e.g., `isMap.CtorResult`), you must first import the path-dependent type to bring its `Type` into scope — but that's the same problem you're trying to solve. Use `.asInstanceOf` inside `Expr.quote` instead.

### Macro methods require concrete types at call site

A generic `def helper[A](value: A)` that calls `KindlingsEncoder.encode(value)` internally won't work — the macro sees `A` as abstract, not the concrete type. Always call macro methods directly with concrete types at each call site.

### Sibling `Expr.splice` blocks have isolated `Quotes` contexts (Scala 3)

**This is a critical constraint for combined codec derivation.** On Scala 3, Hearth's `hearth-cross-quotes` compiler plugin transforms `Expr.splice` calls inside `Expr.quote` into `${ CrossQuotes.nestedCtx { ... } }`. Each splice gets its **own separate nested `Quotes` context**. Expressions (including `Type` instances) created in one splice **cannot be used in a sibling splice** within the same `Expr.quote`.

```scala
// BROKEN on Scala 3 — `myType` created in first splice, used in second:
Expr.quote {
  val encResult = Expr.splice {
    val myType = Type.of[Either[String, Int]]  // created here
    deriveEncoder[A]  // uses myType internally
  }
  val decResult = Expr.splice {
    deriveDecoder[A]  // also uses myType — ERROR: "Expression created in a splice was used outside of that splice"
  }
}
```

This affects any scenario where derivation logic in sibling splices shares state — even indirectly through `MIO` computations, `Environment.loadStandardExtensions()`, or `ValDefsCache`. Standard library types like `Either`, `Nothing`, `Option` etc. are commonly loaded by `loadStandardExtensions()` and stored globally; if the first splice triggers loading, the resulting `Type` instances belong to that splice's context.

**Solution — pre-derive with `LambdaBuilder`:**

Perform all derivation in a **single `MIO.scoped`/`runSafe` call** before the `Expr.quote`, producing pre-built function expressions via `LambdaBuilder`. Then splice only the finished function references into the quote:

```scala
MIO.scoped { runSafe =>
  val cache = ValDefsCache.mlocal
  val selfType: Option[??] = Some(Type[A].as_??)

  val (encLambdaRaw, decLambdaRaw, nullVal, vals) = runSafe {
    for {
      _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
      // Derive encoder as (A, Writer, Config) => Unit
      encLambdaRaw <- LambdaBuilder
        .of3[A, Writer, Config]("value", "writer", "config")
        .traverse { case (v, w, c) => deriveEncoder[A](using EncoderCtx.from(v, w, c, cache, selfType)) }
        .map(_.build[Unit])
      // Derive decoder as (Reader, Config) => A
      decLambdaRaw <- LambdaBuilder
        .of2[Reader, Config]("reader", "config")
        .traverse { case (r, c) => deriveDecoder[A](using DecoderCtx.from(r, c, cache, selfType)) }
        .map(_.build[A])
      nullVal <- deriveNullValue[A]
      vals <- cache.get
    } yield (encLambdaRaw, decLambdaRaw, nullVal, vals)
  }

  // Wrap val defs around the ENTIRE result, not individual lambdas
  val resultExpr = Expr.quote {
    new MyCodec[A] {
      private val _encode = Expr.splice(encLambdaRaw)
      private val _decode = Expr.splice(decLambdaRaw)
      def nullValue: A = Expr.splice(nullVal)
      def encode(x: A, out: Writer): Unit = _encode(x, out, Expr.splice(configExpr))
      def decode(in: Reader): A = _decode(in, Expr.splice(configExpr))
    }
  }
  vals.toValDefs.use(_ => resultExpr)
}
```

**Key principles:**
1. All derivation (encoder, decoder, null value) happens in one `runSafe` scope
2. `LambdaBuilder` packages each derivation as a function `Expr` before the quote
3. Inside the `Expr.quote`, splices only reference pre-built `Expr` values — no derivation logic runs inside splices
4. `vals.toValDefs.use` wraps the entire result expression (see next pitfall)

**Reference:** `jsoniter-derivation/CodecMacrosImpl.scala` — `deriveCodecFromCtxAndAdaptForEntrypoint` method.

### `ValDefsCache` wrapping scope for multi-function derivation

When a single derivation produces multiple function expressions (e.g., encoder + decoder for a combined codec), **do not** wrap cached val defs around each function individually. This causes duplicate definitions and unused-method warnings (fatal with `-Xfatal-warnings`).

```scala
// BROKEN — each lambda gets ALL cached defs, including the other side's unused methods:
vals <- cache.get
encodeFn = vals.toValDefs.use(_ => encLambdaRaw)  // has decode_X (unused!)
decodeFn = vals.toValDefs.use(_ => decLambdaRaw)  // has encode_X (unused!)

// CORRECT — wrap the outermost expression containing both lambdas:
vals <- cache.get
val resultExpr = provideCtxAndAdapt(encLambdaRaw, decLambdaRaw, nullVal)
vals.toValDefs.use(_ => resultExpr)  // all defs in scope for both lambdas
```

**When using a shared `ValDefsCache` for encoder and decoder:** This works correctly because they use different cache keys (`"cached-encode-method"` vs `"cached-decode-method"`) and different arity types. The shared `"cached-codec-instance"` key is intentionally shared — both sides reference the same cached codec instance for recursive types.

**Reference:** `jsoniter-derivation/CodecMacrosImpl.scala` — see how `vals.toValDefs.use` wraps the entire `provideCtxAndAdapt` result.

### Path-dependent types from `IsMap`/`IsCollection` need explicit import

When using pattern matchers like `IsMap` or `IsCollection`, the matched type exposes path-dependent types (e.g., `isMap.Key`, `isMap.Value`, `isMap.CtorResult`). These must be **explicitly imported** to bring their `Type` instances into scope, otherwise Scala 3 will fail with missing `scala.quoted.Type` errors when those types appear inside `Expr.quote`.

```scala
// BROKEN on Scala 3 — CtorResult has no Type in scope:
Type[A] match {
  case IsMap(isMap) =>
    val factoryExpr = isMap.factory  // Expr[Factory[..., isMap.CtorResult]]
    Expr.quote { ... Expr.splice(factoryExpr) ... }  // error: no Type[isMap.CtorResult]
}

// CORRECT — import brings Type instances into scope:
Type[A] match {
  case IsMap(isMap) =>
    import isMap.{Key, Value, CtorResult}  // brings Type[Key], Type[Value], Type[CtorResult]
    val factoryExpr = isMap.factory
    Expr.quote { ... Expr.splice(factoryExpr) ... }  // works!
}
```

The same pattern applies to `IsCollection` (`import isCollection.Underlying as Item`), `IsOption`, and `IsValueType`.

**Reference:** `circe-derivation/DecoderMacrosImpl.scala` — map and collection rules.

### Discriminator-style enum decoding in streaming codecs

When implementing a combined codec for a streaming JSON library (like jsoniter-scala), the discriminator-style enum decoder has a fundamental constraint: after reading the discriminator field from the JSON object, the remaining fields must be read **from the same already-opened object**, not as a separate nested object.

```json
// Wrapper-style: each child is a separate object under a key
{"Dog": {"name": "Rex", "breed": "Labrador"}}

// Discriminator-style: discriminator + fields are in the SAME object
{"type": "Dog", "name": "Rex", "breed": "Labrador"}
```

For wrapper-style, the child decoder reads a complete standalone object (starting with `{`). For discriminator-style, the outer object is already open — the child decoder must read the **remaining fields** without expecting an opening `{`.

**Solution:** Implement two variants:
1. `decodeCaseClassFields` — reads a full object (with `{` ... `}`)
2. `decodeCaseClassFieldsInline` — reads remaining fields from an already-opened object

The enum decoder rule must build **two dispatch functions** — one for wrapper mode and one for inline/discriminator mode — and select between them based on the config:

```scala
children.parTraverse { case (childName, child) =>
  for {
    wrapper <- deriveChildDecoder[A, ChildType](childName)        // full object
    inline  <- deriveChildDecoderInline[A, ChildType](childName)  // fields only
  } yield (wrapper, inline)
}.flatMap { allDispatchers =>
  for {
    wrapperFn <- buildDispatchLambda(allDispatchers.map(_._1))
    inlineFn  <- buildDispatchLambda(allDispatchers.map(_._2))
  } yield Expr.quote {
    config.discriminatorFieldName match {
      case Some(field) => readWithDiscriminator(reader, field)(Expr.splice(inlineFn))
      case None        => readWrapped(reader)(Expr.splice(wrapperFn))
    }
  }
}
```

This issue does not arise in cursor-based libraries (like circe) where the decoder receives a `HCursor` that can navigate freely.

**Reference:** `jsoniter-derivation/CodecMacrosImpl.scala` — `decodeEnumCases`, `deriveChildDecoderInline`, `decodeCaseClassFieldsInline`. `jsoniter-derivation/runtime/JsoniterDerivationUtils.scala` — `readWithDiscriminator`, `readObjectInline`.

### Built-in type rule is mandatory when the library lacks implicit instances

Some libraries (like circe) provide implicit instances for primitive types (e.g., `Encoder[Int]`, `Decoder[String]`). Others (like jsoniter-scala) do **not** provide standalone `JsonValueCodec[Int]` instances — the macro must generate direct read/write calls.

If you skip the built-in type rule, primitive fields inside case classes will fall through to the collection rule (e.g., `String` matches `IsCollection[Char]`), causing cascading failures.

**Always implement a built-in type rule** that handles at minimum: `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String`. For value types (`extends AnyVal`), the value type rule unwraps to the underlying type, which then needs the built-in rule.

### `group()` is from `MacroSuite`, not plain `FunSuite`

Tests that need `group("...")` blocks must extend `MacroSuite` (from `hearth-munit`), which in turn depends on hearth. Modules that do NOT depend on hearth (e.g., a standalone JSON AST module) cannot use `MacroSuite`. In that case, extend plain `munit.FunSuite` and use flat test names instead of nested `group()` blocks.

## Implementation requirements checklist

Every type class derivation must satisfy all requirements below. Use this checklist when implementing a new derivation and when verifying that an existing one is complete. Each item includes what to implement, where to look for a reference, and how to verify it.

### REQ-1: Dual entry points — type class instance AND inlined body

The macro must provide **two** entry points:

1. **`deriveTypeClass[A]`** — returns `Expr[MyTypeClass[A]]`, creating a new instance of the type class
2. **`deriveInline[A]`** — returns the result type directly (e.g., `Expr[String]`, `Expr[Either[Error, A]]`), embedding the derivation logic inline without allocating a type class instance

Both entry points should delegate to the same core derivation logic (e.g., `deriveFromCtxAndAdaptForEntrypoint`). The difference is only in how the result is wrapped.

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 16-68 (`deriveInline` and `deriveTypeClass`), `DecoderMacrosImpl.scala` lines 17-64.

**Verification:**
- Confirm both `deriveTypeClass` and `deriveInline` methods exist in the macro impl
- Confirm the Scala 2 and Scala 3 bridge files expose both entry points
- Confirm the public companion object has both `derived` (type class) and an inline method (e.g., `render`, `decode`)
- Write a test that calls the inline method directly and another that uses the derived type class instance

### REQ-2: Recursive derivation with caching

The macro must handle nested structures **recursively**, inlining the derivation logic rather than allocating intermediate type class instances where possible.

#### REQ-2a: Case class and enum derivation cached as defs

Derived logic for case classes and enums must be **cached as local `def`s** (not inlined at every use site). This:
- Keeps generated code compact (avoids exceeding JVM method size limits)
- Improves compilation speed (derive once, call many times)
- Improves runtime performance (no redundant allocations)
- Handles recursive data types automatically (the def can reference itself before it is fully derived)

**How to implement:**
1. In the context class, provide `getHelper[B]` and `setHelper[B]` methods that wrap `cache.getNAry` / `cache.buildCachedWith` with `ValDefBuilder.ofDefN[...]`
2. At the start of case class / enum rules, call `ctx.setHelper[A]` to forward-declare the def, then derive the body, then return the cached call
3. Use `MIO.scoped { runSafe => ... }` inside the builder to convert `MIO[Expr[...]]` to `Expr[...]`

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 172-209 (`getHelper`/`setHelper`), lines 663-670 (case class rule calling `setHelper`), lines 762-770 (enum rule calling `setHelper`).

**Verification:**
- Define a recursive data type (e.g., `case class Tree(children: List[Tree])`) and verify it compiles and works at runtime
- Verify the generated code does not grow exponentially with nesting depth — check that derivation of a deeply nested type completes quickly
- Write a test with a recursive ADT that would stack-overflow or fail compilation without caching

### REQ-3: Implicit resolution

#### REQ-3a: Always prefer user-provided implicits

Before deriving from scratch, check if the user has provided an implicit instance of the type class for the current type. If one exists, use it.

**How to implement:** The `UseImplicitWhenAvailableRule` must appear in the rule chain **before** built-in, case class, and enum rules (but after the cache rule).

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 330-370 (`UseImplicitWhenAvailableRule`).

**Verification:**
- Define a type with a manually-provided implicit instance
- Verify the macro uses the manual instance rather than deriving one
- Test that nested types also pick up manual instances for their fields

#### REQ-3b: Ignore self-summoning implicits

When looking for implicits, **exclude** the method(s) that trigger this macro. Otherwise the macro would summon itself, causing infinite recursion.

**How to implement:** Collect the method symbols to ignore (e.g., the `derived` method on the companion object) and pass them to `Type[TC[A]].summonExprIgnoring(ignoredImplicits*)`.

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 334-336 (`ignoredImplicits`), line 340 (`summonExprIgnoring`).

**Verification:**
- Verify the macro does not loop infinitely — if `derived` were not ignored, calling `MyTypeClass.derived[SomeCaseClass]` would recursively try to summon `MyTypeClass[SomeCaseClass]`, hitting `derived` again
- A basic compilation test of any case class already validates this; if it compiles, self-summoning is properly ignored

#### REQ-3c: Subtype derivation — look for parent type implicits

When implementing a type class that is a **subtype** of an existing type class (e.g., `KindlingsDecoder <: circe.Decoder`):

1. **Summon implicits of the parent type** (e.g., look for `Decoder[A]`), not just the subtype (`KindlingsDecoder[A]`)
2. **Ignore automatic derivation methods from the parent** — e.g., `Decoder.derived` should be in `ignoredImplicits` alongside your own `KindlingsDecoder.derived`
3. **Ignore companion-provided built-in instances from the parent** — if the parent type class provides instances for primitives via its companion object (e.g., `Decoder.decodeInt`), and your macro handles those primitives via a built-in rule, add those companion methods to `ignoredImplicits` as well to avoid summoning them when you can handle the type directly

**Reference:** `DecoderMacrosImpl.scala` lines 276-284 — ignores both `KindlingsDecoder.derived` and `Decoder.derived`.

**Verification:**
- Test that a user-provided `Decoder[MyType]` (parent type) is picked up by the macro
- Test that the macro does not summon `Decoder.derived` for types it should derive itself
- If ignoring parent companion built-ins: test that the macro handles primitives without summoning parent companion instances

#### REQ-3d: Cache resolved implicits as lazy vals

Every successfully resolved implicit must be **cached as a `lazy val`**. Do not summon or allocate the same implicit multiple times in the generated code.

**How to implement:** When `summonExprIgnoring` succeeds, immediately store the result via `ctx.cache.buildCachedWith("instance", ValDefBuilder.ofLazy[TC[A]]("instance"))`, then use the cached reference everywhere.

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 342-344 — after summoning, the implicit is stored in cache and then `UseCachedDefWhenAvailableRule` retrieves it.

**Verification:**
- Test a type that uses the same nested type in multiple fields (e.g., `case class Pair(a: String, b: String)`) — verify compilation succeeds and the implicit is only resolved once
- Inspect derivation logs (see REQ-8) to confirm caching messages appear

### REQ-4: Handle built-in/primitive types without allocation

Built-in types (Boolean, Byte, Short, Int, Long, Float, Double, Char, String, and any others appropriate to the type class) must be handled **directly** — either by generating an expression inline or by delegating to a non-allocating runtime utility. Do not create a type class instance for these.

**How to implement:**
1. Create a `UseBuiltInSupportRule` that checks `Type[A] <:< Type[X]` for each supported type
2. For each match, generate an expression that calls a runtime utility method or produces the result inline
3. Runtime utilities go in `internal/runtime/MyTypeClassUtils.scala`

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 379-514 (`UseBuiltInSupportRule`), `FastShowPrettyUtils.scala` for runtime helpers.

**Verification:**
- Write tests that exercise each built-in type individually
- Verify the built-in rule fires (check derivation logs) rather than falling through to implicit resolution

### REQ-5: Handle value types via std extensions

If the type class should support value types (classes extending `AnyVal`), handle them using hearth's `IsValueType` std extension.

**How to implement:**
1. Create a `HandleAsValueTypeRule` that pattern-matches `Type[A]` against `IsValueType`
2. Unwrap the value type via `isValueType.unwrap(ctx.value)` to get the underlying value
3. Recurse on the underlying type

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 516-546 (`HandleAsValueTypeRule`).

**Verification:**
- Define a value class (e.g., `case class UserId(value: Int) extends AnyVal`)
- Write a test that derives and uses the type class for it
- Verify it works on both Scala 2.13 and Scala 3

### REQ-6: Handle optional types via std extensions

If the type class should support `Option` and similar optional types, handle them using hearth's `IsOption` std extension.

**How to implement:**
1. Create a `HandleAsOptionRule` (or incorporate into built-in support) that pattern-matches against `IsOption`
2. Generate code that checks for `None`/`Some` and recurses on the inner type

**Reference:** Check `FastShowPrettyMacrosImpl.scala` for how optional-like types are handled. Use `IsOption` from hearth's std extensions.

**Verification:**
- Write tests for `Option[A]` where `A` is a primitive and where `A` is a case class
- Test `None` and `Some(...)` cases
- Test nested optionals (`Option[Option[Int]]`)

### REQ-7: Handle collection types via std extensions

If the type class should support collections and maps, handle them using hearth's `IsCollection` and `IsMap` std extensions.

**How to implement:**
1. Create `HandleAsCollectionRule` that pattern-matches `Type[A]` against `IsCollection`
2. Create `HandleAsMapRule` that pattern-matches against `IsMap`
3. Cache the item/key/value derivation as a def (just like case classes) to avoid code duplication when the collection has many elements
4. Use `LambdaBuilder` for generating iteration callbacks

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 548-662 (`HandleAsMapRule`, `HandleAsCollectionRule`).

**Verification:**
- Test `List[A]`, `Vector[A]`, `Set[A]` for collections
- Test `Map[K, V]` for maps
- Test nested collections (`List[List[Int]]`)
- Test collections of case classes

### REQ-8: Log all derivation steps in MIO

Every rule attempt, match, failure, cache hit, cache miss, and recursive descent must be logged via `Log.info(...)` or `Log.namedScope(...)`. This is critical for debugging macro expansion issues.

**How to implement:**
1. Wrap each `deriveResultRecursively` call in `Log.namedScope(s"Deriving for type ${Type[A].prettyPrint}")`
2. At the start of each rule, log that the rule is being attempted
3. On rule match, log what was matched
4. On rule yield, log the reason
5. On cache hit/miss, log accordingly

**Reference:** `FastShowPrettyMacrosImpl.scala` — every rule object starts with `Log.info(...)` and logs on match/yield.

**Verification:** Logging correctness cannot be tested by unit tests directly — it is verified by manual inspection during development. However, the logging infrastructure (the `debug` package and scalac option check) should be implemented and can be tested for existence.

### REQ-9: Configurable logging

Provide the user with **two** ways to enable derivation logging:

#### REQ-9a: Import-based logging

Create a `debug/package.scala` in the module that defines an implicit value. When the user imports it in the scope of the macro call, logging is enabled for that scope.

```scala
package hearth.kindlings.mymodule

package object debug {
  implicit val logDerivationForMyTypeClass: hearth.LogDerivation = hearth.LogDerivation.Enabled
}
```

#### REQ-9b: Scalac option logging

Check for a scalac macro setting at macro expansion time:

```
-Xmacro-settings:myModule.logDerivation=true
```

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 121-136 (how both mechanisms are checked).

**Verification:** Logging configuration cannot be tested by unit tests. Verify by confirming the `debug/package.scala` file exists and the scalac option check is present in the macro bridge code.

### REQ-10: Error aggregation and reporting

When derivation fails, **all** rule failure reasons must be collected and reported together in a single error message. Do not fail on the first error — try all rules and aggregate.

The error message must include:
1. The type that failed to derive
2. Why each rule was not applicable
3. A hint telling the user how to enable debug logging (both the import and the scalac option)

**How to implement:**
1. The `Rules(...)` combinator already collects `Left(reasons)` from all rules
2. Format the reasons into a readable error message
3. Append a line like: `"Enable debug logging with: import hearth.kindlings.mymodule.debug.logDerivationForMyTypeClass or -Xmacro-settings:myModule.logDerivation=true"`

**Reference:** `FastShowPrettyMacrosImpl.scala` lines 77-119 (`DerivationError` and error formatting).

**Verification:**
- Write a test using `compileErrors("MyTypeClass.derived[UnsupportedType]").check(...)` to verify that:
  - The error message is produced (not a crash or infinite loop)
  - The error mentions the type that failed
  - The error includes the debug logging hint
- Test with multiple unsupported nested types to ensure all errors are aggregated

### REQ-11: Comprehensive test coverage

Every requirement above (except logging, REQ-8/REQ-9) must be covered by unit tests. Use the following test plan as a minimum:

| Test category | What to test | Validates |
|---|---|---|
| **Built-in types** | Boolean, Byte, Short, Int, Long, Float, Double, Char, String | REQ-4 |
| **Value types** | `case class Wrapper(value: Int) extends AnyVal` | REQ-5 |
| **Optionals** | `Option[Int]`, `Option[CaseClass]`, `None`, `Some`, nested | REQ-6 |
| **Collections** | `List[Int]`, `Vector[String]`, `Set[CaseClass]`, nested | REQ-7 |
| **Maps** | `Map[String, Int]`, `Map[String, CaseClass]` | REQ-7 |
| **Case classes** | Zero-field, single-field, multi-field, nested | REQ-2 |
| **Enums / sealed traits** | With case objects, case classes, nested | REQ-2 |
| **Recursive types** | `case class Tree(children: List[Tree])` — must work without special user-side tricks | REQ-2a |
| **User-provided implicits** | Manual instance overrides derived one | REQ-3a |
| **Inline entry point** | Call the inline method directly | REQ-1 |
| **Type class entry point** | Use `derived` or summon the type class | REQ-1 |
| **Error messages** | `compileErrors(...)` on unsupported types | REQ-10 |
| **Subtype implicits** | (If applicable) parent type implicit is picked up | REQ-3c |
| **Parent auto-derivation ignored** | (If applicable) parent's automatic derivation does not interfere — see REQ-11b | REQ-3c |
| **Scala 3-only types** | Named tuples, Scala 3 enums — in `src/test/scala-3/` | REQ-11c |

#### REQ-11a: Recursive types must work without additional tricks

Recursive data types must compile and work correctly **without** the user needing to do anything special (no manual forward-declarations, no lazy val workarounds, no special imports). The caching-as-defs strategy (REQ-2a) should handle this transparently.

**Test:**
```scala
// This must just work — no lazy val, no manual instance, no tricks
case class Tree(value: Int, children: List[Tree])
test("recursive data type works transparently") {
  val tree = Tree(1, List(Tree(2, Nil), Tree(3, List(Tree(4, Nil)))))
  val result = MyTypeClass.someMethod(tree)
  assertEquals(result, /* expected */)
}
```

**Verification:** If the test compiles and passes, recursive types are handled. If it fails with a stack overflow or compilation error, the caching strategy is broken.

#### REQ-11b: Parent automatic derivation is ignored (subtype type classes only)

When the type class is a subtype of an existing type class that has its own automatic derivation (e.g., `KindlingsDecoder <: circe.Decoder` where `Decoder` has `Decoder.derived`), write tests proving that the parent's automatic derivation does **not** interfere with ours.

**Why this matters:** If the parent's `derived` is not ignored during implicit summoning, the macro might pick up a parent-derived instance instead of doing its own optimized derivation. This would defeat the purpose of the custom macro (no inlining, no caching, potentially different behavior).

**How to test:**
1. Derive a type that both our macro and the parent's auto-derivation can handle
2. Verify the result is produced by **our** derivation (e.g., check for our specific behavior, configuration support, or optimization characteristics)
3. If the parent library provides auto-derivation via different mechanisms per Scala version, test each Scala version separately using Scala-version-specific test source directories (`src/test/scala-2/` and `src/test/scala-3/`)

**Verification:** The test should confirm that our macro's behavior (e.g., configuration transforms, inlined rendering, custom error formatting) is present in the output — not the parent's default behavior.

#### REQ-11c: Scala 3-only types tested in Scala 3-only tests

Types that only exist in Scala 3 (e.g., Scala 3 `enum` declarations, named tuples) must be tested in `src/test/scala-3/` so they don't break Scala 2.13 compilation.

**Example:**
```scala
// In src/test/scala-3/hearth/kindlings/mymodule/MyTypeClassScala3Spec.scala
import hearth.MacroSuite

final class MyTypeClassScala3Spec extends MacroSuite {
  group("Scala 3 enums") {
    enum Color { case Red, Green, Blue }
    test("handles Scala 3 enum") {
      val result = MyTypeClass.someMethod(Color.Red)
      assertEquals(result, /* expected */)
    }
  }
  group("named tuples") {
    test("handles named tuple") {
      val tuple: (name: String, age: Int) = (name = "Alice", age = 30)
      val result = MyTypeClass.someMethod(tuple)
      assertEquals(result, /* expected */)
    }
  }
}
```

### REQ-12: Suppress compiler warnings in generated code

The macro-generated code must not produce compiler warnings (e.g., "is never used") at the
expansion site. Users should never need `@nowarn` annotations to silence warnings from derived
code.

**How to implement:** After implementing all derivation logic, compile tests WITHOUT any
`@nowarn` annotations. If warnings appear, fix them in the macro — either by adjusting
the code generation pattern or by attaching `@nowarn` annotations to generated definitions.

**Verification:**
- All test files must compile without `@nowarn` annotations
- Run `sbt --client "yourModule/clean ; yourModule3/clean ; test-jvm-2_13 ; test-jvm-3"` and
  verify zero warnings from macro-expanded code

### Test file organization

Tests go in the module's test directories following this structure:

```
my-type-class/src/test/
├── scala/hearth/kindlings/mytypeclass/
│   └── MyTypeClassSpec.scala              # Cross-compiled tests (Scala 2 + 3)
├── scala-2/hearth/kindlings/mytypeclass/
│   └── MyTypeClassScala2Spec.scala        # Scala 2-only tests (if needed)
└── scala-3/hearth/kindlings/mytypeclass/
    └── MyTypeClassScala3Spec.scala         # Scala 3 enums, named tuples, etc.
```

Tests extend `MacroSuite` and use `group()` / nested `test()`.

```scala
import hearth.MacroSuite

final class MyTypeClassSpec extends MacroSuite {
  group("MyTypeClass") {
    group("built-in types") {
      test("handles Int") { /* ... */ }
      test("handles String") { /* ... */ }
      // ...
    }
    group("case classes") {
      test("zero-field case class") { /* ... */ }
      test("nested case class") { /* ... */ }
      test("recursive case class") { /* ... */ }
    }
    group("user-provided implicits") {
      test("uses manual instance over derived") { /* ... */ }
    }
    group("error messages") {
      test("unsupported type reports all reasons") {
        compileErrors("MyTypeClass.derived[UnsupportedType]").check(
          "..." -> "expected error fragment"
        )
      }
    }
  }
}
```

**Run tests with:**

```bash
# Clean first (macros require clean after changes)
sbt --client "yourModule/clean ; yourModule3/clean ; test-jvm-2_13 ; test-jvm-3"
```

**Do NOT use** `++2.13.18` or `++3.7.4` to switch versions.

## Syncing from Hearth

When syncing changes from hearth's `hearth-tests` demo modules back to kindlings:

1. **Package adaptation**: `hearth.demo.allfeatures` -> `hearth.kindlings.<module>`
2. **Scope modifier**: `private[allfeatures]` -> `private[<module>]`
3. **FQN references**: Update fully-qualified names in `Types` object
4. **Test imports**: `hearth.examples.ExampleValueClass` -> define locally in test file
5. **Test base class**: Use `MacroSuite` (from `hearth-munit`)
6. **Source set dirs**: hearth uses `scala-newest` / `scala-newest-2` / `scala-newest-3`; kindlings uses `scala` / `scala-2` / `scala-3`

## Workflow summary

1. **Verify APIs with MCP** before writing any code
2. **Create context class** with all needed types, expressions, and cache
3. **Define rules** as objects extending a base rule trait
4. **Log at every decision point** using `Log.info` and `Log.namedScope`
5. **Cache definitions** using `ValDefsCache` to avoid duplication
6. **Derive recursively** using `ctx.nest(...)` for nested types
7. **Ignore the derivation macro** when summoning implicits
8. **For decoder-style derivation**, use `primaryConstructor(fieldMap)` with `Expr_??` — avoid `construct` with dependent types
9. **Avoid path-dependent types in `Expr.quote`** — use `LambdaBuilder` or runtime type witnesses instead
10. **Validate against the implementation requirements checklist** (REQ-1 through REQ-12) — ensure every requirement is met before considering the implementation complete
11. **Test in your module** after MCP confirms compilation — use the test plan from REQ-11
