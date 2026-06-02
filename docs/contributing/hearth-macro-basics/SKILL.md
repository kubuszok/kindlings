---
name: hearth-macro-basics
description: >
  Core Hearth macro architecture: rule-based derivation, context passing, MIO effects,
  ValDefsCache caching, recursive derivation with implicit ignoring. Foundation for all
  type class derivation modules.
paths:
  - "**/compiletime/**Macros*.scala"
user-invocable: false
---

# Skill: Hearth Macro Basics

Core architecture for building type class derivation macros with Hearth. All derivation
modules in Kindlings share these foundational patterns.

**Reference implementation:** `fast-show-pretty/src/main/scala/hearth/kindlings/fastshowpretty/internal/compiletime/`

## Context-based parameter passing

Bundle derivation parameters into a context case class instead of passing many arguments.

```scala
final case class DerivationCtx[A](
    tpe: Type[A],
    sb: Expr[StringBuilder],
    value: Expr[A],
    config: Expr[RenderConfig],
    level: Expr[Int],
    cache: MLocal[ValDefsCache],
    derivedType: Option[??]    // None for inline, Some for derived (see "Self-type skip")
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

## Rule-based derivation architecture

Define derivation as a sequence of rules. Each rule checks if it applies and returns
either a matched result or yields with a reason.

```scala
abstract class DerivationRule(val name: String) extends Rule {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]]
}

def deriveResultRecursively[A: DerivationCtx]: MIO[Expr[StringBuilder]] =
  Log.namedScope(s"Deriving for type ${Type[A].prettyPrint}") {
    Rules(
      UseCachedDefWhenAvailableRule,
      UseImplicitWhenAvailableRule,
      HandleAsNamedTupleRule,     // Scala 3 named tuples — BEFORE HandleAsValueType
      UseBuiltInSupportRule,
      HandleAsValueTypeRule,      // opaque types, refined, iron — AFTER named tuples
      HandleAsOptionRule,
      HandleAsMapRule,            // BEFORE HandleAsCollection (Map <: Iterable)
      HandleAsCollectionRule,
      HandleAsSingletonRule,
      HandleAsCaseClassRule,
      HandleAsEnumRule
    )(_[A]).flatMap { ... }
  }
```

### Canonical rule ordering

The ordering above is semantic — each position matters:

| Position | Rule | Why this order |
|---|---|---|
| 1 | `UseCachedDef` | Short-circuit if already derived (recursion, shared types) |
| 2 | `UseImplicit` | User-provided instances override everything else |
| 3 | `HandleAsNamedTuple` | **Must precede ValueType**: Scala 3 named tuples are opaque types that `IsValueType` would intercept. Guard with `Type[A].isNamedTuple` |
| 4 | `UseBuiltIn` | String, Int, Boolean etc. — no allocation needed |
| 5 | `HandleAsValueType` | Opaque types (refined, iron, newtypes) via `IsValueType` providers |
| 6 | `HandleAsOption` | `Option[A]` via `IsOption` extractor |
| 7 | `HandleAsMap` | `Map[K,V]` via `IsMap` — **must precede Collection** because `Map <: Iterable` |
| 8 | `HandleAsCollection` | `List`, `Set`, `Vector` etc. via `IsCollection` |
| 9 | `HandleAsSingleton` | Case objects, `None.type`, `Nil.type` via `SingletonValue.parse` |
| 10 | `HandleAsCaseClass` | Product types via `CaseClass.parse` |
| 11 | `HandleAsEnum` | Sum types via `Enum.parse` |

**Critical ordering constraints:**
- **NamedTuple before ValueType**: On Scala 3, `(field: Int)` is an opaque type alias. Without the guard, `IsValueType` matches it as a single-field wrapper, stripping the named-tuple semantics.
- **Map before Collection**: `Map[K,V] <: Iterable[(K,V)]`, so `IsCollection` would match maps as pair collections, producing wrong codegen (e.g., JSON array instead of JSON object).
- **Singleton before CaseClass**: `CaseClass.parse` no longer handles singletons; they need their own rule.

**Key points:**
- Rules are tried in order; first match wins
- Use `Rule.matched(...)` when the rule applies
- Use `Rule.yielded(...)` when the rule doesn't apply (with reason)
- The `Rules(...)` combinator from hearth handles the orchestration

## Logging with MIO and Log

Log at every decision point using `Log` from `hearth.fp.effect`.

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
```

**Key points:**
- Use `Log.info(...)` before attempting each rule
- Use `Log.info(...)` when a rule matches or fails
- Use `Log.namedScope(...)` to create hierarchical log structure
- Logs help debug macro expansion issues

## Caching with ValDefsCache and MLocal

Cache generated definitions to avoid code duplication and enable recursive types.

```scala
object UseCachedDefWhenAvailableRule extends DerivationRule("use cached def when available") {
  implicit val StringBuilder: Type[StringBuilder] = Types.StringBuilder

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use cached definition for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      ctx.cache.get0Ary[FastShowPretty[A]]("instance").flatMap {
        case Some(instance) =>
          Log.info(s"Found cached instance for ${Type[A].prettyPrint}, using it") >>
            MIO.pure(Rule.matched(Expr.quote {
              Expr.splice(instance).render(Expr.splice(ctx.sb))(Expr.splice(ctx.value))
            }))
        case None =>
          ctx.cache.get1Ary[A, StringBuilder]("helper").flatMap {
            case Some(helperCall) =>
              Log.info(s"Found cached helper call for ${Type[A].prettyPrint}, using it") >>
                MIO.pure(Rule.matched(helperCall(ctx.value)))
            case None =>
              MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} does not have a cached definition"))
          }
      }
    }
}

// Building a cached helper def for collections:
MIO.scoped { runSafe =>
  ctx.cache.buildCachedWith(
    helperCacheKey,
    ValDefBuilder.ofDef1[Item, StringBuilder]("renderItem", "item")
  ) { case (_, itemExpr) =>
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

## Recursive derivation

Call `deriveResultRecursively` for nested types, updating the context appropriately.

```scala
// For case class fields:
fieldValues
  .parTraverse { case (fieldName, fieldValue) =>
    import fieldValue.{Underlying as Field, value as fieldExpr}
    Log.namedScope(s"Deriving ${ctx.value.prettyPrint}.$fieldName: ${Field.prettyPrint}") {
      deriveResultRecursively[Field](using ctx.incrementLevel.nest(fieldExpr)).map { fieldResult =>
        (fieldName, fieldResult)
      }
    }
  }

// For enum cases:
enumm
  .parMatchOn[MIO, StringBuilder](ctx.value) { matched =>
    import matched.{value as enumCaseValue, Underlying as EnumCase}
    Log.namedScope(s"Deriving ${enumCaseValue.prettyPrint}: ${EnumCase.prettyPrint}") {
      deriveResultRecursively[EnumCase](using ctx.incrementLevel.nest(enumCaseValue))
    }
  }
```

**Key points:**
- Use `ctx.nest(newExpr)` to create context for nested type
- Use `ctx.incrementLevel` when nesting increases indentation
- Wrap in `Log.namedScope(...)` for hierarchical logging
- Use `.parTraverse` for parallel derivation of independent items (requires Hearth 0.2.0-268+)

## Ignoring implicits to prevent self-summoning

Prevent the macro from summoning itself (which would cause infinite recursion).

```scala
object UseImplicitWhenAvailableRule extends DerivationRule("use implicit when available") {

  lazy val ignoredImplicits = Type.of[FastShowPretty.type].methods.collect {
    case method if method.value.name == "derived" => method.value.asUntyped
  }

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use implicit for ${Type[A].prettyPrint}") >> {
      implicit val FastShowPretty: Type[FastShowPretty[A]] = Types.FastShowPretty[A]

      Type[FastShowPretty[A]].summonExprIgnoring(ignoredImplicits*).toEither match {
        case Right(instanceExpr) =>
          Log.info(s"Found implicit ${instanceExpr.prettyPrint}, caching it")
          ctx.cache.buildCachedWith("instance", ValDefBuilder.ofLazy[FastShowPretty[A]]("instance"))(_ =>
            instanceExpr
          ) >> UseCachedDefWhenAvailableRule[A]
        case Left(reason) =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} has no implicit instance: $reason"))
      }
    }
}
```

**Key points:**
- Get method symbols from the companion object that should be ignored
- Use `Type[TC[A]].summonExprIgnoring(symbols*)` to summon while skipping specific methods
- For subtype derivation (e.g., `KindlingsDecoder <: circe.Decoder`), also ignore the **parent library's** companion implicits

## Self-type skip in `derived` entrypoint

When the user writes `implicit val tc: MyTypeClass[X] = MyTypeClass.derived[X]`, the macro
might find `tc` itself during implicit search, generating code that calls itself infinitely.

**Solution:** Track the type being derived in the context and skip implicit search when the
current type matches the derived type.

```scala
final case class DerivationCtx[A](
    tpe: Type[A],
    cache: MLocal[ValDefsCache],
    derivedType: Option[??]   // None for inline, Some for derived
)
```

In the **`derived`** entrypoint, set `derivedType = Some(Type[A].as_??)`.
In the **inline** entrypoint, set `derivedType = None`.

In the implicit rule, check before searching:

```scala
if (ctx.derivedType.exists(_.Underlying =:= Type[A]))
  MIO.pure(Rule.yielded(s"Skipping implicit search for self type ${Type[A].prettyPrint}"))
else
  Type[TC[A]].summonExprIgnoring(ignoredImplicits*).toEither match { ... }
```

**Cross-compilation pitfall:** The expression `Some(Type[A].as_??)` must be extracted to
a local `val` **before** the `Expr.quote` block. See [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md).

## Handling built-in types

Check types using `<:<` (subtype check) and generate appropriate code.

```scala
object UseBuiltInSupportRule extends DerivationRule("use built-in support") {

  implicit val Boolean: Type[Boolean] = Types.Boolean
  implicit val Byte: Type[Byte] = Types.Byte

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to use built-in support for ${Type[A].prettyPrint}") >> MIO {
      if (Type[A] <:< Type[Boolean]) Rule.matched(Expr.quote {
        FastShowPrettyUtils.renderBoolean(Expr.splice(ctx.sb))(Expr.splice(ctx.value.upcast[Boolean]))
      })
      else Rule.yielded(s"${Type[A].prettyPrint} is not a built-in type")
    }
}
```

## Handling collections and maps

Use pattern matching on types with extractors like `IsCollection` and `IsMap`.
Always check `IsMap` before `IsCollection` (because `Map <: Iterable`).

```scala
object HandleAsCollectionRule extends DerivationRule("handle as collection") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a collection") >> {
      Type[A] match {
        case IsCollection(isCollection) =>
          import isCollection.Underlying as Item
          // ... derive using ctx.nest for items
        case _ =>
          MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is not a collection"))
      }
    }
}
```

## Handling singletons

Use `SingletonValue.parse[A]` to detect singleton types. Singletons require a dedicated
rule -- they are no longer handled by `CaseClass.parse`.

```scala
object HandleAsSingletonRule extends DerivationRule("handle as singleton") {

  def apply[A: DerivationCtx]: MIO[Rule.Applicability[Expr[StringBuilder]]] =
    Log.info(s"Attempting to handle ${Type[A].prettyPrint} as a singleton") >> {
      SingletonValue.parse[A].toEither match {
        case Right(_) =>
          val name = Expr(Type[A].shortName)
          MIO.pure(Rule.matched(Expr.quote {
            Expr.splice(ctx.sb).append(Expr.splice(name)).append("()")
          }))
        case Left(reason) =>
          MIO.pure(Rule.yielded(reason))
      }
    }
}
```

For decoder-style derivation, use `sv.singletonExpr` to get the singleton value expression.

## Entry point patterns

Every derivation module's `deriveTypeClass[A]` method follows the same structure:

### Nothing/Any validation

Guard against unintended type inference at the very top:

```scala
if (Type[A] =:= Type.of[Nothing].asInstanceOf[Type[A]] ||
    Type[A] =:= Type.of[Any].asInstanceOf[Type[A]])
  Environment.reportErrorAndAbort(
    s"$macroName: type parameter was inferred as ${Type[A].prettyPrint}, which is likely unintended.")
```

### Placeholder expressions for cache setup

Forward-declared defs via `ValDefsCache` receive actual parameter expressions from
the `buildCachedWith` callback. The initial context uses **placeholder** expressions
(never evaluated) so the cache can be registered before derivation runs:

```scala
val placeholderLeft: Expr[A] = Expr.quote(null.asInstanceOf[A])
val placeholderRight: Expr[A] = Expr.quote(null.asInstanceOf[A])
```

### Config evaluation with semiEval

Every module with configuration evaluates it at macro entry and threads the result
through the derivation context:

```scala
val evConfig: Option[MyConfig] = configExpr.semiEval.toOption
val ctx = DerivationCtx(..., evaluatedConfig = evConfig)
```

When `evaluatedConfig` is `Some`, rules can pre-compute field names, skip runtime boolean
checks, and inline constants. When `None`, rules fall back to runtime evaluation. See
[kindlings-runtime-perf](../kindlings-runtime-perf/SKILL.md) for optimization techniques.

### Annotation support infrastructure

Every module with field-level annotations defines an `AnnotationSupport` trait providing
cross-platform annotation extraction:

```scala
trait AnnotationSupport { this: MacroCommons =>
  protected def hasAnnotationType[Ann: Type](param: Parameter): Boolean
  protected def getAnnotationStringArg[Ann: Type](param: Parameter): Option[String]
  protected def findAnnotationOfType[Ann: Type](param: Parameter): Option[Expr[Ann]]
}
```

Platform-specific implementations live in Scala 2/3 bridge files. The shared macro impl
trait mixes in `AnnotationSupport` via self-type.

### Literal type handling

For singleton literal types (`"hello"`, `42`, `true`), use Hearth's codec extractors:

```scala
Type[A] match {
  case Type.StringCodec(value) => // value: String, compile-time known
  case Type.IntCodec(value)    => // value: Int
  case Type.LongCodec(value)   => // value: Long
  case Type.BooleanCodec(value) => // value: Boolean
  case _ => // not a literal type
}
```

Used in jsoniter/avro literal type rules to inline known values without runtime checks.

## Runtime helpers — when macro-generated code can't express something

When `Expr.quote` cannot reify a certain construct (Newtype aliases, HKT erasure, path-dependent
types on Scala 2), extract the logic into a runtime helper object in `internal/runtime/`:

```scala
// internal/runtime/MyModuleUtils.scala
object MyModuleUtils {
  def convertNewtype(value: Any): Any = // cast through Newtype alias
}
```

The macro-generated code calls the helper: `Expr.quote { MyModuleUtils.convertNewtype(...) }`.

**When to use:**
- Cats Newtype aliases fail in `Expr.quote` on Scala 2 → `CatsConversions` runtime object
- HKT `ConsK.cons` calls have type parameter issues → `ConsKRuntime` wraps with casts
- Factory methods need witness type erasure → `CatsDerivationFactories` with W1/W2/W3/W4

**When NOT to use:** If the construct can be expressed directly in `Expr.quote` (possibly via
a helper method with regular type parameters), prefer that over runtime indirection.

## MLocal beyond ValDefsCache

For tracking derivation state beyond cached defs (e.g., detecting recursive types), create
additional `MLocal` instances:

```scala
val inProgress: MLocal[Set[String]] = MLocal(Set.empty[String])(identity)((a, b) => a ++ b)
```

The merge function `(a, b) => a ++ b` matters for `parTuple` — parallel branches fork MLocal
state, and results are merged via this function.

Used in tapir-schema-derivation for recursive type detection (`inProgress` set tracks which
types are currently being derived).

## Related skills

- [hearth-case-class-rules](../hearth-case-class-rules/SKILL.md) -- case class derivation patterns
- [hearth-enum-rules](../hearth-enum-rules/SKILL.md) -- enum/sealed trait derivation patterns
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- cross-compilation pitfalls
- [hearth-hkt-derivation](../hearth-hkt-derivation/SKILL.md) -- polymorphic type class derivation
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- def-caching pattern for multi-method instances
- [kindlings-new-module](../kindlings-new-module/SKILL.md) -- bootstrapping a new module
- [kindlings-debugging](../kindlings-debugging/SKILL.md) -- debugging derivation
