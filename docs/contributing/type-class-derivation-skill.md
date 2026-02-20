# Skill: Type Class Derivation

Use this skill when implementing or modifying type class derivation macros in this repository.

**Reference implementation:** `fast-show-pretty/src/main/scala/hearth/kindlings/fastshowpretty/internal/compiletime/FastShowPrettyMacrosImpl.scala`

This module demonstrates how to derive type classes using hearth. When creating derivations for different type classes in other modules, follow the same patterns and conventions shown here.

## Before writing code

1. **Use MCP to verify available APIs** - Query the `kindlings-metals` MCP server (at `.metals/mcp.json`) to confirm that types, methods, and imports you plan to use actually exist in hearth
2. **Read the reference implementation** - Study `FastShowPrettyMacrosImpl.scala` to understand the patterns
3. **Check hearth documentation** - See `hearth-documentation-skill.md` for how to find the right docs version

## Core architecture

### Context-based parameter passing

Instead of passing many parameters through method signatures, bundle them into a context case class. This makes the code easier to modify and extend.

**From FastShowPrettyMacrosImpl.scala (lines 94-131):**

```scala
final case class DerivationCtx[A](
    tpe: Type[A],
    sb: Expr[StringBuilder],
    value: Expr[A],
    cache: MLocal[ValDefsCache],
    config: Expr[RenderConfig],
    level: Expr[Int]
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

## Testing

When you create a type class derivation in a **new module**:

1. Create tests in that module's test directory
2. Test built-in types, collections, case classes, and enums
3. Test recursive types and error cases

Use `fast-show-pretty` tests as a reference for test structure, but write tests in your own module.

**Only run tests after MCP shows no compilation issues.**

This project uses **sbt-projectmatrix**. Scala version is determined by project suffix, not `++` commands.

```bash
# For your module (replace 'yourModule' with actual name)
sbt --client "yourModule/test"       # Scala 2.13 JVM
sbt --client "yourModule3/test"      # Scala 3 JVM
sbt --client "yourModuleJS/test"     # Scala 2.13 JS
sbt --client "yourModuleJS3/test"    # Scala 3 JS
```

**Do NOT use** `++2.13.18` or `++3.7.4` to switch versions.

## Workflow summary

1. **Verify APIs with MCP** before writing any code
2. **Create context class** with all needed types, expressions, and cache
3. **Define rules** as objects extending a base rule trait
4. **Log at every decision point** using `Log.info` and `Log.namedScope`
5. **Cache definitions** using `ValDefsCache` to avoid duplication
6. **Derive recursively** using `ctx.nest(...)` for nested types
7. **Ignore the derivation macro** when summoning implicits
8. **Test in your module** after MCP confirms compilation
