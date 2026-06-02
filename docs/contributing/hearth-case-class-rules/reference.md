# Case Class Derivation Reference

Detailed patterns for decoder-style case class derivation. See [SKILL.md](SKILL.md) for
the overview.

## Recursive flatMap chain (recommended)

Build nested `flatMap` calls where each level uses `LambdaBuilder.of1[Field]` to get a
properly-typed expression, then pass it via `Expr_??` to the constructor.

```scala
def buildFlatMapChain(
    fields: List[(String, Parameter, Expr[Decoder[?]])],
    accumulatedArgs: Map[String, Expr_??]
): MIO[Expr[Either[DecodingFailure, A]]] = fields match {
  case Nil =>
    // All fields decoded -- construct the case class
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

**Why this works:** `LambdaBuilder.of1[Field]` properly handles the path-dependent `Field`
type. Inside the builder closure, `fieldValueExpr` is already `Expr[Field]`, so no casts
are needed.

## Collect-then-construct with runtime type witness

Decode all fields into `List[Either[DecodingFailure, Any]]`, sequence into
`Either[DecodingFailure, Array[Any]]`, then use a runtime utility to recover types.

```scala
// Runtime utility -- the Decoder[A] argument provides type inference for A
@scala.annotation.nowarn("msg=unused explicit parameter")
def unsafeCast[A](value: Any, witness: Decoder[A]): A = value.asInstanceOf[A]

// In macro -- decoderExpr carries the type, avoiding path-dependent references
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

**Reference:** `circe-derivation/DecoderMacrosImpl.scala`.

## Key API: `LambdaBuilder`

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
    Expr.quote { someResult.map(Expr.splice(lambda)) }
  }
```

**Important:** Always use `Expr.quote`/`Expr.splice` inside builder closures, never raw
`'{ }` / `${ }` -- raw quotes capture the wrong `Quotes` context on Scala 3 and cause
`ScopeException`.

## `nest` vs `nestInCache` semantics

The decoder context provides two methods for creating sub-contexts:

- **`nest[B](newCursor)`** -- used in `deriveFieldDecoder`, `HandleAsOptionRule`, etc.
  The resulting decoder's `apply` method ALWAYS returns `Either` (fail-fast), so `nest`
  must hardcode any mode-specific state. For example, `nest` should force
  `failFast = Expr.quote(true)`.

- **`nestInCache(newCursor, newConfig, newFailFast)`** -- used inside `setHelper` bodies
  where the cursor/config/failFast come from the cached def's formal parameters. This
  preserves all parameters as-is since the cached def handles both modes at runtime.

**Why this matters:** If `nest` propagates the parent's `failFast` parameter, field decoders
created via `LambdaBuilder` would capture `failFast=false` when called from the accumulating
path. Their `apply()` would then return `ValidatedNel` instead of `Either`, causing
`ClassCastException` at runtime.

```scala
// CORRECT -- nest forces fail-fast for sub-decoders
def nest[B: Type](newCursor: Expr[HCursor]): DecoderCtx[B] = copy[B](
  tpe = Type[B],
  cursor = newCursor,
  failFast = Expr.quote(true)  // always fail-fast in sub-derivations
)

// CORRECT -- nestInCache preserves the def's parameters
def nestInCache(newCursor: Expr[HCursor], newConfig: Expr[Configuration],
    newFailFast: Expr[Boolean]): DecoderCtx[A] =
  copy(cursor = newCursor, config = newConfig, failFast = newFailFast)
```

**Reference:** `DecoderMacrosImpl.scala` lines 262-275.

## Dual-path derivation with a runtime boolean parameter

When a single derivation must produce two different output types (e.g., `Either[E, A]` for
fail-fast and `ValidatedNel[E, A]` for error accumulation), add a runtime boolean parameter
to the cached def:

```scala
// Single cached def handles both paths:
// def decode_MyType(cursor: HCursor, config: Configuration, failFast: Boolean): Any
dctx.setHelper[A] { (cursor, config, failFast) =>
  decodeCaseClassFields[A](caseClass)(using dctx.nestInCache(cursor, config, failFast))
    .map { (ffExpr, accExpr) =>
      Expr.quote {
        (if (Expr.splice(failFast)) Expr.splice(ffExpr) else Expr.splice(accExpr)): Any
      }
    }
}

// Callers pass true/false:
// In apply():             helper(cursor, config, true).asInstanceOf[Either[E, A]]
// In decodeAccumulating(): helper(cursor, config, false).asInstanceOf[ValidatedNel[E, A]]
```

**Why not two cached defs?** Doubles generated code size and duplicates field decoder
derivations.

**Why not `LambdaBuilder`?** Adds runtime allocation overhead.

**Why not two `Expr.splice` blocks?** Sibling splice isolation on Scala 3 -- see
[hearth-cross-compilation](../hearth-cross-compilation/SKILL.md).

## ValDefsCache key collisions with `Any` return type

When changing a cached def's return type to `Any`, `ValDefsCache` uses a composite key:
`(String name, Seq[UntypedType] args, UntypedType returned)`. If the return type is `Any`
for ALL types, the string key alone must disambiguate.

```scala
// BROKEN -- all types share the same cache key:
cache.forwardDeclare("cached-decode-method", defBuilder)

// CORRECT -- type-specific string key prevents collisions:
private def helperCacheKey[B: Type]: String = s"cached-decode-method:${Type[B].prettyPrint}"
cache.forwardDeclare(helperCacheKey[B], defBuilder)
```

**Reference:** `DecoderMacrosImpl.scala` -- `helperCacheKey`, `getHelper`, `setHelper`.

## `directDecoderOpt` pattern

In `deriveDecoderTypeClass`, the entry point builds a `KindlingsDecoder[A]` that delegates
to the cached helper. But some types (value types, options, collections, maps) don't create
cached helpers -- they produce inline expressions.

For these types, build a `Decoder[A]` via `LambdaBuilder` with a fresh derivation context:

```scala
val directDecoderOpt: Option[Expr[Decoder[A]]] =
  if (helperOpt.isDefined) None
  else {
    Some(runSafe {
      LambdaBuilder.of1[HCursor]("directCursor").traverse { cursorExpr =>
        val freshCtx = DecoderCtx.from[A](cursorExpr, configExpr, Expr.quote(true),
          derivedType = selfType)
        for {
          _ <- Environment.loadStandardExtensions().toMIO(allowFailures = false)
          result <- deriveDecoderRecursively[A](using freshCtx)
          freshCache <- freshCtx.cache.get
        } yield freshCache.toValDefs.use(_ => result)
      }.map { builder =>
        val decodeFn = builder.build[Either[DecodingFailure, A]]
        Expr.quote(CirceDerivationUtils.decoderFromFn(Expr.splice(decodeFn)))
      }
    })
  }
```

**Why not just inline the expression?** Constructing fallback error expressions inside
nested quotes fails on Scala 2 with reification errors. The `LambdaBuilder` approach
builds the expression in a clean scope.

**Reference:** `DecoderMacrosImpl.scala` lines 84-103.
