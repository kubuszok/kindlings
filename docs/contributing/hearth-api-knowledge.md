# Hearth API Quick Reference

Living cache of hearth API signatures used in kindlings. **Always verify with `kindlings-metals` MCP before use.**

**Last verified against hearth version:** check `build.sbt` `versions.hearth`

## Effect system (MIO, Log)

| API | Signature | Description |
|-----|-----------|-------------|
| `MIO.pure` | `[A](a: A): MIO[A]` | Lift a pure value |
| `MIO.fail` | `(error: Throwable): MIO[Nothing]` | Fail with error |
| `MIO { ... }` | `[A](thunk: => A): MIO[A]` | Lift a thunk |
| `mio1 >> mio2` | `MIO[A] >> MIO[B]: MIO[B]` | Sequence, discard first result |
| `mio.flatMap(f)` | `MIO[A].flatMap(A => MIO[B]): MIO[B]` | Monadic bind |
| `mio.map(f)` | `MIO[A].map(A => B): MIO[B]` | Functor map |
| `MIO.scoped` | `[A](f: (MIO ~> Id) => A): MIO[A]` | Unsafe escape hatch for builders |
| `Log.info` | `(msg: => String): MIO[Unit]` | Log at info level |
| `Log.debug` | `(msg: => String): MIO[Unit]` | Log at debug level |
| `Log.namedScope` | `[A](name: String)(body: MIO[A]): MIO[A]` | Hierarchical log scope |

## Type representations

| API | Signature | Description |
|-----|-----------|-------------|
| `Type.of[A]` | `[A](implicit ev: Type[A]): Type[A]` | Get type representation |
| `Type[A].prettyPrint` | `: String` | Human-readable type name |
| `Type[A].shortName` | `: String` | Short type name (no package) |
| `Type[A] <:< Type[B]` | `: Boolean` | Subtype check |
| `Type[A] =:= Type[B]` | `: Boolean` | Type equality |
| `Type[A].summonExpr` | `: ImplicitSearchResult[A]` | Summon implicit |
| `Type[A].summonExprIgnoring` | `(symbols: UntypedMethod*): ImplicitSearchResult[A]` | Summon ignoring symbols |
| `Type[A].methods` | `: List[Existential[Method]]` | List methods on type |
| `Type.Ctor1.of[F]` | Higher-kinded type constructor | For `F[_]` types |

## Expression manipulation

| API | Signature | Description |
|-----|-----------|-------------|
| `Expr(value)` | `[A](value: A)(implicit Liftable[A]): Expr[A]` | Lift literal value |
| `Expr.quote { ... }` | `[A](body: => A): Expr[A]` | Cross-platform quasiquote |
| `Expr.splice(expr)` | `[A](expr: Expr[A]): A` | Splice inside quote |
| `expr.prettyPrint` | `: String` | String representation of tree |
| `expr.upcast[B]` | `[B](implicit ev: A <:< B): Expr[B]` | Widen type (not narrow!) |
| `expr.as_??` | `: Expr_??` | Wrap into existential |

## Existentials (`Expr_??`, `??`)

| API | Signature | Description |
|-----|-----------|-------------|
| `expr.as_??` | `Expr[A] => Expr_??` | Erase concrete type |
| `Type[A].as_??` | `Type[A] => ??` | Erase concrete type from Type |
| `import e.Underlying` | Brings `Type[Underlying]` into scope | Pattern: `import e.{Underlying as T, value as v}` |
| `import e.value` | Brings typed expression into scope | Used with `Underlying` alias |

## Lambda and val builders

| API | Signature | Description |
|-----|-----------|-------------|
| `LambdaBuilder.of1[A]` | `(name: String): LambdaBuilder1[A]` | 1-arg lambda builder |
| `LambdaBuilder.of2[A, B]` | `(name1: String, name2: String): LambdaBuilder2[A, B]` | 2-arg lambda builder |
| `LambdaBuilder.of3[A, B, C]` | `(name1: String, name2: String, name3: String): LambdaBuilder3[A, B, C]` | 3-arg lambda builder |
| `builder.traverse` | `(f: Expr[A] => MIO[Expr[B]]): MIO[Built1[A, B]]` | Build with MIO body |
| `builder.buildWith` | `(f: Expr[A] => Expr[B]): Expr[A => B]` | Build with pure body |
| `built.build[B]` | `: Expr[A => B]` | Finalize lambda |
| `ValDefBuilder.ofLazy[T]` | `(name: String): ValDefBuilder` | `lazy val` builder |
| `ValDefBuilder.ofDef1[A, B]` | `(name: String, argName: String): ValDefBuilder` | `def` with 1 arg |
| `ValDefBuilder.ofDef2[A, B, C]` | `(name: String, argName1: String, argName2: String): ValDefBuilder` | `def` with 2 args |
| `ValDefBuilder.ofDef3[A, B, C, D]` | `(name: String, a1: String, a2: String, a3: String): ValDefBuilder` | `def` with 3 args |
| `ValDefsCache.mlocal` | `: MLocal[ValDefsCache]` | Create cache storage |
| `cache.get0Ary[T]` | `(key: String): MIO[Option[Expr[T]]]` | Get cached value |
| `cache.get1Ary[A, B]` | `(key: String): MIO[Option[Expr[A] => Expr[B]]]` | Get cached function |
| `cache.get2Ary[A, B, C]` | `(key: String): MIO[Option[(Expr[A], Expr[B]) => Expr[C]]]` | Get cached 2-arg function |
| `cache.get3Ary[A, B, C, D]` | `(key: String): MIO[Option[(Expr[A], Expr[B], Expr[C]) => Expr[D]]]` | Get cached 3-arg function |
| `cache.buildCachedWith` | `(key: String, builder: ValDefBuilder)(body: ...): MIO[...]` | Build and cache |
| `cache.forwardDeclare` | `(key: String, builder: ValDefBuilder): MIO[Unit]` | Forward-declare for recursion |
| `cache.get` | `: MIO[ValDefsCache]` | Extract accumulated cached defs |
| `vals.toValDefs.use` | `(f: ... => Expr[A]): Expr[A]` | Wrap expr in cached val defs block |

## Data model introspection

| API | Signature | Description |
|-----|-----------|-------------|
| `CaseClass.parse[A]` | `: Option[CaseClass[A]]` | Parse type as case class |
| `cc.caseFieldValuesAt(expr)` | `: ListMap[String, Expr_??]` | Read field values (encoder) |
| `cc.primaryConstructor(map)` | `(Map[String, Expr_??]): Either[String, Expr[A]]` | Construct from field map (decoder) |
| `cc.construct[F](f)` | `(ConstructField[F] => F[Option[Expr[A]]])` | Construct via callback (Scala 2 pitfall!) |
| `Enum.parse[A]` | `: Option[Enum[A]]` | Parse type as enum/sealed trait |
| `enum.parMatchOn[F, R]` | `(expr)(handler): F[Expr[R]]` | Exhaustive pattern match |
| `param.tpe` | `: Existential[Type]` | Parameter's type (existential) |
| `param.index` | `: Int` | Parameter position index |

## Type extractors (std extensions)

| API | Pattern | Description |
|-----|---------|-------------|
| `IsCollection` | `Type[A] match { case IsCollection(ic) => ... }` | Detect collection types |
| `IsMap` | `Type[A] match { case IsMap(im) => ... }` | Detect map types |
| `IsOption` | `Type[A] match { case IsOption(io) => ... }` | Detect optional types |
| `IsValueType` | `Type[A] match { case IsValueType(ivt) => ... }` | Detect value classes |

## Rules

| API | Signature | Description |
|-----|-----------|-------------|
| `Rule.matched(value)` | `: Rule.Applicability[A]` | Rule applies with result |
| `Rule.yielded(reason)` | `: Rule.Applicability[Nothing]` | Rule does not apply |
| `Rules(r1, r2, ...)` | `(f): MIO[Either[reasons, A]]` | Try rules in order, first match wins |

## Known gotchas

| Gotcha | Details | Workaround |
|--------|---------|------------|
| Path-dependent types in `Expr.quote` | `import param.tpe.Underlying as F; Expr.quote { x.asInstanceOf[F] }` fails on Scala 2 | Use `LambdaBuilder.of1[F]` or runtime type witness |
| Macro-internal types leak | `??`, `Expr_??` inside `Expr.quote` causes reification failures on Scala 2 | Extract to `val` before `Expr.quote` |
| `Array` needs `ClassTag` | `Array.empty[T]` inside `Expr.quote` fails on Scala 2 | Use `List.empty[T]` and `::` |
| `upcast` only widens | `expr.upcast[B]` requires `A <:< B`; also needs `Type[A]` in scope | Use `.asInstanceOf` for narrowing |
| Raw quotes in `LambdaBuilder` | `'{ }` captures wrong `Quotes` on Scala 3 | Use `Expr.quote`/`Expr.splice` |
| Generic macro wrapper | `def helper[A](v: A)` calling macro sees abstract `A` | Call macros with concrete types |
| Sibling splice isolation | Each `Expr.splice` in an `Expr.quote` gets its own `Quotes` context on Scala 3; types/exprs can't be shared between sibling splices | Derive everything in one `runSafe`, use `LambdaBuilder` to package as function `Expr`s, then splice references only |
| `IsMap`/`IsCollection` types need import | `isMap.Key`, `isMap.Value`, `isMap.CtorResult` need `Type` in scope | `import isMap.{Key, Value, CtorResult}` before `Expr.quote` |
| `toValDefs.use` wrapping scope | Wrapping individual lambdas with all cached defs causes unused-method warnings | Wrap the outermost expression that contains all references |
