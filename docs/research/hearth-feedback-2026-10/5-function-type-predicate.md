# No public predicate/extractor for function and context-function types

**Verdict:** confirmed gap (an API request, not a bug). There is no public `isFunction`/`isContextFunction` and no arity
extractor. The only tools are a `Type.CtorN.of[FunctionM]` per arity, or `fqcn` string matching, which is wrong for
subclasses and for `ContextFunctionN`.

**Platforms:** both. The context-function part is Scala 3 only.

## What exists

- `Type.isTuple`, `isNamedTuple`, `isUnionType`, `isArray`, … but nothing for functions (checked the
  `Types.scala` / `UntypedTypes.scala` `is*` lists).
- Hearth has an internal `isContextFunctionType` (`UntypedTypesScala3.scala:490`, `private`, used for
  context-function-returning methods). It is not exposed.
- `Type.Ctor2.of[Function1].unapply` works (see item 7), but only for one arity at a time. Covering 0..22 plus
  `ContextFunction0..22` means 46 constructors.

## Reproducer

`KindlingsFeedbackFixturesImpl.testFunctionClassification[A]`. The tests pass, and they document current behaviour:

| `A` | `fqcn` | `fqcn.startsWith("scala.Function")` | `Ctor2.of[Function1].unapply` | `<:< Function1[Nothing, Any]` |
|---|---|---|---|---|
| `Int => String` | `scala.Function1` | true | defined | true |
| `IntPredicate extends (Int => Boolean)` | `...IntPredicate` | **false** (misses it) | defined | true |
| `Int ?=> String` (Scala 3, `KindlingsFeedbackScala3Spec`) | `scala.ContextFunction1` | **false** (misses it) | **None** | **false** |

Output: `KindlingsFeedbackScala3Spec finished: 0 failed, 1 total`. The two shared tests also pass on 2.13 and 3.

kindlings di uses `Type.fqcn[D].startsWith("scala.Function")` to detect "dependency is a factory function". It misses
context functions and user subclasses of `FunctionN`.

## Proposed API

```scala
Type.isFunction[A]: Boolean            // A <: FunctionN for some N (Scala 3: also FunctionXXL?)
Type.isContextFunction[A]: Boolean     // Scala 3 ContextFunctionN; always false on Scala 2
// and/or an extractor in the std-extension style:
Type.FunctionType.unapply[A]: Option[(List[??], ??)]          // params, result (via baseType, so subclasses work)
Type.ContextFunctionType.unapply[A]: Option[(List[??], ??)]
```

Scala 3 has `TypeRepr.isFunctionType`, `isContextFunctionType` (public in `quotes.reflect`) and `defn.FunctionClass(n)`.
Scala 2 has `definitions.FunctionClass.seq` / `isFunctionType`. Both are cheap.
