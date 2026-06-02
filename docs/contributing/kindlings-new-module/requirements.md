# Implementation Requirements and Test Coverage

Full checklist for new Kindlings derivation modules. See [SKILL.md](SKILL.md) for the
overview.

## REQ-1: Dual entry points -- type class instance AND inlined body

The macro must provide **two** entry points:

1. **`deriveTypeClass[A]`** -- returns `Expr[MyTypeClass[A]]`, creating a new instance
2. **`deriveInline[A]`** -- returns the result type directly (e.g., `Expr[String]`),
   embedding derivation logic inline without allocating a type class instance

Both entry points delegate to the same core derivation logic. The difference is only in
how the result is wrapped.

**Reference:** `FastShowPrettyMacrosImpl.scala`, `DecoderMacrosImpl.scala` lines 17-64.

**Verification:**
- Confirm both methods exist in the macro impl
- Confirm Scala 2 and Scala 3 bridge files expose both entry points
- Confirm the public companion has both `derived` and an inline method
- Write tests for both

## REQ-2: Recursive derivation with caching

The macro must handle nested structures **recursively**, inlining derivation logic rather
than allocating intermediate type class instances.

### REQ-2a: Case class and enum derivation cached as defs

Derived logic for case classes and enums must be **cached as local `def`s**. This:
- Keeps generated code compact (avoids JVM method size limits)
- Improves compilation speed (derive once, call many times)
- Handles recursive data types automatically

**How to implement:**
1. In the context class, provide `getHelper[B]` / `setHelper[B]` wrapping
   `cache.getNAry` / `cache.buildCachedWith` with `ValDefBuilder.ofDefN[...]`
2. At the start of case class / enum rules, call `ctx.setHelper[A]`
3. Use `MIO.scoped { runSafe => ... }` inside the builder

**Reference:** `FastShowPrettyMacrosImpl.scala` (`getHelper`/`setHelper`),
`FastShowPrettyHandleAsCaseClassRule.scala`, `FastShowPrettyHandleAsEnumRule.scala`.

**Verification:**
- Recursive data type (e.g., `case class Tree(children: List[Tree])`) compiles and works
- Derivation of deeply nested types completes quickly

## REQ-3: Implicit resolution

### REQ-3a: Always prefer user-provided implicits

The `UseImplicitWhenAvailableRule` must appear **before** built-in, case class, and enum
rules (but after the cache rule).

### REQ-3b: Ignore self-summoning implicits

Collect method symbols to ignore and pass to `Type[TC[A]].summonExprIgnoring(symbols*)`.

**Critical failure mode -- OOM:** If you use `Expr.summonImplicit[TC[A]]` without ignoring,
and the target library provides auto-derivation, the compiler enters infinite macro
expansion causing `OutOfMemoryError`. **Always** use `summonExprIgnoring`.

### REQ-3c: Subtype derivation -- look for parent type implicits

When the type class is a subtype (e.g., `KindlingsDecoder <: circe.Decoder`):
1. Summon implicits of the **parent type**
2. Ignore automatic derivation methods from the parent
3. Ignore companion-provided built-in instances from the parent

**Reference:** `DecoderMacrosImpl.scala` lines 276-284.

### REQ-3d: Cache resolved implicits as lazy vals

Every successfully resolved implicit must be cached via
`ctx.cache.buildCachedWith("instance", ValDefBuilder.ofLazy[TC[A]]("instance"))`.

### REQ-3e: Ignore library companion implicits for features handled by macro rules

Whatever a library handles via companion implicits, the macro should handle instead --
and ignore all those companion-provided implicits:

1. Automatic derivation implicits (`derived`, `apply`, `generic.auto._`)
2. Literal type implicits (`decodeLiteralString`, `encodeLiteralInt`, etc.)
3. Any other companion-provided instances for types handled by dedicated rules

```scala
lazy val ignoredImplicits: Seq[UntypedMethod] = {
  val ours = Type.of[KindlingsDecoder.type].methods.collect {
    case method if method.value.name == "derived" => method.value.asUntyped
  }
  val libraryCompanion = Type.of[Decoder.type].methods.collect {
    case method if method.value.name == "derived"
      || method.value.name.startsWith("decodeLiteral") =>
      method.value.asUntyped
  }
  ours ++ libraryCompanion
}
```

## REQ-4: Handle built-in/primitive types without allocation

Built-in types (Boolean, Byte, Short, Int, Long, Float, Double, Char, String) must be
handled **directly** by generating inline expressions or delegating to non-allocating
runtime utilities.

**Reference:** `FastShowPrettyUseBuiltInSupportRule.scala`, `FastShowPrettyUtils.scala`.

## REQ-5: Handle value types via std extensions

Guard against named tuples first:
```scala
def apply[A: Ctx]: MIO[Rule.Applicability[Expr[Result]]] =
  if (Type[A].isNamedTuple)
    MIO.pure(Rule.yielded(s"${Type[A].prettyPrint} is a named tuple"))
  else Type[A] match {
    case IsValueType(isValueType) => // ...
    case _ => MIO.pure(Rule.yielded(...))
  }
```

**Reference:** `FastShowPrettyHandleAsValueTypeRule.scala`.

## REQ-6: Handle optional types via std extensions

Pattern-match against `IsOption` from hearth's std extensions. Generate code that checks
for `None`/`Some` and recurses on the inner type.

## REQ-7: Handle collection types via std extensions

Create `HandleAsCollectionRule` (against `IsCollection`) and `HandleAsMapRule` (against
`IsMap`). Cache item/key/value derivation as a def. Use `LambdaBuilder` for iteration
callbacks.

## REQ-8: Log all derivation steps in MIO

Every rule attempt, match, failure, cache hit/miss, and recursive descent must be logged
via `Log.info(...)` or `Log.namedScope(...)`.

## REQ-9: Configurable logging

### REQ-9a: Import-based logging
```scala
package object debug {
  implicit val logDerivationForMyTypeClass: hearth.LogDerivation = hearth.LogDerivation.Enabled
}
```

### REQ-9b: Scalac option logging
```
-Xmacro-settings:myModule.logDerivation=true
```

## REQ-10: Error aggregation and reporting

All rule failure reasons must be collected and reported together. The error message must
include: the type that failed, why each rule was not applicable, and a hint for enabling
debug logging.

## REQ-11: Comprehensive test coverage

| Test category | What to test | Validates |
|---|---|---|
| **Built-in types** | Boolean, Byte, Short, Int, Long, Float, Double, Char, String | REQ-4 |
| **Value types** | `case class Wrapper(value: Int) extends AnyVal` | REQ-5 |
| **Optionals** | `Option[Int]`, `Option[CaseClass]`, `None`, `Some`, nested | REQ-6 |
| **Collections** | `List[Int]`, `Vector[String]`, `Set[CaseClass]`, nested | REQ-7 |
| **Maps** | `Map[String, Int]`, `Map[String, CaseClass]` | REQ-7 |
| **Case classes** | Zero-field, single-field, multi-field, nested | REQ-2 |
| **Enums / sealed traits** | Case objects, case classes, nested | REQ-2 |
| **Recursive types** | `case class Tree(children: List[Tree])` | REQ-2a |
| **User-provided implicits** | Manual instance overrides derived one | REQ-3a |
| **Inline entry point** | Call the inline method directly | REQ-1 |
| **Type class entry point** | Use `derived` or summon the type class | REQ-1 |
| **Error messages** | `compileErrors(...)` on unsupported types | REQ-10 |
| **Subtype implicits** | Parent type implicit is picked up | REQ-3c |
| **Parent auto-derivation ignored** | Parent's auto-derivation does not interfere | REQ-3c |
| **Scala 3-only types** | Named tuples, Scala 3 enums -- in `src/test/scala-3/` | REQ-11c |

### REQ-11a: Recursive types must work without additional tricks

```scala
case class Tree(value: Int, children: List[Tree])
test("recursive data type works transparently") {
  val tree = Tree(1, List(Tree(2, Nil), Tree(3, List(Tree(4, Nil)))))
  val result = MyTypeClass.someMethod(tree)
  assertEquals(result, /* expected */)
}
```

### REQ-11b: Parent automatic derivation is ignored (subtype type classes only)

Verify the result is produced by **our** derivation, not the parent's auto-derivation.

### REQ-11c: Scala 3-only types tested in Scala 3-only tests

Named tuple testing must include single-element tuples (`(field: Int)`).

## REQ-12: Suppress compiler warnings in generated code

The macro-generated code must not produce warnings at the expansion site. Users should
never need `@nowarn` annotations.

Run `sbt --client "yourModule/clean ; yourModule3/clean ; test-jvm-2_13 ; test-jvm-3"` and
verify zero warnings from macro-expanded code.

## Test file organization

```
my-type-class/src/test/
+-- scala/hearth/kindlings/mytypeclass/
|   +-- examples.scala                    # Shared test data types (Scala 2 + 3)
|   +-- MyTypeClassSpec.scala             # Cross-compiled tests
+-- scala-2/hearth/kindlings/mytypeclass/
|   +-- MyTypeClassScala2Spec.scala       # Scala 2-only tests (if needed)
+-- scala-3/hearth/kindlings/mytypeclass/
    +-- scala3examples.scala              # Scala 3-only types (enums, opaque types)
    +-- MyTypeClassScala3Spec.scala       # Scala 3-only tests
```

Test data types are defined in separate `examples.scala` files, not inline in spec files.

**Run tests with:**
```bash
sbt --client "yourModule/clean ; yourModule3/clean ; test-jvm-2_13 ; test-jvm-3"
```

Do NOT use `++2.13.18` or `++3.8.2` to switch versions.

## Test coverage checklist

- [ ] Primitive types (Boolean, Byte, Short, Int, Long, Float, Double, Char, String)
- [ ] Case classes (empty, single field, nested, with collections)
- [ ] Value classes (AnyVal) -- unwrapped
- [ ] Options (Some, None)
- [ ] Collections (List, Vector, Set, Array, Seq)
- [ ] Maps (Map[String, V], empty map)
- [ ] Sealed traits / enums -- wrapper-style and discriminator-style
- [ ] Sealed traits with case object singletons
- [ ] Tuples (Tuple2, Tuple3)
- [ ] Recursive data structures
- [ ] Opaque types (Scala 3 only)
- [ ] Named tuples (Scala 3 only)
- [ ] Scala 3 enums (parameterless + parameterized)
- [ ] All configuration options with all name transform variants
- [ ] Custom implicit priority (user-provided instances override derivation)
- [ ] Compile-time error messages for unsupported types
- [ ] Error messages enriched with diagnostics
- [ ] Behavior parity with original library (if reimplementing)

## Error types

Each module defines a sealed error trait hierarchy at the bottom of its `MacrosImpl.scala`.
All error traits extend `util.control.NoStackTrace` and have a `message` field.

### Error trait hierarchy per module

| Module | Sealed Trait | Case Classes |
|--------|-------------|--------------|
| fast-show-pretty | `DerivationError` | `UnsupportedType`, `NoChildrenInSealedTrait` |
| circe encoder | `EncoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait` |
| circe decoder | `DecoderDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `CannotConstructType` |
| jsoniter codec | `CodecDerivationError` | `UnsupportedType`, `TransientFieldMissingDefault`, `NoChildrenInSealedTrait`, `CannotConstructType`, `UnexpectedParameterInSingleton` |

### Error fail pattern

All error sites follow the `Log.error >> MIO.fail` pattern:
```scala
val err = XxxDerivationError.SomeCase(...)
Log.error(err.message) >> MIO.fail(err)
```

### Conditional errorRendering

```scala
errorRendering = if (shouldWeLogXxx) RenderFrom(Log.Level.Info) else DontRender
```

Prevents error log trees from being rendered when debug logging is disabled.
