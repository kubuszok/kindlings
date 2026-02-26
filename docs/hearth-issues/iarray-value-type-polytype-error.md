# IArray + IsValueType: `PolyType` error on Scala 3

## Summary

`IsValueTypeProviderForOpaque` matches `IArray[T]` on Scala 3 because `IArray` is an opaque type. The **encoder path** (unwrap) works fine, but the **decoder path** (wrap) crashes with a Scala 3 compiler error:

```
Expected `fun.tpe` to widen into a `PolyType`
```

## Current workaround

Every `HandleAsValueTypeRule` in Kindlings (9 occurrences across all modules) has this guard:

```scala
case _ if Type[A].isIArray =>
  MIO.pure(Rule.yielded(s"The type ${Type[A].prettyPrint} is IArray, handled as collection instead"))
```

This skips the `IsValueType` extractor for `IArray`, letting it fall through to `IsCollectionProviderForIArray` which handles it correctly.

## Reproduction

### Setup

- Hearth `0.2.0-235-g8dd0a9f-SNAPSHOT`
- Scala 3.7.4

### Steps

1. Remove the `isIArray` guard from `DecHandleAsValueTypeRule` in any decoder module
2. Compile a test that uses `IArray[Int]` in the decoder:

```scala
import io.circe.Json

val json = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
val decoded = KindlingsDecoder.decode[IArray[Int]](json)
```

3. Compilation fails with:

```
Macro derivation failed with the following errors:
  - Expected `fun.tpe` to widen into a `PolyType`
```

### Debug log (with `import hearth.kindlings.circederivation.debug.logDerivationForKindlingsDecoder`)

```
KindlingsDecoder.decode:
└ Deriving decoder for scala.IArray$package.IArray[scala.Int]:
    ├ [Info]  Successfully loaded 34 extensions:
    │   ...
    │   - hearth.std.extensions.IsCollectionProviderForIArray
    │   - hearth.std.extensions.IsValueTypeProviderForOpaque
    │   ...
    └ Deriving decoder for type scala.IArray$package.IArray[scala.Int]:
      ├ [Info]  Attempting to use cached decoder for scala.IArray$package.IArray[scala.Int]
      ├ [Info]  Attempting to handle scala.IArray$package.IArray[scala.Int] as a literal type
      ├ [Info]  Attempting to use implicit Decoder for scala.IArray$package.IArray[scala.Int]
      └ [Info]  Attempting to handle scala.IArray$package.IArray[scala.Int] as a value type
                ^--- crashes here
```

### Encoder works without workaround

Removing the `isIArray` guard from the **encoder** does NOT cause any failure. The encoder's `IsValueType` match:
1. Unwraps `IArray[Int]` → `Array[Int]` (via `isValueType.value.unwrap`)
2. Recursively derives encoder for `Array[Int]` (matched by `IsCollectionProviderForArray`)
3. Produces correct JSON array output

## Root cause analysis

`IsValueTypeProviderForOpaque` (Scala 3 only) matches `IArray[Int]`:
- `tpe.isOpaqueType` returns `true` for `IArray` (it's `opaque type IArray[+T] = Array[T & AnyRef]` in Scala 3)
- It finds the underlying type `Array[Int]`
- It finds a smart constructor via `CtorLikes.unapply` — likely `IArray.apply[T: ClassTag](xs: T*)` or similar
- It creates a `wrap: CtorLikeOf[Array[Int], IArray[Int]]` using this constructor

The **unwrap** works because it's a simple `asInstanceOf` cast (opaque types are erased).

The **wrap** fails because the smart constructor is polymorphic (`[T: ClassTag]`) and when Hearth tries to generate the wrapping code (calling the constructor with a concrete `Array[Int]` value), the Scala 3 compiler rejects the generated tree with `Expected 'fun.tpe' to widen into a 'PolyType'`.

## Affected files (workaround locations)

| Module | File | Rule |
|--------|------|------|
| Circe | `EncoderMacrosImpl.scala` | `EncHandleAsValueTypeRule` |
| Circe | `DecoderMacrosImpl.scala` | `DecHandleAsValueTypeRule` |
| Jsoniter | `CodecMacrosImpl.scala` | `EncHandleAsValueTypeRule` |
| Jsoniter | `CodecMacrosImpl.scala` | `DecHandleAsValueTypeRule` |
| YAML | `EncoderMacrosImpl.scala` | `EncHandleAsValueTypeRule` |
| YAML | `DecoderMacrosImpl.scala` | `DecHandleAsValueTypeRule` |
| Avro | `SchemaForMacrosImpl.scala` | `SfHandleAsValueTypeRule` |
| Avro | `EncoderMacrosImpl.scala` | `EncHandleAsValueTypeRule` |
| Avro | `DecoderMacrosImpl.scala` | `DecHandleAsValueTypeRule` |

## Suggested fix options

### Option A: Skip IArray in IsValueTypeProviderForOpaque

Add an `isIArray` check to `IsValueTypeProviderForOpaque.parse`:

```scala
override def parse[A](tpe: Type[A]): ProviderResult[IsValueType[A]] =
  if !tpe.isOpaqueType then skipped(...)
  else if tpe.isIArray then skipped(s"${tpe.prettyPrint} is IArray, handled as collection")
  else { ... }
```

This is the simplest fix and moves the workaround from every consumer (kindlings) to the single provider (hearth).

### Option B: Fix the CtorLike for polymorphic constructors

The `wrap` `CtorLikeOf` generated for `IArray` uses a polymorphic constructor that the Scala 3 compiler rejects. If the `CtorLike` infrastructure could correctly handle constructors with type parameters and context bounds (like `ClassTag`), the issue would be resolved generically for all opaque types with polymorphic constructors.

### Option C: Priority-based provider ordering

If `IsCollectionProviderForIArray` could be checked **before** `IsValueTypeProviderForOpaque`, the issue would be avoided. But currently the provider registration order determines priority, and `IsValueType` is checked in the rule chain before `IsCollection`.
