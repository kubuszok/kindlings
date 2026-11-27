# Feature Parity: Kindlings vs Original Libraries

This document tracks feature parity between Kindlings derivation modules and the original libraries they replace, as well as UX improvements Kindlings provides.

Legend: **Parity** = feature matches original, **Improvement** = Kindlings does it better, **Gap** = feature not yet implemented.

**Order of the subtypes of a sealed hierarchy** (wherever it is observable: schema symbols and subtypes, the order
subtypes are tried in, generators, `Order` across subtypes, names listed in errors): on Scala 3 the declaration order
(the order `Mirror.SumOf` uses, as upstream's Scala 3 derivations); on Scala 2 the subtypes sorted by name (Hearth's
`ChildrenOrder.ByName`: the simple name in natural order, then the full name), the same in every build - Scala 2 class
files do not record the declaration order, so it is unknown for a sealed type from another module or one an incremental
build did not recompile. Upstream Scala 2 derivations (shapeless, Magnolia) sort by the full name; the two orders agree
except for numbered names (`Item2` before `Item10` vs after it) and subtypes declared in different scopes. Java enum
constants keep their declaration order.

**Collections typed as an abstract collection type** (every module that builds collections: decoders, generators,
`Monoid`/`Empty` and the like): a collection is built with the `scala.collection.Factory` that Scala's implicit search
finds for the declared type. When the *element* type is itself a collection which is a subtype of the declared type,
the element type's companion is in the implicit scope too and its more specific factory wins:
`mutable.Map[String, mutable.ListMap[String, Int]]` is built as a `mutable.ListMap`,
`mutable.Map[String, mutable.LinkedHashMap[String, Int]]` as a `mutable.LinkedHashMap` and
`mutable.IndexedSeq[mutable.ArrayStack[Int]]` as a `mutable.Stack`; with immutable or non-collection elements the
declared type's default is used (`mutable.HashMap`, `ArrayBuffer`). Upstream libraries which map each abstract
collection type to a fixed implementation (jsoniter-scala) build the declared type's default there. To get a specific
implementation, declare that type (`mutable.HashMap[String, mutable.ListMap[String, Int]]`).

---

## avro-derivation

**Replaces:** `com.sksamuel.avro4s` (v4 for Scala 2, v5 for Scala 3 — separate incompatible versions)

Checked against avro4s by the `upstream-oracle-tests` module (`AvroOracleSpec`, `scala-3/AvroScala3OracleSpec`): each
Scala version is compared with its own avro4s line (4.x on Scala 2.13, 5.x on Scala 3) — the schemas, the binary
encoding of sample values, each side decoding the other's bytes, hand-built and invalid inputs, and schema evolution.
The only differences are the ones documented below ("Design decisions" and "Default value differences"); the
`AvroConfig.avro4sDefaults` preset switches every default value difference to avro4s' behaviour.

### Design decisions

| Difference | avro4s | Kindlings | Why |
|---|---|---|---|
| Tuple record names | `scala.Tuple2` for every `(A, B)` | `scala.Tuple2__A_B` (the type arguments, as for any generic type) | With avro4s two different tuple types in one schema get the same name, so the schema's JSON describes the second one wrongly |
| `@AvroEnumDefault` on avro4s 5 | Ignored (never implemented in 5.x) | `@avroEnumDefault` sets the ENUM default (as avro4s 4) on both Scala versions | Superset |
| `@AvroScalePrecision` on avro4s 5 | Declared but ignored (the implicit `ScalePrecision` applies) | `@avroScalePrecision(precision, scale)` is honoured on both Scala versions | Superset |
| avro4s 5 gaps | No recursive types, value classes, `scala.Enumeration`, working `FieldMapper` | All supported (compared with avro4s 4 on Scala 2.13) | Superset |
| `@AvroProp` with a `JsonNode` (avro4s 5) | avro4s 5's `@AvroProp` also takes a `JsonNode` value | `@avroProp` values are Strings, as avro4s 4's, on both Scala versions | Follows avro4s 4; annotation arguments must be literals |
| Default of a decimal (avro4s bug) | The schema default of a `BigDecimal` field is written as its decimal string (`"964.55"`), which Avro reads as the bytes of those characters, i.e. another number | The bytes of the unscaled value, as the Avro specification requires for a `bytes` default | avro4s' default is wrong for every Avro reader; Kindlings writes the spec's form (or no default: `withoutScalaDefaultsInSchema`) |
| Record defaults (avro4s 4 bug) | `None` fields are left out of a case class default (`{"name": "x"}`) | The complete record (`{"name": "x", "age": null}`), as avro4s 5 | Avro rejects the incomplete default when it is needed |
| Sealed trait's `@AvroNamespace` (avro4s 4) | Applied to the subtypes only as members of the trait's union; a subtype derived on its own keeps its package | A subtype without its own `@avroNamespace` always takes its sealed parent's (as avro4s 5) | The encoder builds a subtype's record from the subtype's own schema: it must have the name it has in the union |
| Sealed trait with one subtype (avro4s 5 differs) | avro4s 5: the subtype's record, without a union (avro4s 4: a union of one) | A union of one, as avro4s 4, on both Scala versions | Follows avro4s 4: one wire format on both Scala versions; adding a second subtype stays a compatible change |
| Decimal rounding (avro4s 5 differs) | avro4s 5 always rounds a value with more decimal places than the scale `HALF_UP` (avro4s 4: an implicit `RoundingMode`, default `UNNECESSARY`, i.e. rejected) | `AvroConfig.withRoundingMode(mode)` (every decimal, also `@avroScalePrecision` fields without a `DecimalConfig`), `withDecimalConfig(precision, scale, roundingMode)` or per field `@avroScalePrecision(precision, scale, roundingMode)`; default `UNNECESSARY`, as avro4s 4, on both Scala versions | Follows avro4s 4: no silent loss of precision by default |
| avro4s bugs not reproduced | avro4s 4 cannot encode an `Option` below a field `@AvroNamespace`, decodes only the `Left` of `Either[A, Option[B]]`; avro4s 5 cannot encode a sealed trait below a field `@AvroNamespace`, `Either[A, Option[B]]` or a `None.type` field, ignores `@AvroName` on type arguments, decodes the `Right` of `Either[Long, Int]` as a `Left` | All work | Superset |

### Default value differences

| Setting | avro4s | Kindlings default | Opt in to avro4s' behaviour |
|---|---|---|---|
| `BigDecimal` | BYTES + `decimal(precision = 8, scale = 2)` | STRING | `withDecimalConfig(8, 2)` or `avro4sDefaults` |
| `OffsetDateTime` | ISO-8601 STRING with the `datetime-with-offset` logical type | `timestamp-millis` LONG (UTC; the offset is dropped) | `withOffsetDateTimeAsIsoString` or `avro4sDefaults` |

Scala default values become schema defaults, as in avro4s (the default's union branch moved first), written as the
Avro specification requires (a `None` as `null` with the null branch first, a decimal as the bytes of its unscaled
value). The opt-out `withoutScalaDefaultsInSchema` (or `withScalaDefaultsInSchema(false)`) keeps only `@avroDefault`
defaults; Avro's schema resolution then rejects old data read
with a newer reader schema that adds fields with Scala defaults (`missing required field`), while decoding a
`GenericRecord` directly still falls back to the Scala defaults.

### Configuration

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Namespace config | `@AvroNamespace` only | `@avroNamespace` + `AvroConfig.namespace` | Improvement |
| Namespace of a type in the empty package | avro4s 5 (Scala 3): none; avro4s 4 (Scala 2): `"empty"` for records, `"<empty>"` (not a valid Avro namespace) for enums | None, on both Scala versions | Difference (Scala 2 only) |
| Field name transforms (`FieldMapper`) | Yes (`SnakeCase`, custom; broken on avro4s 5) | Yes (3 built-in strategies — snake, kebab, Pascal case — plus a custom function via `withTransformFieldNames`) | Parity |
| Constructor name transforms | Not built-in | Yes | Improvement |
| Decimal config (precision/scale) | `@AvroScalePrecision` or implicit `ScalePrecision` | `AvroConfig.decimalConfig`, `@avroScalePrecision` | Parity (different API) |
| Decimal rounding mode | Implicit `RoundingMode` (v4; v5: always `HALF_UP`) | `withRoundingMode(roundingMode)` / `withDecimalConfig(precision, scale, roundingMode)`; per field `@avroScalePrecision(precision, scale, roundingMode)` | Parity with v4 (different API); the per-field rounding mode is a superset |
| Timestamp precision | A `SchemaFor` with the `timestamp-micros`/`-nanos` logical type | `withInstantPrecision`, `withLocalDateTimePrecision` | Parity (different API) |

### Type classes

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| `SchemaFor[A]` | Yes | Yes (`AvroSchemaFor`) | Parity |
| `Encoder[A]` | Yes | Yes (`AvroEncoder`) | Parity |
| `Decoder[A]` | Yes | Yes (`AvroDecoder`) | Parity |

### Annotations

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@AvroName` / `@fieldName`) | Yes (overrides the field mapper) | Yes (overrides the field name transformation) | Parity |
| ENUM symbol rename (`@AvroName` / `@avroName` on a case object) | Yes | Yes | Parity |
| `@AvroName` of a type argument in a generic record's name | v4 only | Yes | Parity (superset on Scala 3) |
| Documentation (`@AvroDoc` / `@avroDoc`) | Yes (a value class's doc documents the fields of that type: v4) | Yes (same) | Parity |
| Namespace (`@AvroNamespace` / `@avroNamespace`) | Yes; on a field: every named type below the field | Same | Parity |
| Custom properties (`@AvroProp` / `@avroProp`) | Yes (String values; avro4s 5 also `JsonNode`) | Yes (stackable, String values) | Parity |
| Aliases (`@AvroAlias` / `@avroAlias`) | Yes | Yes (stackable) | Parity |
| Fixed size (`@AvroFixed` / `@avroFixed`) | Fields of bytes or a `String` (padded with zeros); value classes (v4) | Same | Parity |
| Error type (`@AvroError` / `@avroError`) | v5 only (no v4 equivalent), on a type or a field | Yes (both Scala 2+3), on a type or a field | Improvement (we add it on Scala 2) |
| Transient field (`@AvroTransient` / `@transientField`) | Yes (always decoded as `None`) | Yes (decoded as the default value, or `None` for an `Option` without one; other fields need a default) | Parity (superset) |
| Default values | From constructor defaults (automatic; no `@AvroDefault` annotation exists upstream) | From constructor defaults (opt out: `withoutScalaDefaultsInSchema`), overridable with `@avroDefault(json)` | Parity |
| Subtype ordering (`@AvroSortPriority` / `@avroSortPriority`) | Yes | Yes | Parity |
| Subtype position (`@AvroUnionPosition` / `@avroUnionPosition`) | Yes | Yes | Parity |
| Java enum annotations (`@AvroJavaName`, `@AvroJavaNamespace`, `@AvroJavaProp`, `@AvroJavaEnumDefault`) | Yes | Yes (Kindlings' own Java annotations of the same names) | Parity |
| `@AvroNoDefault` (suppress defaults) | Yes | Yes (`@avroNoDefault`) | Parity |
| `@AvroErasedName` (disable generic name encoding) | Yes | Yes (`@avroErasedName`) | Parity |
| `@AvroEnumDefault` | Yes (v4; ignored by v5) | Yes (`@avroEnumDefault`) | Parity (superset on Scala 3) |

### Type support

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| UUID | Yes | Yes | Parity |
| Instant | `timestamp-millis` | `timestamp-millis` (configurable precision) | Parity |
| LocalDate | Yes | Yes | Parity |
| LocalTime | `time-micros` | `time-micros` | Parity |
| LocalDateTime | `timestamp-nanos` | `timestamp-nanos` (configurable precision) | Parity |
| `java.sql.Date` / `java.sql.Timestamp` | `date` / `timestamp-millis` | `date` / `timestamp-millis` | Parity |
| BigDecimal | BYTES + decimal logical type **by default** (`ScalePrecision(scale=2, precision=8)`) | STRING **by default**; BYTES + decimal logical type when `DecimalConfig` is provided | Default value difference (see above) |
| `OffsetDateTime` | ISO-8601 string (`datetime-with-offset`) | `timestamp-millis` long by default; avro4s' string with `withOffsetDateTimeAsIsoString` | Default value difference (see above) |
| Value classes | Unwrapped | Unwrapped | Parity |
| Case classes → RECORD | Yes | Yes | Parity |
| Sealed case objects → ENUM | Symbols by full name (v4) / in declaration order (v5) | By name (Scala 2, see the subtype order note at the top) / in declaration order (Scala 3) | Parity |
| Sealed traits → UNION | Members by `@AvroSortPriority`, then full name | Same | Parity |
| `Either[A,B]` → UNION | Yes | Yes | Parity |
| `Option[T]` → UNION(null,T) | Yes | Yes | Parity |
| Recursive types | v4 only | Yes | Parity (superset on Scala 3) |
| Tuples (`Tuple2`..) → RECORD | Named `scala.TupleN` | Named `scala.TupleN__A_B` | Design decision (see above) |
| Generic record names | `Name__A_B` | `Name__A_B` | Parity |
| `Array[Byte]` / `Seq[Byte]` / `ByteBuffer` → BYTES | Yes | Yes | Parity |
| `Utf8` / `CharSequence` → STRING | Yes | Yes | Parity |
| Java enums → ENUM (constants in declaration order) | Yes (v5: schema only) | Yes | Parity |
| Scala 3 enums → ENUM | v5 only | Yes | Parity |
| Scala `Enumeration` → ENUM (named after the enumeration object, symbols in the order of their ids) | v4 only | Yes | Parity (superset on Scala 3) |
| Shapeless Coproduct → UNION | Yes | No | Not ported |

### Decoding

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Integral widening (`Int`/`Short`/`Byte` → `Long`, `Byte`/`Short` → `Int`) | Yes | Yes | Parity |
| Floating-point widening (`Float` → `Double`) | No | No | Parity |
| Missing field with a Scala default | Decodes to the default | Decodes to the default | Parity |
| Field with a Scala default that fails to decode | Decodes to the default | Decodes to the default | Parity |
| Missing field without a default / `null` for a primitive | Rejected (except a `null` Boolean: `false`) | Rejected (except a `null` Boolean: `false`) | Parity |
| A decimal with more digits than the precision | Rejected (`AvroTypeException`) | Rejected (`AvroTypeException`) | Parity |
| Lenient inputs: a String from a `CharSequence`, bytes or a FIXED value; bytes / decimals from a `ByteBuffer`, an array or a FIXED value; an ARRAY from a Java collection or array | Yes (v4 and v5 each accept a different subset) | Yes (the union of both) | Parity (superset) |
| A timestamp out of the range of a long (`Instant.MAX`) | `ArithmeticException` | `ArithmeticException` | Parity |
| Streams: binary, JSON, data files (with a codec) | `AvroOutputStream` / `AvroInputStream` | `AvroIO` (`toBinary`, `toJson`, `toDataFile`, ...) | Parity (different API) |

### Cross-compilation

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | No (v4 ≠ v5, different APIs) | Yes | Improvement |
| Cross-platform | JVM only | JVM only (Avro dependency is JVM-only) | Parity |

### Not ported

| Feature | Notes |
|---|---|
| Shapeless Coproduct → UNION | avro4s can map `A :+: B :+: CNil` to UNION — Hearth doesn't use Shapeless |
| Cats integration | `avro4s-cats` module for `NonEmptyList`, etc. — external ecosystem |
| Refined types | `avro4s-refined` module — external ecosystem |
| Kafka `GenericSerde` | `avro4s-kafka` module — external ecosystem |

### `@avroSortPriority` note

Kindlings uses `Int` priority (upstream avro4s uses `Float`). Higher priority values appear first in UNION/ENUM ordering, matching upstream semantics.

---

## cats-derivation

**Replaces:** [kittens](https://github.com/typelevel/kittens) — automatic/semi-automatic type class derivation for Cats

### Type classes — monomorphic (kind `*`)

| Type class | kittens (Scala 2) | kittens (Scala 3) | Kindlings | Status |
|---|---|---|---|---|
| `cats.Show` | Yes | Yes | Yes | Parity |
| `ShowPretty` | Yes | Yes | Yes | Parity |
| `cats.kernel.Eq` | Yes | Yes | Yes | Parity |
| `cats.kernel.Order` | Yes | Yes | Yes | Parity |
| `cats.kernel.PartialOrder` | Yes | Yes | Yes | Parity |
| `cats.kernel.Hash` | Yes | Yes | Yes | Parity |
| `cats.kernel.Semigroup` | Yes | Yes | Yes | Parity |
| `cats.kernel.Monoid` | Yes | Yes | Yes | Parity |
| `cats.kernel.CommutativeSemigroup` | Yes | Yes | Yes | Parity |
| `cats.kernel.CommutativeMonoid` | Yes | Yes | Yes | Parity |
| `cats.kernel.Band` | No | Yes | Yes | Parity+ |
| `cats.kernel.Semilattice` | No | Yes | Yes | Parity+ |
| `cats.kernel.BoundedSemilattice` | No | Yes | Yes | Parity+ |
| `cats.kernel.Group` | No | Yes | Yes | Parity+ |
| `cats.kernel.CommutativeGroup` | No | Yes | Yes | Parity+ |
| `alleycats.Empty` | Yes | Yes | Yes | Parity |

### Type classes — polymorphic (kind `* -> *`)

| Type class | kittens (Scala 2) | kittens (Scala 3) | Kindlings | Status |
|---|---|---|---|---|
| `cats.Functor` | Yes | Yes | Yes | Parity |
| `cats.Contravariant` | Yes | Yes | Yes | Parity |
| `cats.Invariant` | Yes | Yes | Yes | Parity |
| `cats.Apply` | Yes | Yes | Yes | Parity |
| `cats.Applicative` | Yes | Yes | Yes | Parity |
| `cats.Foldable` | Yes | Yes | Yes | Parity |
| `cats.Traverse` | Yes | Yes | Yes | Parity |
| `cats.Reducible` | Yes | Yes | Yes | Parity |
| `cats.NonEmptyTraverse` | Yes | Yes | Yes | Parity |
| `cats.SemigroupK` | Yes | Yes | Yes | Parity |
| `cats.MonoidK` | Yes | Yes | Yes | Parity |
| `alleycats.Pure` | Yes | Yes | Yes | Parity |
| `alleycats.EmptyK` | Yes | Yes | Yes | Parity |
| `cats.NonEmptyAlternative` | No | Yes | Yes | Parity |
| `cats.Alternative` | No | Yes | Yes | Parity |
| `alleycats.ConsK` | Yes (list-like sealed traits) | No ([#489](https://github.com/typelevel/kittens/issues/489)) | Yes (list-like sealed traits, and case classes: head + nested tail) | Parity (list-like sealed traits); Improvement (case classes) |

### Type classes — bi-variant (kind `(*, *) -> *`)

| Type class | kittens (Scala 2) | kittens (Scala 3) | Kindlings | Status |
|---|---|---|---|---|
| `cats.Bifunctor` | No | Yes | Yes | Parity+ |
| `cats.Bifoldable` | No | Yes | Yes | Parity+ |
| `cats.Bitraverse` | No | Yes | Yes | Parity+ |

### Annotations

| Feature | kittens | Kindlings | Status |
|---|---|---|---|
| `@sensitiveData` — redact field or type in Show/ShowPretty | No | Yes | Improvement |
| `@sensitiveData("reason")` — redact with custom reason | No | Yes | Improvement |

### Type support

| Feature | kittens | Kindlings | Status |
|---|---|---|---|
| Case classes (products) | Yes | Yes | Parity |
| Sealed traits (coproducts) — Show, Eq, Order, Hash | Yes | Yes | Parity |
| Sealed traits (coproducts) — Functor, Foldable, Traverse (e.g., `IList[A]`) | Yes (both, via Shapeless on 2 / Mirrors on 3) | Yes (both Scala 2+3) | Parity |
| Sealed traits (coproducts) — Bifunctor, Bifoldable, Bitraverse (e.g., `Result[A, E]`) | Yes (Scala 3) | Yes (both Scala 2+3) | Parity (Improvement on Scala 2) |
| Bifunctor / Bifoldable / Bitraverse: fields nesting the type parameters (`errs: List[E]`, `either: Either[A, E]`, `Option[A]`, another data type) and recursive types | Yes (Scala 3) | Yes (both Scala 2+3) | Parity (fixed: such fields used to get the function applied to the whole field) |
| Sealed trait with case objects only | Yes | Yes | Parity |
| Fields nesting several type constructors (e.g. `List[Option[A]]`) for Functor/Foldable/Traverse | Yes (composed instances) | Yes (composed instances) | Parity |
| Recursive sealed traits for Functor/Foldable/Traverse without an instance in scope | Yes | Yes | Parity |
| Recursion through another type constructor (`rest: Option[AtLeastOne[A]]`, `child: Option[Search[A]]`) for Functor/Foldable/Traverse | Yes | Yes | Parity (fixed: the inner values used to be passed to the function) |
| Fields wrapping the type parameter (`tail: List[A]`, `children: List[Tree[A]]`, `Option[Vector[A]]`, `List[Option[A]]`) for Apply / Applicative / Pure / Reducible / NonEmptyTraverse / NonEmptyAlternative / Alternative | Yes | Yes (instances combined field by field, like kittens; same shapes rejected: no instance for the wrapping type, no non-empty field for Reducible / NonEmptyTraverse, a field of type `A` for (NonEmpty)Alternative) | Parity |
| Apply / Applicative for recursive types (e.g. a rose tree) | Yes, but the derived instance overflows the stack (observed on Scala 3) | Yes | Improvement |
| A wrapped field whose type constructor is another case class to derive (e.g. `middle: Nel[A]`) | Derived recursively | Derived recursively | Parity |
| PartialOrder: fields with only a `PartialOrder` (e.g. `Set`); different subtypes incomparable (`NaN`) | Yes | Yes | Parity |
| Empty for sealed traits: the single subtype forming an `Empty` (case object or derivable case class) | Yes | Yes (a single subtype with an implicit `Empty` is preferred) | Parity+ |
| Empty for a type recursing through a sealed type (`Recursive(i: Int, is: Option[Recursive])` → `Recursive(0, None)`) | Yes | Yes (a subtype needing the empty value of a type being derived does not form one) | Parity (fixed: was rejected as "multiple variants") |
| Empty for recursive sealed traits whose cons case has an `Empty` field (`IList[Int]` → `INil()`) | No (rejected) | Yes (the non-recursive subtype) | Improvement |
| SemigroupK/MonoidK/EmptyK: fields `G[A]` combined with `SemigroupK[G]`/`MonoidK[G]`/`EmptyK[G]` | Yes | Yes | Parity |
| Nested container fields (e.g., `Vector[A]`, `Option[A]` in `F[A]`) for Functor/Foldable/Traverse | Scala 2 only (Shapeless) | Yes (both Scala 2+3) | Improvement |
| Nested container fields (e.g., `List[A]` in `F[A]`) for ConsK | Scala 2 only (Shapeless) | Yes (both Scala 2+3) | Improvement |
| Head+tail shift pattern for ConsK (e.g., `NEL[A](head: A, tail: List[A])`) | Scala 2 only | Yes (both Scala 2+3) | Improvement |

### Derivation API

| Feature | kittens | Kindlings | Status |
|---|---|---|---|
| Semi-automatic (`TypeClass.derived`) | Yes | Yes | Parity |
| Automatic (`import cats.derived.auto.<tc>._`) | Yes | Yes for `Show`, `Eq`, `Order`, `PartialOrder`, `Hash`, `Semigroup`, `Monoid`, `Empty` (`import hearth.kindlings.catsderivation.auto.<tc>._`), plus `Functor`, `Contravariant`, `Foldable`, `Traverse` on Scala 3; other type classes need `.derived` | Parity (listed type classes) / Gap (the rest) |
| Scala 3 `derives` clause | Yes | Yes (with `import hearth.kindlings.catsderivation.extensions.*`) | Parity |
| Unified Scala 2+3 API | No — Shapeless on 2, Mirrors on 3 | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | JVM focus | JVM + JS + Native | Improvement |
| Strict mode: fail instead of deriving missing field instances (`StrictDerivation`) | No | Yes (monomorphic type classes) | Improvement |

### Runtime performance (ops/s, 2f/5w/10m, temurin 17)

| Type class | Scala | Kindlings | kittens best | vs kittens |
|---|---|---|---|---|
| Show (SimpleCC) | 2.13 | 38.2M | 7.5M | **5.1x faster** |
| Show (SimpleCC) | 3 | 27.2M | 19.9M | **1.4x faster** |
| Eq (SimpleCC) | 2.13 | 100.2M | 46.1M | **2.2x faster** |
| Hash (SimpleCC) | 2.13 | 824.9M | 27.4M | **30.1x faster** |
| Hash (SimpleCC) | 3 | 828.3M | 110.0M | **7.5x faster** |
| Semigroup (IntPair) | 2.13 | 194.1M | 54.3M | **3.6x faster** |
| Monoid (IntPair) | 2.13 | 192.8M | 49.0M | **3.9x faster** |
| Functor (map) | 2.13 | 277.3M | 5.7M | **49x faster** |
| Functor (map) | 3 | 275.9M | 65.2M | **4.2x faster** |
| Foldable (foldLeft) | 3 | 1.6B | 109.6M | **14.6x faster** |
| Traverse (traverse) | 3 | 164.2M | 18.7M | **8.8x faster** |
| ShowPretty (SimpleCC) | 3 | 34.3M | 5.4M | **6.4x faster** |

### Not ported

| Feature | Notes |
|---|---|
| Automatic derivation for the remaining type classes | `hearth.kindlings.catsderivation.auto` covers `Show`, `Eq`, `Order`, `PartialOrder`, `Hash`, `Semigroup`, `Monoid`, `Empty` (and `Functor`, `Contravariant`, `Foldable`, `Traverse` on Scala 3); the others need `.derived` |
| `cats.sequence` / `cats.lift` / `cats.replicateH` | kittens Scala-2/Shapeless utilities (sequencing over HLists/records, lifting functions, heterogeneous replicate); out of scope for type-class derivation |
| `Iterable` derivation (`MkIterable`) | kittens Scala-2-only helper; not ported |

### Law checks

`cats-derivation`'s tests run the discipline law checks of kittens' own suites (`EqSuite` … `BitraverseSuite`, kittens
3.5.0, with kittens' data types and type aliases) on Kindlings-derived instances: `KittensKernelLawsSpec` (Eq, Hash,
Order, PartialOrder, Semigroup, Monoid, the commutative ones, Band, Semilattice, BoundedSemilattice, Group,
CommutativeGroup) and `KittensHktLawsSpec` (Functor, Contravariant, Invariant, Apply, Applicative, Foldable, Traverse,
Reducible, NonEmptyTraverse, SemigroupK, MonoidK, NonEmptyAlternative, Alternative, Bifunctor, Bifoldable, Bitraverse,
plus the Pure / EmptyK expectations, the "respects existing instances" checks and `Serializable`). Not mirrored: the
`auto` / `strict` / `derives` variants of each suite (Kindlings has one derivation mechanism; `StrictDerivation` and
`derives` have their own specs).

### Upstream oracle

`upstream-oracle-tests` (`CatsDerivationOracleSpec`, plus `CatsDerivationScala3OracleSpec` / `CatsDerivationScala2OracleSpec`
for what kittens derives on one Scala version only) derives the same fixtures with Kindlings' `TypeClass.derived` and with
kittens' `cats.derived.semiauto` and compares: `show`/`showLines` strings, `eqv`/`compare`/`partialCompare` on every pair
of values (subtype ordering included), hash consistency with `eqv`, `combine`/`empty`/`inverse`/`remove`, `map`/`fold`/
`traverse`/`reduce`/`bimap`/`bitraverse` results, the order effects run in (`State` logs) and how lazy `foldRight` /
`reduceRightTo` are, `contramap`/`imap`, `map2`/`ap`/`pure`, `combineK`/`empty`, `cons`. Documented differences:

- **Zero-field types in `Show`**: Kindlings renders a case class without fields and a case object as `Name()` on both
  Scala versions; kittens 3's `Show` renders `Name` (its `ShowPretty` keeps `Name()`), kittens 2 renders a case object by
  its runtime class (`Name$()`), and kittens 2's `ShowPretty` puts the parentheses on two lines.
- **`reduceRightTo` laziness**: same results, but when the function short-circuits on an earlier field Kindlings never
  evaluates the later ones; kittens evaluates the function on the later fields first.
- **Hash values** are not compared (only consistency with `eqv`); kittens does not specify them.
- `ConsK` for list-like sealed traits is compared with kittens 2; shapes both reject are asserted too (e.g. Reducible without a non-empty field, a wrapping
  type without the needed instance, a field of type `A` in NonEmptyAlternative).
- Apply/Applicative for the recursive rose tree are not compared: kittens' derived instances overflow the stack.

---

## cats-tagless-derivation

**Replaces:** [cats-tagless](https://typelevel.org/cats-tagless/) derivation macros — `cats.tagless.Derive.*`, the `@auto*K` macro annotations (Scala 2), and the Scala 3 `derive`/`derived` givens.

### Type classes — kind `(* -> *) -> *` (tagless-final algebras)

| Type class | cats-tagless (Scala 2) | cats-tagless (Scala 3) | Kindlings | Status |
|---|---|---|---|---|
| `cats.tagless.FunctorK` | Yes | Yes | Yes | Parity |
| `cats.tagless.ContravariantK` | Yes | Yes | Yes | Parity |
| `cats.tagless.InvariantK` | Yes | Yes | Yes | Parity |
| `cats.tagless.ApplyK` | Yes | Yes | Yes | Parity |
| `cats.tagless.SemigroupalK` | Yes | Yes | Yes | Parity |
| `cats.tagless.aop.Instrument` | Yes | Yes | Yes | Parity |

### Algebra support

| Feature | cats-tagless | Kindlings | Status |
|---|---|---|---|
| `trait Alg[F[_]]` algebras | Yes | Yes | Parity |
| `case class Alg[F[_]]` algebras | Scala 3 only (separate `cats-tagless-data` module via `derives`; core `Derive.*` is trait-only) | Yes (Scala 2 + 3, same `.derived`) | Improvement |
| Sealed trait / enum algebras (`enum Tree[F[_]] { case Leaf(values: F[String]); case Branch(size: Int, left: Tree[F], right: Tree[F]) }`) for FunctorK / ContravariantK / InvariantK, recursive ones included | Scala 3 only (`cats-tagless-data`) | Yes (Scala 2 + 3) | Parity (Improvement on Scala 2) |
| Case-class algebra fields nesting `F` in a type with several arguments (`Kleisli[F, A, B]`, `EitherT[F, E, A]`, `OptionT[F, A]`, `Tuple2K[F, Option, A]`, `EitherK[List, F, A]`, `FunctionK[F, Option]`, `Cokleisli[F, A, B]`) | Yes (`cats-tagless-data`, through the `FunctorK` & co. of `[F[_]] =>> T[F]`) | Yes | Parity (was rejected) |
| `F` nested in trait algebra members (`EitherT[F, E, A]` results, `Cokleisli[F, A, B]` parameters, other algebras, builder-style self references), varargs element by element | Yes | Yes | Parity |
| Algebras with extra type parameters (`AlgWithExtraTP[F, T]`) | Yes | Scala 3 (type lambda `[F[_]] =>> Alg[F, T]`); Scala 2: Gap (Hearth, `docs/research/hearth-gap-cats-tagless-trait-algebras.md` item 5) | Parity on Scala 3 / Gap on Scala 2 |
| Refined algebras with type members (`AlgWithTypeMember.Aux[F, String]`, `AlgWithTypeBound`, `AlgWithAbstractTypeClass`) | Yes | No (Hearth: anonymous instances of refined types, item 4) | Gap |
| Dependent method types (`def dependent[A](f: DepFn[A])(out: f.Out)`) | Yes | No (Hearth, item 3) | Gap |
| By-name parameter of a method type parameter (`def defer[A](x: => A): F[A]`) in SemigroupalK | Yes | Scala 2: Yes; Scala 3: No (Hearth, item 2) | Gap on Scala 3 |
| Mixed members (`F[_]` + plain) | Yes | Yes | Parity |
| `F[_]` in argument position (`InvariantK`) | Yes | Yes | Parity |
| Nested algebras | Yes | Yes | Parity |
| Recursive derivation of nested algebra without explicit instance | Partial | Yes | Improvement |
| Polymorphic methods (`def m[A](a: A): F[List[A]]`) in trait algebras | Yes | Yes | Parity |
| Multiple parameter lists, by-name and default arguments, varargs | Yes | Yes | Parity |
| SemigroupalK/ApplyK: members without `F` combined with their `Semigroup` (else the first algebra's result) | Yes | Yes (trait algebras) | Parity |

### Derivation API

| Feature | cats-tagless | Kindlings | Status |
|---|---|---|---|
| Semi-automatic (`Derive.functorK` / `KindlingsFunctorK.derived`) | Yes | Yes | Parity |
| Macro-annotation derivation (`@autoFunctorK`, …) | Yes (Scala 2 only) | No (use `.derived`) | Trade-off |
| Unified Scala 2 + 3 API | No — macro annotations on 2, inline `derive` on 3 | Yes — same `.derived` call shape on both | Improvement |
| `@experimental` required (Scala 3) | Yes (`Derive.*` / `derives` are `@experimental`) | No | Improvement |
| Cross-platform (JVM / JS / Native) | JVM + JS + Native | JVM + JS + Native | Parity |

### Not ported

| Feature | Notes |
|---|---|
| `@autoFunctorK` / `@finalAlg` / `@autoInstrument` macro annotations | Kindlings uses explicit `.derived` calls instead of annotation-driven companion generation |
| `aop.Aspect` (general AOP) beyond `Instrument` | Only `Instrument` is derived; the more general `Aspect[Alg, Dom, Cod]` is not ported |
| `Derive.const` / `Derive.void` / `Derive.readerT` algebra constructors | Not ported — out of scope (constant / `Unit` / `ReaderT`-forwarding algebra implementations) |
| `Derive.semigroupK` / `Derive.monoidK` | Out of scope — these derive plain cats `SemigroupK`/`MonoidK` for `F[_]`, not `(* -> *) -> *` algebra type classes |
| `Derive.functor` / `contravariant` / `invariant` / `semigroupal` / `apply` / `flatMap` / `bifunctor` / `profunctor` | cats-tagless also derives cats' kind `* -> *` / `* -> * -> *` type classes for algebras whose type parameter is the *result* type (`trait SafeAlg[T] { def parseInt(s: String): T }`, cats-tagless' `autoFunctorTests` … `autoProfunctorTests`): not ported (Gap). For data types, `kindlings-cats-derivation` derives these type classes |

### Upstream oracle

`upstream-oracle-tests` (`CatsTaglessOracleSpec`) derives FunctorK, InvariantK, ContravariantK, SemigroupalK, ApplyK and
Instrument with Kindlings and with cats-tagless' `Derive.*` (Scala 2 macros / Scala 3 `@experimental` macros) for trait
algebras with one method shape each (plain, nullary, multiple parameter lists, by-name, polymorphic, default arguments,
varargs, `F` in parameter position, members without `F`) and compares the results of every method of the transformed
algebra, when the natural transformation runs, how often by-name arguments are evaluated, and `Instrument`'s algebra and
method names. No differences. `CatsTaglessShapesOracleSpec` does the same for the shapes of cats-tagless' own tests (`F`
nested in `EitherT` / `Kleisli` / `OptionT` / `Cokleisli`, builder-style algebras, concrete members, `F` varargs), and
`CatsTaglessLawsSpec` runs cats-tagless-laws' FunctorK, InvariantK, ContravariantK, SemigroupalK, ApplyK and Instrument
rule sets (and `SerializableTests`) on Kindlings-derived instances for cats-tagless' test algebras (`SafeAlg`,
`SafeInvAlg`, `CalculatorAlg`, `AutoApplyKTestAlg`, `autoContravariantKTests.TestAlgebra`). Case-class and sealed
algebras are not compared (cats-tagless' `Derive.*` is trait-only, `cats-tagless-data` is Scala 3 only); they are
covered by `DataAlgebrasSpec` (cats-tagless-data's suite algebras). For case-class algebras Kindlings' SemigroupalK/ApplyK
still keep the first algebra's members without `F`. The shapes blocked by Hearth gaps are asserted by `KnownGapsSpec`.

---

## diff-derivation

**Original type class** — no replacement target. Provides structural comparison (`Diff[A]`) for case classes, sealed traits, collections, maps, options, and strings. Uses the Myers diff algorithm for ordered sequences and hierarchical string comparison (line → word → character).

### Type classes

| Type class | Description |
|---|---|
| `Diff[A]` | `def diff(left: A, right: A): DiffResult` + `def snapshot(value: A): DiffResult` |

### Type support

| Type | Support |
|---|---|
| Primitives, String | Built-in (value comparison + string diff) |
| Case classes | Field-by-field structural diff → `DiffResult.Record` |
| Sealed traits | Variant matching → `DiffResult.Variant` / `DiffResult.TypeMismatch` |
| `Option[A]` | Presence tracking → `DiffResult.OptionalDiff` |
| `List`, `Vector`, `Seq` | Myers diff → `DiffResult.SeqDiff` (Insert/Delete/Equal edits) |
| `Map[K, V]` | Key-matched diff → `DiffResult.MapDiff` (Matched/Added/Removed entries) |
| `Set[A]` | Element-matched diff → `DiffResult.SetDiff` |
| `String` | Hierarchical Myers: line → word → character → `DiffResult.StringDiff` |
| Recursive types | Yes (via def-caching) |
| Nested containers (`Option[Person]`, `List[Person]`, `Map[String, Person]`) | Yes (recursive inner-type derivation) |

### Output rendering

`DiffRenderer` formats `DiffResult` as human-readable text with ANSI colors or plain text, showing insertions, deletions, and changes at each nesting level.

### Customization

| Feature | Support |
|---|---|
| Implicit `Diff` for a field type overrides derivation | Yes |
| Approximate numeric comparison | `Diff.approximate(epsilon)` |
| Ignoring fields | Top-level fields only: `ignoreField(name)` / `modifyField(name).ignore` (and `modifyField(name).using(diff)` to replace a field's comparison) |
| Key-matched sequence diff | `Diff.seqDiff(elementDiff, ObjectMatcher.by(key))` |
| Assertions | `Diff.assertNoDiff` / `Diff.assertDiff` (throw `AssertionError`, framework-agnostic) |

### Cross-compilation

| Feature | Status |
|---|---|
| Scala 2.13 + 3 | Yes |
| JVM + JS + Native | Yes |

---

## circe-derivation

**Replaces:** `circe-generic-extras` (community-maintained; Scala 2 via Shapeless, Scala 3 via Mirrors — cross-published JVM/JS/Native since 0.14.5) + circe's Scala 3 `ConfiguredEncoder`/`ConfiguredDecoder`/`ConfiguredCodec` + circe-generic's semiauto helpers

Checked against circe-generic (`deriveCodec`), circe-generic-extras (`deriveConfiguredCodec`, `deriveEnumerationCodec`,
`deriveUnwrappedCodec`, `deriveExtrasCodec`, `deriveConfiguredFor[..].patch`/`.incomplete`, `@JsonKey`) and, on Scala 3,
circe-core's `ConfiguredCodec`/`ConfiguredEnumCodec` by the `upstream-oracle-tests` module (`CirceGenericOracleSpec`,
`CirceExtrasScala2OracleSpec`, `CirceCoreOracleSpec`): for each configuration (defaults, strict decoding,
discriminators, member and constructor name transforms) the encoded JSON (key order included) and the results of
`decodeJson` and `decodeAccumulating` are compared, on valid inputs and on inputs with every key removed, set to `null`,
set to a wrong type, renamed, plus unknown keys and wrapper/discriminator edge cases. On Scala 3 failure messages and
cursor histories are compared too; on Scala 2 only outcomes and decoded values (Kindlings uses Scala 3 core's wording
on both versions, see "Error messages and cursor history"). The differences found are the ones listed below as
"Difference". The fixtures are compiled in another module than the tests (as for users deriving instances for types
of a dependency), which keeps covering a since-fixed Hearth bug (`docs/research/hearth-bug-children-order-from-classpath.md`):
the strict wrapper failure lists the constructor names in declaration order on Scala 3, like circe.

The fixtures and configurations of upstream's own derivation test suites are run the same way
(`CirceUpstreamSuitesOracleSpec`, `CirceCoreSuitesOracleSpec`): circe-generic's `SemiautoDerivedSuite`, circe-core's
`DerivesSuite`, `SemiautoDerivationSuite`, `ConfiguredDerivesSuite` and `ConfiguredEnumDerivesSuites`, generic-extras'
`ConfiguredSemiautoDerivedSuite`, `ConfiguredAutoDerivedSuite` and `SharedConfiguredSemiautoDerivedSuite` (every
member/constructor name transformation, defaults with `Option`s and generic classes, discriminators, strict decoding,
children with hand-written instances, tuples and sequences of sums, recursive types, 33-field classes, 33-case sums and
enums, members of intersection types, hierarchies with diamonds). What those suites assert beyond values (call counts
of the name transformations, `decodeStrict`, compile-time errors, the macro annotations) is mirrored in
`CirceUpstreamSuitesSpec` and `JsonCodecAnnotationSpec`.

### Configuration

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Field name transforms (snake/kebab/pascal/screaming) | Yes | Yes | Parity |
| Constructor name transforms | Yes | Yes | Parity |
| Public renaming functions | Scala 3 core `io.circe.derivation.renaming` (`snakeCase`, `screamingSnakeCase`, `kebabCase`, `pascalCase`, `replaceWith`) | `hearth.kindlings.circederivation.renaming` (same functions) | Parity |
| `useDefaults` | Yes (absent field, or `null` the field decoder rejects, uses the default; a `Decoder[Option[X]]` that decodes an absent key to a value keeps that value; Scala 3 reads the constructor default methods, no compiler flag needed) | Yes (same rules) | Parity |
| Discriminator field | Yes | Yes | Parity |
| Strict decoding | Yes (discriminator key is an expected field; `decodeAccumulating` reports the strict failure and the field errors; multi-key wrapper objects rejected) | Yes (same rules) | Parity |
| `enumAsStrings` config flag | Scala 2: `deriveEnumeration*`; Scala 3: `ConfiguredEnumEncoder`/`ConfiguredEnumDecoder`/`ConfiguredEnumCodec` (parameterless cases only) | Yes (unified config flag, both Scala 2+3, full enums) | Improvement |
| Value classes (`AnyVal` case classes) | Scala 2 (circe-generic, generic-extras): encoded as products (`{"value":5}`); Scala 3: not derivable (no `Mirror` for value classes), instances are written by hand; unwrapping via explicit `deriveUnwrapped*` | Unwrapped by default (`5`); `withoutValueClassUnwrapping` (`unwrapValueClasses = false`) restores the upstream product format, also for value-class sum children | Difference (default) / Parity (`withoutValueClassUnwrapping`) |
| Named-argument factories | Scala 3 core `ConfiguredCodec.derive(transformMemberNames = ..., ...)` (also `ConfiguredEncoder`/`ConfiguredDecoder`) | Scala 3: `KindlingsCodecAsObject.derive(...)`, `KindlingsEncoder.derive(...)`, `KindlingsDecoder.derive(...)`; both versions: `Configuration(...)` with named arguments | Parity |
| Enum factories | Scala 3 core `ConfiguredEnumCodec.derive(transformConstructorNames = ...)` (also `ConfiguredEnumEncoder`/`ConfiguredEnumDecoder`) | Scala 3: `KindlingsCodecAsObject.deriveEnum(...)` (a plain `Codec`), `KindlingsEncoder.deriveEnum(...)`, `KindlingsDecoder.deriveEnum(...)`; a sum with non-singleton cases is a compile-time error | Parity |
| Compile-time configuration | n/a (Scala 3 core: inline `Configuration` givens) | Evaluated at compile time when possible (inline expressions, named arguments, and on Scala 3 local `val`s/givens): enables compile-time checks | Improvement |

### Type classes

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| `Encoder[A]` | Yes | Yes | Parity |
| `Encoder.AsObject[A]` | Yes | Yes (`deriveAsObject`) | Parity |
| `Decoder[A]` | Yes | Yes | Parity |
| `Codec.AsObject[A]` | Yes (circe-generic `deriveCodec`, generic-extras `deriveConfiguredCodec`, Scala 3 `ConfiguredCodec`) | Yes (`KindlingsCodecAsObject`, keeps `decodeAccumulating`) | Parity |
| `ExtrasDecoder` (`isStrict`, `decodeStrict` with the extraneous fields) | generic-extras | `hearth.kindlings.circederivation.ExtrasDecoder`, implemented by `KindlingsDecoder` and `KindlingsCodecAsObject` (derived case-class decoders are strict with `strictDecoding`) | Parity |
| Patch decoders | circe-generic `deriveFor[A].patch` / generic-extras `deriveConfiguredFor[A].patch` (with `useDefaults`, generic-extras' patch decoder throws a `ClassCastException`) | `KindlingsDecoder.patch[A]` (absent/`null` keeps the value, wrong type fails, no deep merge, `useDefaults` ignored) | Parity |
| Incomplete decoders | circe-generic `deriveFor[F].incomplete` / generic-extras `deriveConfiguredFor[F].incomplete` (`F = (P1, ..) => C`) | `KindlingsDecoder.incomplete[F]` (each `Pi` supplies the first remaining field of that type, like shapeless' `RemoveAll`; the configuration's names and defaults apply) | Parity |
| Labelled incomplete decoders | circe-generic (Scala 2): shapeless `FieldType[Witness.`'j`.T, Int] => C` selects the field `j` | `Labelled["j", Int] => C` (a `String` literal type, both Scala 2+3); labelled and by-type parameters can be mixed | Parity |

### Annotations

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Per-field rename | `@JsonKey` (Scala 2 only! No Scala 3 equivalent) | `@fieldName` (both Scala 2+3) | Improvement |
| Transient field | No built-in annotation | `@transientField` (requires default value) | Improvement |
| `@JsonCodec` / `@JsonCodec(encodeOnly/decodeOnly = true)` | circe-generic, Scala 2 macro annotation (`-Ymacro-annotations`); only one, named argument set to `true` is accepted | Same (Scala 2, `-Ymacro-annotations`; positional arguments and `= false` are accepted too, both set to `true` is rejected); Scala 3: `derives KindlingsCodecAsObject` / `KindlingsEncoder` / `KindlingsDecoder` | Parity |
| `@ConfiguredJsonCodec` | generic-extras, Scala 2 macro annotation | Same (Scala 2, `-Ymacro-annotations`, uses the `Configuration` in scope); Scala 3: `derives` with a given `Configuration` | Parity |

### Type support

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Explicit unwrapping | `deriveUnwrapped*` (any single-field case class; both Scala 2+3) | `KindlingsEncoder/KindlingsDecoder.deriveUnwrapped` (same) | Parity |
| Tuples | Encoded/decoded as JSON arrays of the exact arity | Same, also for tuples containing derived types | Parity |
| Recursive types | Scala 2: needs Shapeless `Lazy`; Scala 3: broken with fully-automatic `generic.auto` derivation ([#1980](https://github.com/circe/circe/issues/1980)) | Works without wrappers | Improvement |
| Non-string map keys | Built-in for Int/Long/Double/Short/Byte/UUID/URI/Symbol; manual `KeyEncoder`/`KeyDecoder` for value classes & enums | A `KeyEncoder`/`KeyDecoder` in scope first (user-provided or circe's), then built-in primitives, value classes and enums (also nested case-object hierarchies) | Improvement (auto value-class/enum keys) |
| Scala 3 enums | Object encoding of enums with parameterised cases (`derives Codec`, `ConfiguredCodec`); string encoding (`ConfiguredEnumCodec`) for parameterless cases only | Object encoding of full enums; string encoding (`enumAsStrings`) for parameterless cases | Parity |

### Sum types and decoding behaviour

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Nested sealed hierarchies (derived intermediates) | Scala 3 core: flattened, a leaf is written under its own name (`{"Child":{..}}` / `"type":"Child"`) and decoded from its own name at the top level, also when the intermediate's instance is a `ConfiguredCodec` of the sum (its output is used as is, its constructors are dispatched on) | Same, also when the intermediate's instance was derived by Kindlings (`derives KindlingsCodecAsObject`, `implicit val codec = KindlingsCodecAsObject.derived[Parent]`, ...) | Parity |
| Nested sealed hierarchies (hand-written intermediate instances) | Scala 3 core: the instance is used under the intermediate's name; generic-extras (Scala 2, shapeless flattens to the leaves): the instance is ignored | The instance is used under the intermediate's name, on Scala 2 and 3 | Parity (Scala 3 core) / Difference (generic-extras) |
| A leaf reachable through several intermediates (diamond) | Written once under its own name; with an intermediate instance, the first intermediate (declaration order) containing it | Same | Parity |
| Nested hierarchy of case objects with string encoding | Flattened (`ConfiguredEnumCodec` recurses into intermediates and ignores their instances) | Flattened (`enumAsStrings`), also when an intermediate has a user-provided instance | Parity |
| Wrapper object with superfluous keys | Scala 3 core: the first key (JSON order) naming a known constructor is used, rejected with strict decoding; Scala 2 (circe-generic, generic-extras): the first constructor in coproduct (alphabetical) order found among the keys, accepted also with strict decoding (strict only inside case classes) | Scala 3 core behaviour on both versions | Parity (Scala 3 core) / Difference (Scala 2: key choice, strict wrappers) |
| Strict wrapper failure | Lists the direct constructor names (`constructorNames`) | Same | Parity |
| Errors inside collection elements | Keep the cursor history (`.members[1].age`) | Same | Parity |
| Missing `Option[A]` field (any `A`, with or without a default, `useDefaults = false`) | `None` | `None` | Parity |
| Value class over `Option` as an absent field | Upstream codes value classes as products (absent: missing field); `deriveUnwrapped*` keeps `Option` decoding (wrapped `None`) | Default (unwrapping): wrapped `None` with a derived decoder (the field's default with `useDefaults`, like a plain `Option` field), a missing-field failure with a user-provided decoder; `withoutValueClassUnwrapping`: missing field | Kindlings default / Parity (`withoutValueClassUnwrapping`, `deriveUnwrapped`) |
| Discriminator with a non-object child | generic-extras: written in the wrapped form `{"Name":..}` (not read back); Scala 3 core: the value is dropped (`{"type":"Name"}`) | Written in the wrapped form and read back (tapir schemas show the same wrapped form) | Parity (encoding, generic-extras) / Improvement (decoding) |
| Enum map keys | Manual `KeyEncoder`/`KeyDecoder` | Constructor name (with `transformConstructorNames`) on both sides | Improvement |
| User-provided `KeyEncoder`/`KeyDecoder` for `Int`/`Long`/... | Used | Used on both sides (take precedence over the built-in key codecs), also when the map's value type is derived | Parity |
| Accumulating decoding | Accumulates in nested derived types, `Option`s, collections and maps | Same (also for top-level derived non-products) | Parity |
| Error messages and cursor history | Scala 3 core wording (`type X has no class/object/case named 'n'.`, `Strict decoding X - unexpected fields: ...`, ...); Scala 2 shapeless derivations word them differently (`JSON decoding to CNil should never happen`, a missing first field for a non-object input, ...) | Scala 3 core wording and history on both versions | Parity (Scala 3) / Difference (Scala 2 wording) |
| Discriminator position | Appended after the fields | Appended after the fields | Parity |
| Strict decoding of case objects | Extra keys rejected (in sums, at the top level, as fields) | Same | Parity |
| `Encoder.AsObject` from semiauto / `derives` | Products and sums | `KindlingsEncoder.derived` returns an `Encoder.AsObject` instance for products and sums; `deriveAsObject` on a non-object type is a compile-time error | Parity |
| `enumAsStrings` / `ConfiguredEnumCodec` on non-singleton cases | Compile-time error | Compile-time error when the configuration is evaluable at compile time; otherwise a clear runtime failure | Parity (compile time) / Difference (runtime-only configuration) |
| Non-object input for a case class whose fields all have defaults or are `Option`s (`useDefaults`) | Scala 3 core: rejected (`WrongTypeExpectation("object", ..)`); generic-extras on Scala 2: every field is read through a failed cursor, so any JSON (`null`, `1`, `"x"`, `[]`) decodes to the all-defaults value | Rejected on both versions | Parity (Scala 3 core) / Difference (generic-extras on Scala 2) |
| Case classes with several parameter lists | Not derived | Compile-time error (also with an empty first list) | Parity |

### Cross-compilation

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | No — different modules, different APIs, feature gaps on Scala 3 | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | Yes (circe core and circe-generic-extras 0.14.5+ both cross-publish) | JVM + JS + Native | Parity |
| Compile-time performance | Scala 2: Shapeless = slow, stack overflows on deep types; Scala 3: Mirror-based | Hearth macros | Improvement (esp. vs Scala 2 Shapeless) |
| Error messages | "could not find Lazy implicit value..." | Clear messages from Hearth | Improvement |

### tapir schemas

`tapir-schema-derivation` with a circe `Configuration` describes what the circe codec writes: subtypes wrapped as
`{"Name": ..}` without a discriminator, nested sealed hierarchies flattened (a hand-written intermediate instance kept as
one subtype), `AnyVal` case classes as objects with `unwrapValueClasses = false`.

### Not ported

Nothing: every circe-generic, generic-extras and Scala 3 core derivation feature has an equivalent (shapeless-specific
APIs have a cross-compiled counterpart, e.g. `Labelled` for `FieldType`).

---

## jsoniter-derivation

**Replaces:** `com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker`

**Checked against upstream** by `JsoniterOracleSpec` and `JsoniterScala3OracleSpec` (upstream-oracle-tests): the same
fixtures derived with `KindlingsJsonValueCodec.derived` (with the config known at compile time and only at runtime) and
with `JsonCodecMaker.make` (with the equivalent `CodecMakerConfig` / `makeXxx` entry point) are compared on encoded JSON,
accepted and rejected inputs (decoded values and error messages with offsets) and `nullValue`, for upstream's defaults
(`JsoniterConfig.jsoniterScalaDefaults`), Kindlings' defaults (exactly the differences listed below) and every config
option with an upstream counterpart (field/leaf/Java-enum name mappers, discriminators, `skipUnexpectedFields`,
`transient*`, `require*`, `checkFieldDuplication`, `isStringified`, `mapAsArray`, number and insert limits,
`useScalaEnumValueId`, `alwaysEmitDiscriminator`, `skipNestedOptionValues`, `inlineOneValueClasses`,
`circeLikeObjectEncoding`, `requireDiscriminatorFirst`, `scalaTransientSupport`, `transientNull`, `decodingOnly` /
`encodingOnly`, all `makeXxx` presets), on case classes, defaults, nested options, collections and maps (non-`String`
keys), `::`, ADTs, Scala 3 enums, Java enums, `Enumeration`s, value classes, tuples (generic, named, recursive),
recursive types, annotations, `BigDecimal`/`BigInt`, `java.time`, `UUID` and `Either`.

`JsoniterUpstreamSpecOracleSpec` does the same with jsoniter-scala's **own test suite**: the fixtures, values and inputs
of its `JsonCodecMakerSpec` (v2.42.3; about 330 cases: primitives and their boxes at the top level, stringified and as
map keys, `Enumeration`s with private/lazy/later-added values, arrays, iterators, mutable/immutable/sorted
collections and maps, `IntMap`/`LongMap`/bit sets, all `java.time` types as values and keys, value classes and
one-value classes, defaults, transient fields, name mappers, required fields, ADT shapes and generic/higher-kinded
types).

### Configuration

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Field name mapper | Yes (a `PartialFunction`) | Yes (function or partial function) | Parity |
| ADT leaf class name mapper | Yes (receives the fully-qualified name, default `simpleClassName`) | Yes (same) | Parity |
| Discriminator field | Yes (default `Some("type")`) | Yes (`withDiscriminator`, `withDiscriminatorFieldName`) | Parity |
| Skip unexpected fields | Yes (default `true`) | Yes | Parity |
| Case objects / enum values as strings | Yes (without a discriminator; Java enums and `Enumeration`s always) | Yes (same), plus `withEnumAsStrings` for case-object-only ADTs with a discriminator | Parity + Improvement |
| `requireDiscriminatorFirst` | Yes (default `true`) | Yes (default `true`) | Parity |
| Map as array | Yes | Yes | Parity |
| `transientDefault` | Yes | Yes | Parity |
| `transientEmpty` | Yes | Yes | Parity |
| `transientNone` | Yes | Yes | Parity |
| `isStringified` (global) | Yes (from the root down to the first class/ADT: top level, collections, map values, options, tuples, bit sets; never case class fields) | Yes (same) | Parity |
| `decodingOnly` / `encodingOnly` | Yes | Yes | Parity |
| `requireCollectionFields` | Yes | Yes | Parity |
| `requireDefaultFields` | Yes | Yes | Parity |
| `circeLikeObjectEncoding` | Yes | Yes (case objects encoded as `{"Name":{}}` matching upstream) | Parity |
| `checkFieldDuplication` | Yes | Yes | Parity |
| `bigDecimalPrecision`/`bigDecimalScaleLimit`/`bigDecimalDigitsLimit` | Yes (DoS protection) | Yes | Parity |
| `bigIntDigitsLimit` | Yes (default `308`) | Yes (default `308`) | Parity |
| `transientNull` | Yes (Scala 3 `T \| Null` fields, which are also optional when absent) | Yes (same; no-op on Scala 2) | Parity |
| `scalaTransientSupport` | Yes | Yes (compile-time and runtime configs, also `writeToString`/`readFromString`) | Parity |
| Name mapper helpers (`enforce_snake_case`, `enforce-kebab-case`, `EnforcePascalCase`, `enforceCamelCase`, `simpleClassName`) | Yes | Yes (`JsoniterConfig.*`, same semantics) | Parity |
| Compile-time config validations (flag conflicts, key and discriminator collisions) | Yes | Yes (compile-time configs; flag conflicts of runtime configs throw `IllegalArgumentException` when the codec is created) | Parity |
| `mapMaxInsertNumber` / `setMaxInsertNumber` | Yes (DoS protection) | Yes (`setMaxInsertNumber` limits only sets, as upstream) | Parity |
| `useScalaEnumValueId` | Yes | Yes | Parity |
| `Enumeration` values which are not public `val`s (private, lazy, `var`, created by a method, added after the codec was created) | Yes (looked up at runtime) | Yes (same) | Parity |
| `javaEnumValueNameMapper` | Yes | Yes (`withJavaEnumValueNameMapper`) | Parity |
| `inlineOneValueClasses` | Yes (also map keys) | Yes (`withInlineOneValueClasses`, also map keys and transient parameters) | Parity |
| `bitSetValueLimit` (DoS protection) | Yes | Yes | Parity |
| `skipNestedOptionValues` | Yes (without it, inner `Option`s are the ADT `Option`: `{"type":"Some","value":...}`) | Yes (same) | Parity |
| `alwaysEmitDiscriminator` | Yes | Yes (`withAlwaysEmitDiscriminator`) | Parity |
| Convenience factories (`JsonCodecMaker.makeCirceLike`, `makeWithoutDiscriminator`, etc.) | Yes (`makeXxx[A]` entry points) | Yes, as config presets: `KindlingsJsonValueCodec.derived[A](JsoniterConfig.makeCirceLike)` etc. (all `makeXxx` variants, built on upstream's defaults) | Parity |

### Type classes

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| `JsonValueCodec[A]` | Yes | Yes | Parity |
| `JsonCodec[A]` | Yes | Yes | Parity |
| `JsonKeyCodec[A]` | Yes (user-provided ones take precedence over built-in key types) | Yes (`deriveKeyCodec`; user-provided ones take precedence, also for `String`) | Parity |

### Annotations

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@named` / `@fieldName`) | Yes (`@named`) | Yes (`@fieldName`, and `@named` on fields) | Parity |
| ADT leaf rename (`@named` on classes, objects, Scala 3 enum cases) | Yes | Yes (`@named`) | Parity |
| Transient field (`@transient` / `@transientField`) | Yes | Yes (requires default value) | Parity |
| `@stringified` | Yes (also on `Option`/collection fields) | Yes (same) | Parity |

### Type support

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Non-string map keys | Yes (extensive) | Yes (primitives, boxed, `BigInt`/`BigDecimal`, `UUID`, `java.time`, value classes/opaque types, case-object enums, `JsonKeyCodec`); unsupported keys are a compile-time error with a compile-time config | Parity |
| `IntMap`/`LongMap` specialized maps | Yes | Yes (via Hearth standard collection provider) | Parity |
| `UUID` | Yes | Yes | Parity |
| `BitSet` (immutable + mutable) | Yes | Yes | Parity |
| `Either[A, B]` | Yes | Yes | Parity |
| Boxed primitives (`java.lang.Integer`, etc.) | Yes | Yes | Parity |
| Value class unwrapping | Yes (+ `inlineOneValueClasses`) | Automatic (always) | Parity |
| Recursive types | Yes (`allowRecursiveTypes = true`) | Yes (no flag needed) | Improvement |
| java.time built-in codecs | Yes (ISO-8601) | Yes (ISO-8601: Instant, LocalDate, LocalTime, LocalDateTime, OffsetDateTime, ZonedDateTime, Duration, Period) | Parity |
| Tuples as JSON arrays (incl. generic tuples and arity > 22 on Scala 3) | Yes | Yes | Parity |
| Named tuples (JSON objects) | Yes | Yes (also behind type aliases, and recursive ones behind local type aliases) | Parity |
| Recursive tuples behind local type aliases (`type R = (Int, Option[R])`) | Yes (`allowRecursiveTypes`) | Yes | Parity |
| `scala.collection.immutable.::` (non-empty JSON arrays, required fields) | Yes | Yes | Parity |
| Literal types (also as map keys and case class fields) | Yes | Yes | Parity |
| Plain (non-case) classes, plain-class/object ADT leaves, non-abstract sealed classes as leaves | Yes | Yes | Parity |
| Multiple parameter lists (with dependent defaults) | Yes | Yes | Parity |
| User-provided codecs of ADT leaves with a discriminator | Yes (encode/decode the whole leaf object) | Yes (same) | Parity |
| Codecs of case objects (`derived[Leaf.type]`) | Yes (any JSON object or `null` is the object, which is its `nullValue`) | Yes (same) | Parity |
| Field names and ADT leaf names with operators (`kebab-case`, `US/Alaska`) | Yes | Yes (a field name which *starts or ends with a space* is not derivable on Scala 2) | Parity (gap: spaces, Scala 2) |
| Case classes nested in a trait, with default values (`trait T { case class U(a: Int = 1) }; object T extends T`) | Yes | No: the compiler fails after the macro expansion (Hearth bug; without default values such classes work) | Gap |
| Java enums nested in a Java class (`Thread.State`) | Yes | Scala 2: yes. Scala 3: derivation fails (Hearth bug) | Gap (Scala 3) |
| `nullValue` / `default` semantics (`None`, empty collections, `null` and `[]`/`{}` of a collection field → field default, user-provided field codecs receive the field default) | Yes | Yes (except: with `checkFieldDuplication = false`, a repeated key's decoder gets the field default, not the value read for the earlier occurrence) | Parity |

### Cross-compilation

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | Separate macro implementations | Unified API | Improvement |
| Cross-platform | JVM + JS + Native | JVM + JS + Native | Parity |

**Design decisions** (not gaps; each one is an expected difference of the oracle suites):
- `allowRecursiveTypes` — upstream gates recursive types behind a flag (default `false`); Kindlings always allows them, so no flag is needed.
- Without a discriminator, a case object is also read from `{"Leaf":{}}` or `{"Leaf":null}` (a superset; errors for objects given where
  upstream expects a string of an ADT of case objects only are Kindlings' then).
- A Scala 3 `T | Null` field holding `null` is written as `null` without `transientNull` (upstream throws a
  `NullPointerException`).
- With `checkFieldDuplication = false`, a repeated key's decoder gets the field's default (upstream: the value read for
  the earlier occurrence).
- Error messages for a value of another JSON type in a field with a default value do not say `or null`
  (`expected '{'`; upstream: `expected '{' or null`).
- A field typed as an abstract collection type whose *element* type is a collection which is a subtype of it
  (`mutable.Map[K, mutable.ListMap[..]]`, `mutable.IndexedSeq[mutable.Stack[A]]`) is built as the element type's
  implementation, where jsoniter-scala builds the declared type's default one: see "Collections typed as an abstract
  collection type" at the top of this page.
- Where upstream's Scala 2 and Scala 3 macros report different errors (ADTs without case objects, or with
  `circeLikeObjectEncoding`, given something else than an object: `expected '{'` / `expected '"' or '{'`), Kindlings
  reports what upstream reports on the same Scala version.

### Default value differences

| Config field | Upstream default | Kindlings default | Notes |
|---|---|---|---|
| `mapMaxInsertNumber` | `1024` | `Int.MaxValue` | Kindlings: no map-size limit unless set explicitly |
| `setMaxInsertNumber` | `1024` | `Int.MaxValue` | Kindlings: no set-size limit unless set explicitly |
| `transientDefault` | `true` | `false` | Kindlings requires explicit opt-in |
| `transientEmpty` | `true` | `false` | Kindlings requires explicit opt-in |
| `transientNone` | `true` | `false` | Kindlings requires explicit opt-in |
| `checkFieldDuplication` | `true` | `false` | Kindlings is permissive by default |
| `discriminatorFieldName` | `Some("type")` | `None` | Kindlings uses single-key wrapper encoding by default (case objects as strings) |

`JsoniterConfig.jsoniterScalaDefaults` is a one-shot preset that restores upstream's defaults (1024 insert limits, `transientDefault`/`transientEmpty`/`transientNone = true`, etc.) for users who want byte-for-byte upstream behavior.

---

## mock

**Modelled on:** [ScalaMock](https://github.com/ScalaMock/ScalaMock) (classic `mock`/`stub` API; checked against the
cases of ScalaMock 7.6.3's own suite in `mock/src/test/scala/hearth/kindlings/mock/ScalaMockParitySpec.scala` and
`MockSpec`). An independent reimplementation, not a drop-in replacement.

| Feature | ScalaMock | Kindlings | Status |
|---|---|---|---|
| `mock[T]` / `stub[T]`, `expects`/`returning`/`throwing`/`onCall`, `when`/`verify` | Yes | Yes (faithful and name-keyed DSLs) | Parity |
| Call counts, `inSequence`/`inAnyOrder` (nested), argument matchers (`*`, `~`, `where`, captures) | Yes | Yes | Parity |
| Overloads, polymorphic/curried methods, bounds, `ClassTag`, varargs, by-name/implicit/default parameters, operators | Yes | Yes | Parity |
| Abstract `val`s (default value, no expectation) | Yes | Yes | Parity (fixed: a strict mock used to throw on construction) |
| Concrete members mocked | Yes | No (keep their implementation) | Difference (by design) |
| Members typed with an inner type of the mocked trait | Yes | No | Gap (Hearth, `docs/research/hearth-gap-anonymous-instance-inner-types.md`) |
| `mockFunction` / `stubFunction`, call log, new `stubs` API, ZIO/cats-effect integrations, Java proxy mocks | Yes | No | Not ported |
| Scala.js / Scala Native | JS (macro backend) | Yes | Improvement |

---

## optics

**Replaces:** [SoftwareMill quicklens](https://github.com/softwaremill/quicklens) (`modify`/`modifyAll`/`modifyLens`
paths). An independent reimplementation with the same DSL names, for Scala 2.13 and 3 on JVM, Scala.js and Scala Native.

| Feature | quicklens | Kindlings | Status |
|---|---|---|---|
| `obj.modify(_.a.b)` and `modify(obj)(_.a.b)` with `using`/`apply`/`setTo`/`setToIfDefined`/`setToIf`/`usingIf` | Yes | Yes | Parity |
| `.each` / `.eachWhere` over `Seq`s, `Array`, `Option`, `Map` values | Yes (`Map.eachWhere` only on Scala 3) | Yes, on every container an `IsCollection`/`IsMap`/`IsOption` provider supports (`Set`, java and cats collections, ...) | Improvement |
| `.eachWhere(p)` with a predicate capturing local values | Yes | Yes | Parity |
| `.at(i)` / `.index(i)` / `.atOrElse(i, d)` over `Seq`s (immutable or not) and `Array` | `.at` throws `IndexOutOfBoundsException` (`ArrayIndexOutOfBoundsException` for an `Array`), `.atOrElse` appends `f(d)` when absent | Same | Parity |
| `.at(k)` / `.index(k)` / `.atOrElse(k, d)` over `Map`s, `.at` / `.index` / `.atOrElse(d)` over `Option` | `.at` throws `NoSuchElementException`; the default is by-name | Same | Parity |
| `.eachLeft` / `.eachRight` | Yes | Yes | Parity |
| `.when[Subtype]`, common fields of sealed hierarchies | Yes | Yes (also Scala 3 `enum`s) | Parity |
| `modifyAll`, `modifyLens`, `modifyAllLens` | Yes | Yes | Parity |
| `modifyAll`: shared prefixes rebuilt once, shared `.each`/`.eachLeft`/`.eachRight`/`.when[T]` traversed once, overlapping paths applied in the order written | Yes (Scala 3; one modification after another on Scala 2) | Yes, on Scala 2 and 3 | Parity |
| `andThenModify` on `PathLazyModify` and on `T => PathModify[T, U]` (`modify(_: T)(_.a)`) | Yes | Yes | Parity |
| Classes with a hand-written `copy` (also a case class' own `copy` replacement) | Yes | Yes | Parity |
| Case class with a second, implicit parameter list | Yes (a non-implicit one is an error) | Same | Parity |
| Intersection types and self types (`A & B`, `this.modify` with a self type) | Yes (Scala 3) | Yes, on Scala 2 (`A with B`) and 3 | Parity |
| Tuples, type aliases, type-parameterised case classes | Yes | Yes | Parity |

### Differences

| Feature | quicklens | Kindlings | Notes |
|---|---|---|---|
| Support for a custom container | User-defined `QuicklensFunctor` / `QuicklensIndexedFunctor` / `QuicklensEitherFunctor` (Scala 3), `QuicklensFunctor` / `QuicklensAtFunctor` / `QuicklensMapAtFunctor` (Scala 2) instances | Hearth's `IsCollection`/`IsMap`/`IsOption` providers, the extension point shared with all derivation modules | Design decision: no optics-specific type classes |
| Rebuilding a plain case class | `copy(field = ...)` | Constructor call (what the generated `copy` does); a hand-written `copy` is called | Same result, no default-argument accessors |

### Not ported

| Feature | Notes |
|---|---|
| `copy` / field accessors defined as Scala 3 extension methods (also on opaque types) | quicklens for Scala 3 only; Hearth cannot call an extension method's default arguments yet (`docs/research/hearth-bug-case-field-overloaded-by-method.md`) |
| A focus with a literal type (`modify["foo"](_.f)`) | quicklens for Scala 3 only; the compiler constant-folds the path before the macro sees it |

### Checked against upstream

Two regression nets:

- **quicklens' own test suite** (1.9.15, the shared and the Scala 3 suites) is ported into the module's tests
  (`optics/src/test/scala*/hearth/kindlings/optics/upstream/`): only the import and the test framework differ. It runs
  on Scala 2.13 and 3 (the Scala 3-only suites on Scala 3). The suites of the features listed above as different or
  not ported are the only ones left out.
- `upstream-oracle-tests` (`OpticsOracleSpec`) applies the same paths and terminal operations with Kindlings and with
  quicklens to the same values — including missing indices/keys, empty options, `Left`s, sealed hierarchies, arrays,
  captured local values, `modifyAll` ordering, hand-written `copy` — and compares the results and the exceptions
  thrown. The one expected difference:

    - `.index(i)` with a **negative** index is a no-op, as in quicklens for Scala 3; quicklens for Scala 2 throws
      `IndexOutOfBoundsException` there (it only checks `i < size`), so on Scala 2.13 this differs.

---

## pureconfig-derivation

**Replaces:** PureConfig's auto/semi-automatic derivation surfaces:
- `pureconfig.generic.semiauto.{deriveReader, deriveWriter, deriveConvert}` (Scala 2 + Scala 3, Shapeless / Magnolia based)
- `import pureconfig.generic.auto._` (Scala 2 only — fully recursive Shapeless auto-derivation)
- native `derives ConfigReader` (Scala 3 only — works without any import since 0.17.7; the older `pureconfig.generic.derivation.default._` import is now `@deprecated`)

The kindlings type classes are **subtypes** of the upstream ones:

```scala
trait KindlingsConfigReader[A] extends pureconfig.ConfigReader[A]
trait KindlingsConfigWriter[A] extends pureconfig.ConfigWriter[A]
trait KindlingsConfigConvert[A] extends pureconfig.ConfigConvert[A]
```

A derived instance is therefore a drop-in replacement for the upstream interface — it can be passed to `pureconfig.ConfigSource.string("…").load[A]` and any other PureConfig user code with no adaptation. All of PureConfig's built-in primitive / collection / `java.time` / `URI` / `Path` / `UUID` instances are reused via implicit search; the kindlings macro does not re-implement them and they live behind the same import path users already know.

### Type classes

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| `ConfigReader[A]` | Yes | Yes (`KindlingsConfigReader extends ConfigReader`) | Parity |
| `ConfigWriter[A]` | Yes | Yes (`KindlingsConfigWriter extends ConfigWriter`) | Parity |
| `ConfigConvert[A]` | Yes (`extends Reader with Writer`) | Yes (`KindlingsConfigConvert extends ConfigConvert`) | Parity |
| `map`/`emap` on `ConfigReader` | Yes | Inherited from upstream | Parity |
| `contramap` on `ConfigWriter` | Yes | Inherited from upstream | Parity |
| `xmap`/`xemap` on `ConfigConvert` | Yes | Inherited from upstream | Parity |

### Derivation API

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Semi-automatic (`deriveReader[A]`) | `semiauto.deriveReader[A]` (Scala 2 + 3) | `KindlingsConfigReader.derived[A]` | Parity |
| Automatic (full recursion via import) | `pureconfig.generic.auto._` (Scala 2 only — Shapeless) | `KindlingsConfigReader.derived[A]` (sanely-automatic, both Scala 2 + 3) | Improvement |
| Scala 3 native `derives` clause | `derives ConfigReader` via `pureconfig.generic.derivation.default` (Scala 3 only) | `derives KindlingsConfigReader` (Scala 3; on Scala 2.13, which has no `derives`, `KindlingsConfigReader.derived[A]` is an implicit def) | Parity |
| Unified Scala 2 + 3 API | No — three separate derivation modules with overlapping APIs and feature gaps | Yes — single API, same call shape on both | Improvement |
| Scala 3 native `derives` for Writer/Convert | Native `derives` / `derivation.default` supports only `ConfigReader`; `ConfigWriter`/`ConfigConvert` require the separate Magnolia `pureconfig-generic-scala3` module (`semiauto.deriveConvert`) | `derives KindlingsConfigWriter` / `KindlingsConfigConvert` on Scala 3 (`.derived[A]` on Scala 2.13) | Improvement |
| Recursive types | Works on Scala 2 (Shapeless `Lazy`); Scala 3 native derivation has limitations | Works without `Lazy` or wrappers on both | Improvement |
| `ProductHint`/`CoproductHint` respected on Scala 3 | Native `derives` ignores all hints in scope | `KindlingsProductHint`/`KindlingsCoproductHint` honored on both Scala 2 + 3 | Improvement |

### Configuration

PureConfig configures derivation via implicit `ProductHint[A]` and `CoproductHint[A]` values placed in the type's companion object, plus the upstream `ConfigFieldMapping` / `CamelCase` / `KebabCase` / etc. naming conventions. Kindlings ships **both layers**: a global `PureConfig` config class threaded as an implicit (matching the convention used by `kindlings-circe-derivation` and `kindlings-yaml-derivation`) **and** type-indexed `KindlingsProductHint[A]` / `KindlingsCoproductHint[A]` for per-type overrides. Defaults match upstream PureConfig exactly: fields are `ConfigFieldMapping(CamelCase, KebabCase)`, subtypes are `ConfigFieldMapping(PascalCase, KebabCase)`, the discriminator is `Some("type")`, `useDefaults` is `true`, and `allowUnknownKeys` is `true`.

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Field name transforms (camelCase ↔ snake_case ↔ kebab-case ↔ …) | `ConfigFieldMapping` + `ProductHint` per type | Same `pureconfig.ConfigFieldMapping` reused; placed on `PureConfig.transformMemberNames` for the global default and on `KindlingsProductHint[A].transformMemberNames` for per-type overrides | Parity |
| Constructor (subtype) name transforms | `FieldCoproductHint.transformConstructorNames` per ADT | Same `ConfigFieldMapping` reused; placed on `PureConfig.transformConstructorNames` globally and on `KindlingsCoproductHint.Field[A]` / `KindlingsCoproductHint.Wrapped[A]` per ADT | Parity |
| Discriminator field | `FieldCoproductHint(fieldName)` (default `"type"`) | `PureConfig.discriminator = Some("type")` global default; `KindlingsCoproductHint.Field[A](fieldName = …)` per ADT override | Parity |
| Single-key sealed-trait wrapping | Custom `CoproductHint` (or `EnumCoproductHint`) | `PureConfig.discriminator = None` (`.withWrappedSubtypes`) globally; `KindlingsCoproductHint.Wrapped[A]()` per ADT | Parity |
| Use case-class default values for missing fields | `ProductHint(useDefaultArgs = true)` (default) | `PureConfig.useDefaults = true` (default, matches upstream); `KindlingsProductHint[A](useDefaults = …)` per type | Parity |
| Strict mode (reject unknown keys) | `ProductHint(allowUnknownKeys = false)` per type | `PureConfig.allowUnknownKeys = false` global (`.withStrictDecoding`); `KindlingsProductHint[A](allowUnknownKeys = false)` per type — one `UnknownKey` failure per unknown key, after the field failures, as upstream | Parity |
| Per-type customization (different config per type) | Yes (per-companion `ProductHint` / `CoproductHint`) | Yes (`implicit val hint: KindlingsProductHint[MyType]` / `KindlingsCoproductHint.Field[MySealed]` resolved by the macro at derivation site, overriding the global `PureConfig`) | Parity |
| First-success sealed families (no disambiguation) | `FirstSuccessCoproductHint` — subtypes attempted in the alphabetical order of their names, first success wins, `NoValidCoproductOptionFound` otherwise; written without disambiguation | `KindlingsCoproductHint.FirstSuccess[A]()` — same order, failure and output | Parity |

### Annotations

PureConfig itself does not use annotation decorators — all customization is done through implicit hints in companion objects. Kindlings adds annotations on top so users can keep field-rename and transient logic next to the field declaration:

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Per-field rename | None (configured via `ProductHint` field-name mapping) | `@configKey("user_name")` (both Scala 2 + 3) | Improvement |
| Per-subtype rename | Via `FieldCoproductHint.fieldValue` override | `@configKey("variant-name")` (both Scala 2 + 3) | Improvement |
| Transient field | None (no built-in annotation) | `@transientField` (requires default value, both Scala 2 + 3) | Improvement |

### Type support

Type-class instances for primitives, collections, java.time, `java.net`, `java.nio.file.Path`, `UUID`, and friends are inherited from PureConfig itself — extending `pureconfig.ConfigReader` means implicit search picks up everything PureConfig already ships. Kindlings adds the same type-shape support its sister modules provide via the macro:

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Primitives, `String`, `BigDecimal`, `BigInt` | Yes (via `BasicReaders`) | Reused from upstream | Parity |
| `Option[A]` (missing key → `None`) | Yes (via `optionReader`) | Reused from upstream | Parity |
| `ReadsMissingKeys` readers of field types | A reader that is a `ReadsMissingKeys` is given the missing key; any other reader (also a user-provided one of an `Option` type) gets `KeyNotFound` | Same | Parity |
| `WritesMissingKeys` writers of field types | A writer that is a `WritesMissingKeys` decides whether the key is written (`toOpt`); any other writer (also a user-provided one of an `Option` type) is always written | Same | Parity |
| Lazily evaluated default values | Scala 3: defaults are evaluated only for missing keys; Scala 2 (Shapeless) evaluates every default on every read | Evaluated only for missing keys, on both | Parity (Scala 3); Improvement (Scala 2) |
| `List`/`Vector`/`Seq`/`Set`/`Array` | Yes (via `CollectionReaders`) | Reused from upstream | Parity |
| `Map[String, A]` | Yes | Reused from upstream | Parity |
| `URL`, `URI`, `Path`, `File`, `UUID`, `Pattern`, `Regex` | Yes | Reused from upstream | Parity |
| Java time (`Instant`, `Duration`, `Period`, `ZoneOffset`, `ZoneId`, `Year`, …) | Yes | Reused from upstream | Parity |
| `FiniteDuration`, `ConfigMemorySize` | Yes | Reused from upstream | Parity |
| Case classes → HOCON object | Via Shapeless / Magnolia | Via Hearth macro (cleaner generated code, faster compile) | Parity |
| Sealed traits → discriminator object | Via `FieldCoproductHint` | Via macro + `discriminator` config | Parity |
| Sealed traits → single-key wrapping | Via custom `CoproductHint` | Via macro + `withWrappedSubtypes` | Parity |
| Case-object enums (`North`/`South`/…) → HOCON string | `deriveEnumerationReader[E]` (separate API) | Same `KindlingsConfigReader.derived[E]` / `KindlingsConfigWriter.derived[E]` with `withWrappedSubtypes` (no discriminator): case-object families read and write strings, as `deriveEnumerationReader`/`Writer`; with the default discriminator they are `{ type = north }` objects, as upstream's default `FieldCoproductHint` | Improvement |
| Singleton (`case object`) members | Yes | Yes | Parity |
| Tuples (`TupleN`) | Lists: `(true, 5)` is `[true, 5]`; `WrongSizeList` for another size. Scala 2 (Shapeless) also reads the product representation `{ _1 = ..., _2 = ... }` when the value is an object and not a list; Scala 3 reads only lists | Read from and written as lists, same failures; the product representation is read on both Scala versions (as upstream on Scala 2) | Parity (Scala 2); Difference (Scala 3: also reads `{ _1, _2 }` objects) |
| Case objects / case classes without parameters read on their own | Must be an object; `ProductHint(allowUnknownKeys = false)` reports every key | Same (`KindlingsProductHint` or the global `allowUnknownKeys`); for a subtype of a sealed family every key but the discriminator | Parity |
| Value classes / `AnyVal` wrappers | Manual `ConfigReader.fromString` per type | Automatic unwrapping via `IsValueType` (covers `AnyVal`, `extends AnyVal`, refined types via `refined-integration`, opaque types) | Improvement |
| Refined types (`eu.timepit.refined`) | Separate `pureconfig-refined` module | Automatic via `kindlings-refined-integration` (`Either[String, A]` wrap detected by `IsValueType`) | Improvement |
| Iron (Scala 3 opaque types with constraints) | Separate `pureconfig-iron` module | Automatic via `kindlings-iron-integration` | Improvement |
| neotype (Scala 3 `Newtype`/`Subtype`) | Manual `ConfigReader`/`ConfigWriter` per type | Automatic via `kindlings-neotype-integration` (validated with the companion's `make`) | Improvement |
| scala-newtype (`@newtype`/`@newsubtype`) | Manual (e.g. `deriving` from the underlying instance) per type | Automatic via `kindlings-newtype-integration` (JVM) | Improvement |
| Scala 3 named tuples | Not supported | Yes | Improvement |
| Scala 3 union types (`A \| B`) | Limited | Yes | Improvement |
| Recursive types | Works (Shapeless on Scala 2; Scala 3 native has limitations) | Works without `Lazy` or wrappers on both | Improvement |

### Cross-compilation

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2 + 3 API | No — `pureconfig-generic` (Scala 2, Shapeless) and `pureconfig-generic-scala3` (Scala 3, Magnolia / native `derives`) are separate modules with overlapping but non-identical features | Yes — single `kindlings-pureconfig-derivation` module compiled for both | Improvement |
| Cross-platform | JVM only (transitive `com.typesafe:config` dependency is JVM-only) | JVM only (same reason) | Parity |

### Interop with regular PureConfig user code

Because the kindlings type classes extend the upstream ones, derived instances are usable wherever a `pureconfig.ConfigReader[A]` (or `Writer[A]` / `Convert[A]`) is expected. This is verified by `KindlingsConfigConvertSpec` test "interop: kindlings reader works with pureconfig.ConfigSource":

```scala
implicit val reader: KindlingsConfigReader[SimplePerson] =
  KindlingsConfigReader.derived[SimplePerson]
val loaded = pureconfig.ConfigSource
  .string("name = Alice, age = 30")
  .load[SimplePerson]   // resolves `reader` via implicit search
loaded ==> Right(SimplePerson("Alice", 30))
```

### `summonExprIgnoring` filter

To prevent infinite macro expansion, the kindlings macro filters out its own `derived` companion methods during implicit search (`KindlingsConfigReader.type` and `KindlingsConfigConvert.type` on the reader side; `KindlingsConfigWriter.type` and `KindlingsConfigConvert.type` on the writer side). Users who also enable upstream auto-derivation — `pureconfig.generic.auto._` (Scala 2) or a `derives ConfigReader` clause on the type itself (Scala 3; the `pureconfig.generic.derivation.default._` import is deprecated since 0.17.7) — will get whichever derivation rule is more specific in implicit resolution; if both are in scope and you want the kindlings derivation, do not also import the upstream auto-derivation surfaces.

### Checked against upstream (oracle)

`upstream-oracle-tests` (`PureConfigOracleSpec`) derives the same fixtures with Kindlings and with pureconfig's own
derivation (`pureconfig.generic.semiauto`: Shapeless on Scala 2, the hint-aware `pureconfig-generic-scala3` on Scala 3,
with the equivalent `ProductHint` / `FieldCoproductHint` / `FirstSuccessCoproductHint`, and `deriveEnumerationReader` /
`deriveEnumerationWriter` for case-object enumerations), and compares the values read from HOCON — including every
failure, its reason, path and origin — and the config values written. There are **no expected differences**:

- every failure is reported, not only the first one: every field of a case class (then the unknown keys of a strict
  product), every item of a collection and every value of a map;
- a missing key is `KeyNotFound` with upstream's candidate keys (`KeyNotFound.forKeys`), passed to readers of missing
  keys (`ReadsMissingKeys`, e.g. `Option`) like upstream;
- with a discriminator (upstream's `FieldCoproductHint`): the value must be an object (also for case objects), the
  subtype is read without the discriminator key (so a strict subtype does not report it, and a strict case class read
  on its own does), an unknown value is `UnexpectedValueForFieldCoproductHint` at the discriminator, and writing a
  subtype that has a field named like the discriminator throws upstream's `CoproductHintException`;
- without a discriminator, a family of case objects reads only strings (upstream's `deriveEnumerationReader`; an
  unknown value is `NoValidCoproductOptionFound` on Scala 2 and `CannotConvert` on Scala 3, as upstream on each);
- an empty `Option` field is omitted when writing (upstream's `WritesMissingKeys`), not written as `null`; a
  user-provided writer of a field's type is asked (`WritesMissingKeys.toOpt`) or, when it is a plain writer, always
  written;
- tuples are lists; case objects and case classes without parameters are read only from objects and honour
  `allowUnknownKeys = false`.

`PureConfigUpstreamSuitesOracleSpec` and (Scala 3) `PureConfigScala3OracleSpec` run the cases of upstream's own
derivation suites (`ProductHintSuite`, `CoproductHintSuite`, `ProductConvertersSuite` / `ProductConvertDerivationSuite`,
`CoproductConvertersSuite` / `CoproductConvertDerivationSuite`, `TupleConvertersSuite`, `EnumerationsSuite` /
`EnumDerivationSuite`, `ValueClassSuite`, `DerivationModesSuite`) through both libraries, failures' origins included.
The **only expected difference**: on Scala 3 Kindlings also reads a tuple from its product representation
(`{ _1 = ..., _2 = ... }`), which upstream reads on Scala 2 only.

The Kindlings-only additions stay opt-in: `withWrappedSubtypes` for families that are not all case objects (single-key
objects, no upstream counterpart), `@configKey`, `@transientField`.

### Not ported

| Feature | Notes |
|---|---|
| `pureconfig-cats` / `pureconfig-magnolia` / `pureconfig-shapeless` modules | External ecosystem add-ons. Cats data types (`NonEmptyList`, etc.) are reachable via `kindlings-cats-integration`. |
| `pureconfig-yaml` / `pureconfig-circe` etc. format adapters | Out of scope — kindlings ships separate `kindlings-yaml-derivation` and `kindlings-circe-derivation` modules. |
| `ConfigSource` API (`ConfigSource.file`, `ConfigSource.url`, …) | Not duplicated — users keep using `pureconfig.ConfigSource.*` and pass our derived instances to it. |
| Custom `FailureReason` subtypes | Inherited from upstream when summoning user-provided `ConfigReader`s. The kindlings macro itself reports upstream's failure reasons (`KeyNotFound`, `WrongType`, `UnknownKey`, `UnexpectedValueForFieldCoproductHint`, `NoValidCoproductOptionFound`, …; the sealed-family ones come from the small `pureconfig-generic-base` module, now a dependency). |
| Shapeless `HList` / `Coproduct` instances (`pureconfig.generic.hlist`) | Not applicable: no Shapeless (derivation mechanism). |
| Custom `ProductHint` / `CoproductHint` implementations | Kindlings reads its own `KindlingsProductHint` / `KindlingsCoproductHint` (the configuration of upstream's built-in hints), not arbitrary `from`/`to` implementations. |

---

## scalacheck-derivation

**Replaces:** manual `Arbitrary` instance writing, [`scalacheck-shapeless`](https://github.com/alexarchambault/scalacheck-shapeless) (Scala 2 only, Shapeless-based; cross-publishes JVM/JS/Native on Scala 2), [`scalacheck-derived`](https://github.com/martinhh/scalacheck-derived) (Scala 3)

### Type classes

| Feature | Manual / scalacheck-shapeless | Kindlings | Status |
|---|---|---|---|
| `Arbitrary[A]` | Shapeless-derived (`derivedArbitrary`) | Yes | Parity |
| `Shrink[A]` | Shapeless-derived (`derivedShrink`) | Yes | Parity |
| `Cogen[A]` | Shapeless-derived (`derivedCogen`) | Yes | Parity |

### Derivation API

| Feature | scalacheck-shapeless | Kindlings | Status |
|---|---|---|---|
| Semi-automatic (`DeriveArbitrary.derived[A]`) | No | Yes | Improvement |
| Semi-automatic (`DeriveShrink.derived[A]`) | No | Yes | Improvement |
| Semi-automatic (`DeriveCogen.derived[A]`) | No | Yes | Improvement |
| `Arbitrary.derived[A]` extension | No | Yes (via `import extensions._`) | Improvement |
| `Shrink.derived[A]` extension | No | Yes (via `import extensions._`) | Improvement |
| `Cogen.derived[A]` extension | No | Yes (via `import extensions._`) | Improvement |
| `derives Arbitrary` (Scala 3) | No | Yes | Improvement |
| `derives Shrink` (Scala 3) | No | Yes | Improvement |
| `derives Cogen` (Scala 3) | No | Yes | Improvement |
| Automatic (fully implicit) | Yes (import and forget) | No — sanely-automatic via `.derived` | Trade-off |
| Unified Scala 2+3 API | No (Scala 2 only) | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | Yes, but Scala 2 only | JVM + JS + Native on Scala 2 **and** 3 | Improvement |

### Type support

| Feature | Manual / scalacheck-shapeless | Kindlings | Status |
|---|---|---|---|
| Primitives (`Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String`, `Unit`) | ScalaCheck built-in | Yes (built-in rule) | Parity |
| `BigInt`, `BigDecimal` | ScalaCheck built-in | Yes (built-in rule) | Parity |
| Case classes | scalacheck-shapeless: yes; manual: tedious | Yes | Parity |
| Sealed traits (case objects only) | scalacheck-shapeless: yes | Yes (`Gen.oneOf`) | Parity |
| Sealed traits (mixed case objects + case classes) | scalacheck-shapeless: yes | Yes | Parity |
| Zero-field case classes | Manual: trivial | Yes (`Gen.const`) | Parity |
| Nested case classes | scalacheck-shapeless: yes | Yes | Parity |
| `Option[A]` | ScalaCheck built-in | Yes (`Gen.option` / `Shrink` / `Cogen`) | Parity |
| Collections (`List`, `Vector`, `Set`, …) | ScalaCheck built-in for some | Yes (via Hearth `IsCollection`) | Parity |
| `Map[K, V]` and map-like types | ScalaCheck built-in for `Map` | Yes (via Hearth `IsMap`) | Parity |
| Value classes / opaque types | Manual only | Yes (via Hearth `IsValueType`) | Improvement |
| Refined types | Manual only | Yes (via `kindlings-refined-integration` + `IsValueType`) | Improvement |
| Iron types (Scala 3) | Manual only | Yes (via `kindlings-iron-integration` + `IsValueType`) | Improvement |
| Java enums | Manual only | Yes (via Hearth `Enum.parse`; JVM-only, both Scala 2.13 & 3) | Improvement |
| Scala `Enumeration` | Manual only | Yes | Improvement |
| Recursive types | scalacheck-shapeless: needs an explicit `Recursive[T]` to bound recursion (else `StackOverflowError`) | Yes (no wrappers needed: sizes shrink with depth and at size 0 only non-recursive cases are generated) | Improvement |
| `Shrink` for collections | ScalaCheck's `shrinkContainer` | Same algorithm (same candidates, same order) | Parity |
| `Cogen` for products, `Option`, collections | ScalaCheck's instances; case classes: every field, then `Cogen[Unit]` | Same perturbation (same seeds) | Parity |
| User-provided implicits | Implicit search | Yes (checked before derivation) | Parity |
| Literal types (`"foo"`, `42`) | scalacheck-shapeless: `Witness` singleton types; scalacheck-derived-extras: `arbLiteral` / `cogenLiteral` | Yes (`Gen.const`, `Cogen` reseeding with `0L`, on Scala 2.13 and 3) | Parity |
| Union types (Scala 3) | scalacheck-derived-extras: `arbUnion` / `cogenUnion` / `shrinkUnion` | Yes (same values, seeds and shrinks) | Parity |
| Nested sealed traits (incl. diamonds) | scalacheck-derived: flattened, every leaf subtype once with the same chance | Same | Parity |
| Same generated values for the same seed | scalacheck-derived: same as hand-written generators from ScalaCheck's instances; scalacheck-shapeless: splits the size among fields | Same as scalacheck-derived and ScalaCheck's own instances (containers of recursive types excepted) | Parity (Scala 3) / Difference (Scala 2, see below) |
| `RecursionFallback[T]` / `Recursive[T]` (what to generate when the size runs out) | Yes | Not needed: at size 0 only the subtypes that cannot recurse are generated | Difference: derivation mechanism |
| `ArbitraryDeriving.buildSumGen` (custom combination of a sum type's subtype generators, scalacheck-derived 0.10+) | Yes | No | Not supported |
| `deriveArbitraryShallow` / `deriveArbitraryExtraShallow` | Yes | No (one `derived`, missing instances are derived) | Not applicable: derivation mechanism |

### Upstream oracle

`upstream-oracle-tests` (`ScalaCheckOracleSpec`) compares Kindlings with scalacheck-shapeless on Scala 2.13 and with
[scalacheck-derived](https://github.com/martinhh/scalacheck-derived) (the Scala 3 equivalent) on Scala 3, each used as
documented (`Recursive` / `RecursionFallback` for the recursive type). Generators are random, so it compares properties:
subtype coverage, collection/string sizes within `Gen.size`, generation at every size including 0, termination of
recursive types - and the generated values themselves for 200 fixed seeds and sizes (what both upstream suites check
against hand-written generators); `Shrink` streams and `Cogen` seeds are deterministic and compared exactly for fixed
values. Where the two upstreams disagree Kindlings follows scalacheck-derived. Documented differences:

- **Size 0** (Scala 3): scalacheck-derived returns `Gen.fail` for some sealed-trait-based types once the size is
  exhausted (even with a `RecursionFallback`); Kindlings still generates a value.
- **Generated values** (Scala 2): scalacheck-shapeless gives each case class field about `1 / (n + 1)` of the size and
  picks subtypes through nested `Gen.frequency`s; Kindlings, like scalacheck-derived and hand-written generators, gives
  every field the whole size and picks a subtype with `Gen.oneOf`, so the values generated for a seed differ (on
  Scala 3 they are the same as scalacheck-derived's).
- **Shrinking sealed traits** (Scala 2): scalacheck-shapeless also shrinks a value into the other subtypes (e.g. a
  `Circle` into a case object); Kindlings, like scalacheck-derived, shrinks only within the value's subtype.
- **`Cogen` for sealed traits** (Scala 2): scalacheck-shapeless advances the seed once per preceding subtype; Kindlings,
  like scalacheck-derived, perturbs with the subtype's `Cogen` and then with its index.

### Not supported

| Feature | Notes |
|---|---|
| Configuration class | No configuration knobs (no field name transforms, etc. — generators don't need them) |
| Annotations (`@fieldName`, `@transientField`) | Not applicable — generators don't interact with serialized field names |

### Debugging

| Feature | Description |
|---|---|
| Debug logging (import) | `import hearth.kindlings.scalacheckderivation.debug.logDerivationForScalaCheckDerivation` |
| Debug logging (scalac option) | `-Xmacro-settings:scalacheckDerivation.logDerivation=true` |

---

## sconfig-derivation

**Original type class** — no replacement target. PureConfig's underlying parser, `com.typesafe:config`, is JVM-only, which locks PureConfig out of Scala.js and Scala Native projects. `sconfig-derivation` fills that gap by deriving config readers/writers/codecs against [`org.ekrich:sconfig`](https://github.com/ekrich/sconfig) — a Scala port of typesafe-config that runs on JVM, Scala.js, and Scala Native — while sharing the same HOCON syntax, the same `Config` / `ConfigValue` shape, and the same derivation knobs as `kindlings-pureconfig-derivation` (so users can mentally swap between the two depending on their target platform).

Because no upstream library does derivation against sconfig today, this module is "exceeds upstream" by definition: there is no upstream oracle to compare against. Its **behaviour follows upstream PureConfig's**, as matched by `kindlings-pureconfig-derivation` (which is checked against PureConfig by `PureConfigOracleSpec`). The regression net is `UpstreamSemanticsSpec` and `UpstreamSuitesSpec` in sconfig-derivation's shared tests: they run the scenarios of pureconfig-derivation's specs of the same names (the latter: the cases of upstream's own derivation suites) and assert the same outcomes (values read, failure kinds and paths, written output), with sconfig's own failure types. The tables below describe the surface area we ship.

### Semantics shared with PureConfig

| Behaviour | Description |
|---|---|
| Failure accumulation | Every failing field of a case class is reported, then each unknown key of a strict one; every failing item of a collection and value of a map |
| Missing keys | `KeyNotFound` at the object that lacks the key, with upstream's candidate keys (`KeyNotFound.forKeys`); readers of missing keys (`Option` fields, `ReadsMissingKeys` readers) read the missing key instead (an `Option` reads it as `None`) |
| Failure paths | A missing key at the object, any other failure at the offending value (`path` lists keys / list indices from the value the reader was called with — sconfig readers read a `ConfigValue`, not a cursor) |
| Value that is not an object | Fails once (`WrongType`), not once per field |
| Discriminator (`CoproductHint.Field`, default `type`) | The value must be an object; the subtype is read without the discriminator key (strict subtypes don't see it as unknown); an unknown value fails with `UnexpectedValueForFieldCoproductHint` at the discriminator; writing a subtype that has a field named like the discriminator throws `CoproductHintException(CollidingKeys)` |
| No discriminator | A family of case objects is read only from strings (like `deriveEnumerationReader`; an unknown string fails with `CannotConvert`), other families from single-key objects |
| `CoproductHint.FirstSuccess` | Subtypes are attempted in the alphabetical order of their names (upstream's order), the first success wins; when none succeeds, `NoValidCoproductOptionFound` lists every attempt; written without disambiguation |
| Empty `Option` fields | Omitted when writing (upstream's `WritesMissingKeys`), not written as `null` |
| Readers / writers of missing keys | The instance in scope for a field's type decides, as upstream: a `ReadsMissingKeys` reader (the built-in `Option` reader, user-provided ones) is given the missing key (`null`), any other reader - also a user-provided plain reader of an `Option` type - gets `KeyNotFound`; a `WritesMissingKeys` writer (the built-in `Option` writer, user-provided ones) decides whether the key is written (`toOpt`), any other writer is always written |
| Tuples (`TupleN`) | Read from and written as lists (`(true, 5)` is `[true, 5]`); a list of another size fails with `WrongSizeList`, every failing element is reported under its index, a value that is neither a list nor an object with `WrongType` (expecting a list). A value that is an object is read from the product representation (`{ _1 = ..., _2 = ... }`), as upstream's Scala 2 derivation does (on both Scala versions). Unlike PureConfig's cursors, an object with numeric keys (`{ "0" = ..., "1" = ... }`) is not treated as a list (sconfig-derivation's collections do not either) |
| Types without fields | A case object or a case class without parameters is read only from an object; with `allowUnknownKeys = false` every key of it is an `UnknownKey` (for a subtype of a sealed family: every key but the discriminator) |
| Default values | Evaluated only when the key is missing |

The unknown-string failure of a case-object family differs between PureConfig's Scala versions (`NoValidCoproductOptionFound` on 2.13, `CannotConvert` on 3); sconfig-derivation reports `CannotConvert` on both.

### Type classes

Owned by the kindlings module — no extending of any third-party type, just a clean three-trait hierarchy:

| Type class | Description |
|---|---|
| `ConfigReader[A]` | `def from(value: ConfigValue): Either[ConfigDecodingError, A]` |
| `ConfigWriter[A]` | `def to(value: A): ConfigValue` |
| `ConfigCodec[A]` | `extends ConfigReader[A] with ConfigWriter[A]` — single value usable as both |

`ConfigReader[A]` exposes `map(f: A => B)` and `emap(f: A => Either[String, B])` combinators; `ConfigWriter[A]` exposes `contramap(f: B => A)`.

### Error model

| Feature | Description |
|---|---|
| `ConfigDecodingError` | Sealed trait with structured variants and a `path: List[String]` for nested-field error attribution |
| `ConfigDecodingError.KeyNotFound(path, key, candidates)` | Required key absent on a HOCON object (upstream `KeyNotFound`, with its candidate keys) |
| `ConfigDecodingError.Missing(path, key)` | Required key absent; kept for hand-written readers, derived readers report `KeyNotFound` |
| `ConfigDecodingError.WrongType(path, expected, actual)` | Value present but the wrong `ConfigValueType` (upstream `WrongType`) |
| `ConfigDecodingError.CannotConvert(path, message, cause)` | Catch-all for `String => A` conversion failures (parse errors, refined-type validation, unknown enumeration strings, …) |
| `ConfigDecodingError.WrongSizeList(path, expected, found)` | A tuple read from a list of another size (upstream `WrongSizeList`) |
| `ConfigDecodingError.UnknownKey(path, key)` | Unknown key of a strict case class, at the key (upstream `UnknownKey`) |
| `ConfigDecodingError.UnexpectedValueForFieldCoproductHint(path, value)` | Discriminator value naming no subtype, at the discriminator (upstream's failure of the same name) |
| `ConfigDecodingError.NoValidCoproductOptionFound(path, value, optionFailures)` | No subtype of a `FirstSuccess` family could be read (upstream's failure of the same name) |
| `ConfigDecodingError.CollidingKeys(path, key, existingValue)` | Thrown by writers inside `CoproductHintException` when a subtype's field collides with the discriminator (upstream `CollidingKeys`) |
| `ConfigDecodingError.Multiple(errors)` | Aggregates multiple errors so one decode reports every problem at once (`ConfigDecodingError.flatten` lists them) |
| `withParentPath(parent)` | Extends the path on the way up the call stack so nested-field errors carry paths like `parent.child` |

### Derivation API

| Feature | Description |
|---|---|
| `ConfigReader.derived[A]` / `ConfigWriter.derived[A]` / `ConfigCodec.derived[A]` | Semi-automatic |
| `ConfigReader.derived[A]` / `ConfigWriter.derived[A]` / `ConfigCodec.derived[A]` | Sanely-automatic — implicit on Scala 3 (`given`), explicit non-implicit on Scala 2 (to avoid clashing with the built-in instances under Scala 2 implicit-search rules) |
| `derives ConfigReader` (Scala 3) | Yes |
| Inline composition of `ConfigCodec.derived` from `ConfigReader.derived` + `ConfigWriter.derived` | Scala 3 — sidesteps sibling-splice isolation by running each derivation as its own top-level macro expansion |
| Single-macro Codec (`ConfigCodec.derived`) | Scala 2 — combines reader + writer in one macro expansion (no sibling-splice issue on Scala 2) |
| Unified Scala 2 + 3 API | Yes |

### Configuration

`SConfig` config class — same shape and **same defaults** as the `PureConfig` class in `kindlings-pureconfig-derivation` so users can move between the two modules with minimal mental overhead:

| Field | Default | Description |
|---|---|---|
| `transformMemberNames` | `ConfigFieldMapping(CamelCase, KebabCase)` | Function applied to every case-class field name when reading/writing the HOCON key. Matches upstream PureConfig's `ProductHint` default. |
| `transformConstructorNames` | `ConfigFieldMapping(PascalCase, KebabCase)` | Function applied to every sealed-trait subtype name. Matches upstream PureConfig's `FieldCoproductHint` default. |
| `discriminator` | `Some("type")` | Discriminator field name. `None` switches to single-key wrapping (`{"VariantName": {…}}`) |
| `useDefaults` | `true` | When a field is missing from the HOCON object, fall back to the case class's compile-time default value. Matches upstream. |
| `allowUnknownKeys` | `true` | When `false`, fails reading if the HOCON object has keys that don't correspond to any case-class field. Matches upstream `ProductHint(allowUnknownKeys = …)`. |
| `withSnakeCaseMemberNames` / `withKebabCaseMemberNames` / `withPascalCaseMemberNames` / `withScreamingSnakeCaseMemberNames` / `withCamelCaseMemberNames` | — | Convenience builders for common case conventions |
| `withSnakeCaseConstructorNames` / `withKebabCaseConstructorNames` | — | Convenience builders for sealed-trait subtype naming |
| `withDiscriminator(field)` / `withWrappedSubtypes` | — | Convenience builders for discriminator-vs-wrapping selection |
| `withUseDefaults` / `withoutUseDefaults` | — | Convenience builders for default-value handling |
| `withAllowUnknownKeys` / `withStrictDecoding` | — | Convenience builders for strict-mode toggling |

Naming conventions live next to `SConfig`: `CamelCase`, `PascalCase`, `KebabCase`, `SnakeCase`, `ScreamingSnakeCase`, plus `ConfigFieldMapping(source, target)` for ad-hoc combinations. The tokeniser is implemented with a manual character-by-character loop (rather than the lookbehind regex upstream uses) so it works on Scala.js without forcing downstream projects into ES2018+ mode.

#### Per-type overrides via `ProductHint` / `CoproductHint`

The global `SConfig` is the default; per-type configuration is expressed via implicit type-indexed hints, mirroring upstream PureConfig's `ProductHint[A]` / `CoproductHint[A]`:

| Hint | Knobs | Replaces |
|---|---|---|
| `ProductHint[A]` | `transformMemberNames` / `useDefaults` / `allowUnknownKeys` | upstream `pureconfig.ProductHint[A]` |
| `CoproductHint.Field[A]` | `fieldName` (default `"type"`) / `transformConstructorNames` | upstream `pureconfig.FieldCoproductHint[A]` |
| `CoproductHint.Wrapped[A]` | `transformConstructorNames` | upstream single-key-wrapping `CoproductHint` |
| `CoproductHint.FirstSuccess[A]` | (no parameters) | upstream `pureconfig.FirstSuccessCoproductHint[A]` — subtypes attempted in alphabetical order of their names, first success wins; written as is |

The macro looks for an implicit hint at every case class / sealed family and falls back to the global `SConfig` when no hint is in scope. Annotations (`@configKey`, `@transientField`) always win over both layers.

### Annotations

| Annotation | Description |
|---|---|
| `@configKey(name)` | Override the HOCON key for a case-class field or a sealed-trait subtype. Takes precedence over `transformMemberNames` / `transformConstructorNames`. |
| `@transientField` | Skip a field during read and write. The field must have a compile-time default value, which is used as the placeholder when reading. |

### Built-in instances

Defined directly in the `ConfigReader` and `ConfigWriter` companion bodies (not in mixed-in traits — Scala 2 macro `Implicits.search` doesn't reliably pick up `implicit val`s declared in inherited traits). They participate in implicit resolution like any other type-class instance:

| Type | Reader | Writer | Notes |
|---|---|---|---|
| `String` | Yes | Yes | Reader accepts numeric and boolean upstream values, coercing to string |
| `Boolean` | Yes | Yes | |
| `Int` / `Long` / `Short` / `Byte` | Yes | Yes | Via `Number.intValue()` etc. |
| `Float` / `Double` | Yes | Yes | |
| `Char` | Yes | Yes | Single-character string |
| `BigDecimal` / `BigInt` | Yes | Yes | Reader accepts both `NUMBER` and `STRING` upstream values |
| `Option[A]` | Yes | Yes | `null` / `ConfigValueType.NULL` ↔ `None`; otherwise recurses into `inner`. The reader is a `ReadsMissingKeys` (a missing key reads as `None`), the writer a `WritesMissingKeys` (the key of an empty `Option` is not written) |

Collection (`List`, `Vector`, `Seq`, `Set`) and `Map[String, A]` readers / writers are deliberately **not** exposed as companion-level implicits because generic `def iterableReader[A, Coll](implicit …): ConfigReader[Coll]` would cause Scala 2's implicit search to diverge whenever it tries to resolve any unrelated `ConfigReader[T]`. Instead, the macro's `HandleAsCollectionRule` and `HandleAsMapRule` synthesise collection codecs directly from the structural pattern, which works for all standard collection types (and any user-defined `IsCollection` / `IsMap` provider) without needing companion-level implicits.

### Type support

| Feature | sconfig (raw) | Kindlings derivation | Notes |
|---|---|---|---|
| Primitives, `String`, `BigDecimal`, `BigInt` | Manual `config.getString(path)` / `getInt(path)` | Built-in instances | |
| `Option[A]` | Manual `if (config.hasPath) … else None` | Built-in instance + macro `HandleAsOptionRule` | Missing key produces `None` automatically when the field type is `Option[T]` |
| `List`/`Vector`/`Seq`/`Set` | Manual iteration over `ConfigList` | Macro `HandleAsCollectionRule` | Works for any `IsCollection` provider |
| `Map[String, A]` | Manual iteration over `ConfigObject` entries | Macro `HandleAsMapRule` | Keys must be `String` |
| Case classes | Manual field-by-field reading | Macro `HandleAsCaseClassRule` | Honours `@configKey`, `@transientField`, `useDefaults` |
| Value classes / `AnyVal` wrappers | Manual unwrap | Automatic via `IsValueType` | |
| Refined types (Scala 2 + Scala 3) | Manual validation | Automatic via `kindlings-refined-integration` (`Either[String, A]` wrap detected) | |
| Iron (Scala 3 opaque types with constraints) | Not supported | Automatic via `kindlings-iron-integration` | |
| neotype (Scala 3 `Newtype`/`Subtype`) | Not supported | Automatic via `kindlings-neotype-integration` (validated with the companion's `make`) | |
| scala-newtype (`@newtype`/`@newsubtype`) | Not supported | Automatic via `kindlings-newtype-integration` | JVM only |
| Sealed traits with discriminator | Manual dispatch | Macro `HandleAsEnumRule` (`SConfig.discriminator`) | Default `"type"` |
| Sealed traits with single-key wrapping | Manual dispatch | Macro `HandleAsEnumRule` (`SConfig.withWrappedSubtypes`) | |
| Case-object enums → string | Manual | Auto-detected when all subtypes are `case object`s and no discriminator is set | Read only from strings; with a discriminator they are objects `{ type = name }` |
| Scala 3 named tuples | Not supported | Yes | |
| Recursive types | Yes (manual) | Yes (no wrappers needed) | |
| Singletons (`case object`) | Yes | Yes | |

### Cross-compilation

| Feature | sconfig (raw) | Kindlings derivation |
|---|---|---|
| Scala 2.13 + Scala 3 | Yes | Yes |
| JVM | Yes | Yes |
| Scala.js 1.x | Yes (with `scala-java-time` polyfill at the link stage) | Yes (test deps include `scala-java-time` to satisfy sconfig's transitive `java.time.Duration` reference) |
| Scala Native 0.5.x | Yes (with `scala-java-time` polyfill at the link stage) | Yes |

### Cross-module symmetry with `kindlings-pureconfig-derivation`

The two modules are deliberately built to be drop-in equivalents at the API surface level, so a project can swap which one it uses based on its target platform without changing call sites:

| Concern | `kindlings-pureconfig-derivation` | `kindlings-sconfig-derivation` |
|---|---|---|
| Platform | JVM only | JVM + JS + Native |
| Underlying parser | `com.typesafe:config` (Java) | `org.ekrich:sconfig` (cross-platform Scala port) |
| Type-class hierarchy | `KindlingsConfigReader extends pureconfig.ConfigReader`, etc. | Owned: `ConfigReader` / `ConfigWriter` / `ConfigCodec` |
| Annotation for field rename | `@configKey(name)` | `@configKey(name)` |
| Annotation for transient field | `@transientField` | `@transientField` |
| Config class | `PureConfig(transformMemberNames, transformConstructorNames, discriminator = Some("type"), useDefaults = true, allowUnknownKeys = true)` | `SConfig(transformMemberNames, transformConstructorNames, discriminator = Some("type"), useDefaults = true, allowUnknownKeys = true)` |
| Default field-name transform | `ConfigFieldMapping(CamelCase, KebabCase)` (matches upstream PureConfig) | `ConfigFieldMapping(CamelCase, KebabCase)` |
| Default subtype-name transform | `ConfigFieldMapping(PascalCase, KebabCase)` (matches upstream PureConfig) | `ConfigFieldMapping(PascalCase, KebabCase)` |
| Default sealed-trait encoding | Discriminator `"type"` | Discriminator `"type"` |
| Per-type `ProductHint[A]` | `KindlingsProductHint[A]` (reuses upstream `pureconfig.ConfigFieldMapping`) | `ProductHint[A]` (reuses sconfig's local `ConfigFieldMapping`) |
| Per-type `CoproductHint[A]` | `KindlingsCoproductHint.Field[A]` / `Wrapped[A]` / `FirstSuccess[A]` | `CoproductHint.Field[A]` / `Wrapped[A]` / `FirstSuccess[A]` |
| Reading / writing semantics | Upstream PureConfig's (checked by `PureConfigOracleSpec`) | The same (checked against pureconfig-derivation's scenarios by `UpstreamSemanticsSpec`) |
| Failure types | Upstream's `ConfigReaderFailures` / `FailureReason`s | Own `ConfigDecodingError` ADT with the corresponding cases |
| Strict mode (reject unknown keys) | `PureConfig.withStrictDecoding` global / `KindlingsProductHint(allowUnknownKeys = false)` per type | `SConfig.withStrictDecoding` global / `ProductHint(allowUnknownKeys = false)` per type |
| Built-in primitive instances | Inherited from PureConfig | Defined directly in the companion |
| Collection / map handling | Inherited from PureConfig | Synthesised by the macro |
| `derives` clause (Scala 3) | Yes | Yes |
| Combined Codec on Scala 3 | Inline composition (Reader + Writer derived independently, combined at runtime — sidesteps sibling-splice isolation) | Same — inline composition |
| Combined Codec on Scala 2 | Single macro that derives both in one expansion (no sibling-splice issue on Scala 2) | Same — single macro |
| Interop with PureConfig's `ConfigSource` | Yes (via subtype relationship) | N/A — sconfig has its own `ConfigFactory.parseString` / `parseFile` API |

---

## tapir-schema-derivation

**Replaces:** Tapir's built-in `Schema.derived` + `sttp.tapir.generic.auto.*`

Checked against Tapir's own derivation, tapir-cats, tapir-iron, tapir-refined, neotype-tapir and tapir-newtype by the
`upstream-oracle-tests` module (the JSON Schemas rendered by Tapir's docs interpreter are compared on Scala 2.13 and 3;
also field descriptions on shared named map schemas, which Tapir's docs interpreter moves next to each `$ref` for both).
`TapirParityOracleSpec` (and `TapirParityScala3OracleSpec`) also runs the cases of Tapir's own tests
(`SchemaGenericAutoTest`, `SchemaMacroTest`, `SchemaAnnotationsTest`, `SchemaApplyValidationTest`,
`LegacySchemaGenericAutoTest`, `EnumsCodecDerivationTest`, `SchemaSerialisationTest`, `SchemaMacroScala3Test`,
`ValidatorScala3EnumTest`, v1.13.33) through both derivations and compares everything a `Schema` contains (names with
type parameters, field names, attributes, `hidden`, defaults, examples, validators with their names), field getters,
the subtype chosen for a value and `applyValidation`. The only differences:

| Difference | Tapir | Kindlings | Status |
|---|---|---|---|
| Required and optional fields | Optional: `Option`s and collections | Optional exactly when the decoder derived with the same JSON config accepts the field missing (e.g. with circe, collections are required) | Design decision |
| Iron constraints | tapir-iron turns them into validators | The underlying type's schema; tapir-iron's schemas (with validators) are used when imported | Parity with tapir-iron imported |
| refined predicates | tapir-refined turns them into validators | The underlying type's schema; tapir-refined's schemas are used when imported | Parity with tapir-refined imported |
| neotype `validate` | neotype-tapir adds a custom validator (not rendered in JSON Schema) | The underlying type's schema; neotype-tapir's schemas are used when imported | Parity with neotype-tapir imported |
| Names of maps keyed by scala-newtype types (Scala 2.13) | `Schema.schemaForMap[K, V]` names the key after the type member (`Name.Type`: `Map_Type_Int` for every newtype) | Named after the companion (`Map_Name_Int`), as Iron `RefinedType` and neotype keys are on both sides | Improvement (tapir guide, "Maps, collections and other built-in types") |
| Type aliases among type arguments of names (e.g. refined's `NonEmpty`) | The alias' name | Dealiased (`Not[Empty]`) | Documented (tapir guide) |
| Discriminator values | Derived from the subtype's schema name (so an `@encodedName` on the subtype changes it), transformed by `Configuration.toDiscriminatorValue` | The value the JSON codec writes (circe: `transformConstructorNames` of the constructor name; jsoniter-scala: `adtLeafClassNameMapper` / `@named`) | Design decision (schema follows the codec) |
| `@hidden` on a field | Ignored by `Schema.derived` (read only for enumerations and endpoint inputs) | The field's schema is hidden (Tapir's docs interpreter leaves it out) | Improvement |
| Annotations of a sealed trait | Applied to each subtype's schema only | Applied to each subtype's schema and to the sealed trait's own (coproduct) schema | Improvement |
| Order of subtypes of a sealed hierarchy | Sorted by name | Declaration order on Scala 3; by name on Scala 2.13 (class files keep no declaration order) | Design decision |
| Name of a parameterless case of a Scala 3 `enum` (as a subtype) | Without the enum (`pkg.Dot`) | With it (`pkg.Shape.Dot`), like the parametrised cases | Design decision |
| Scala 3 union types | Need `Schema.derivedUnion`; the members' own schemas | Derived; named like `derivedUnion` (`A_or_B`), the members described the way the JSON codec writes them (circe-derivation: each wrapped in an object keyed by its type name) | Design decision (schema follows the codec) |
| `Either` | `Schema.schemaForEither`: `oneOf` the two schemas | Described the way the JSON codec writes it (circe: `{"Left": ...}` / `{"Right": ...}`), so validation errors have the wrapper in their path (`Left.value`) | Design decision (schema follows the codec) |

### Annotations

| Feature | Tapir built-in | Kindlings | Status |
|---|---|---|---|
| `@description` | Yes | Yes | Parity |
| `@title` | Yes | Yes | Parity |
| `@encodedName` on a field | Yes | Yes | Parity |
| `@encodedName` on a type (names its schema, without type parameters; not inherited by subtypes) | Yes | Yes | Parity |
| Annotations inherited from the parent type and from the parent's members of the same name (`def name: String` of a sealed trait), unless the element has its own | Yes | Yes | Parity |
| Several `@customise` on one element, applied in declaration order (after the metadata annotations) | Yes | Yes | Parity |
| Annotations of a `scala.Enumeration` object on its values' schema (named after the object) | Yes | Yes | Parity |
| `@format` | Yes | Yes | Parity |
| `@hidden` | Yes | Yes | Parity |
| `@deprecated` | Yes | Yes | Parity |
| `@validate` | Yes | Yes | Parity |
| `@validateEach` | Yes | Yes | Parity |
| `@default` | Yes | Yes | Parity |
| `@encodedExample` | Yes | Yes | Parity |
| `@customise` | Yes | Yes | Parity |

### Configuration

| Feature | Tapir built-in | Kindlings | Status |
|---|---|---|---|
| Field naming | Independent `sttp.tapir.generic.Configuration` (must manually match JSON lib) | Discovers JSON lib config at compile time | Improvement |
| Discriminator | Independent (must manually match JSON lib) | Discovers JSON lib config at compile time | Improvement |
| JSON config consistency | **No** — schema and codec configs are separate, drift is common | **Yes** — automatically reads Circe/Jsoniter config via `JsonSchemaConfigExtension` | Improvement |
| Recursive types on Scala 3 | Must use `implicit def` (not `given`), risk of deadlocks | Works | Improvement |
| Runtime type parameter resolution in SName | No — abstract type params in generic helpers produce `?` in schema names | Resolves at runtime via `runtimePlainPrint` (e.g. `Box[A]` → `SName("Box", List("SimplePerson"))` when `A = SimplePerson`) | Improvement |

### Coproduct (sealed trait) schemas

Discriminator metadata is fully propagated to child schemas:
- Each child `SProduct` gets a discriminator field with single-value `Validator.Enumeration`
- `encodedDiscriminatorValue` attribute is set on each child schema
- Matches upstream Tapir's `addDiscriminatorField` behavior
- The subtype of a value (`SCoproduct.subtypeSchema`, used by `Schema.applyValidation`) is chosen by a pattern match
  on the subtypes, so validation reaches the fields of nested subtypes, subtypes renamed with `@encodedName` and
  Scala 3 enum cases, as with Tapir

With a jsoniter `JsoniterConfig` the schemas also describe the jsoniter codecs' wrapper-style ADTs (leaves wrapped in a
single-field object, case objects as strings), flattened nested hierarchies, `@named` leaves and fields, the
fully-qualified names passed to `adtLeafClassNameMapper`, Java enums / `scala.Enumeration` as strings (or ids), tuples as
arrays (`prefixItems`) and `Option[Option[A]]` as an ADT — see the Tapir Schema derivation guide.

### Not ported

| Feature | Notes |
|---|---|
| `Schema.oneOfWrapped`, `Schema.oneOfUsingField` | Tapir's builders, not derivation; schemas built with them (and with `Schema.derivedEnumeration`, `Schema.string`, ...) are used for fields, like by Tapir's derivation (checked by the oracle) |
| `.modify(_.path)` post-derivation | Tapir core feature, works on derived schemas (field getters and validation checked by the oracle) |
| `Schema.derivedEnumeration` builder | Upstream's full enumeration-schema builder (`.defaultStringBased`, custom encode, …); `KindlingsSchema.derivedEnumeration` is the same as `.defaultStringBased` for case-object-only sealed traits and Scala 3 enums (checked by the oracle) |

---

## ubjson-derivation

**Original type class** — no replacement target. Provides compile-time derivation of combined encoder/decoder codecs for the [UBJson (Universal Binary JSON)](https://ubjson.org/) binary format. Uses a streaming reader/writer API for efficient serialization. Cross-compiled for Scala 2.13 + 3, JVM + JS + Native.

### Type classes

| Type class | Description |
|---|---|
| `UBJsonValueCodec[A]` | Combined encoder/decoder for UBJson binary format (streaming reader/writer) |

### Configuration

| Config option | Default | Description |
|---|---|---|
| `fieldNameMapper` | `identity` | Transform field names (supports snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE) |
| `adtLeafClassNameMapper` | `UBJsonConfig.simpleClassName` | Transform sealed trait subtype names (receives the fully-qualified leaf name; the default keeps the simple name) |
| `discriminatorFieldName` | `None` | Discriminator field for sealed traits (wrapper encoding when `None`) |
| `skipUnexpectedFields` | `true` | Skip unknown fields during decoding |
| `enumAsStrings` | `false` | Encode case object enums as strings instead of objects |
| `transientDefault` | `false` | Omit fields with default values during encoding |
| `transientEmpty` | `false` | Omit empty collections during encoding |
| `transientNone` | `false` | Omit `None` fields during encoding |
| `requireCollectionFields` | `false` | Require collection fields to be present during decoding |
| `requireDefaultFields` | `false` | Require fields with defaults to be present during decoding |
| `checkFieldDuplication` | `false` | Check for duplicate field names during decoding |
| `bigDecimalPrecision` | `34` | Maximum precision for BigDecimal (DoS protection) |
| `bigDecimalScaleLimit` | `6178` | Maximum scale for BigDecimal (DoS protection) |
| `bigDecimalDigitsLimit` | `308` | Maximum digits for BigDecimal (DoS protection) |
| `mapMaxInsertNumber` | `Int.MaxValue` | Maximum map entries (DoS protection) |
| `setMaxInsertNumber` | `Int.MaxValue` | Maximum set entries (DoS protection) |
| `useScalaEnumValueId` | `false` | `scala.Enumeration` values as their ids |
| `skipNestedOptionValues` | `false` | `Option[Option[A]]` as a plain `Option` (otherwise the ADT `Option`, as jsoniter-scala) |
| `scalaTransientSupport` | `false` | `scala.transient` fields are transient |
| `transientNull` | `false` | Omit `null` fields of Scala 3 `T \| Null` types during encoding |
| `circeLikeObjectEncoding` | `false` | Without a discriminator, case objects as wrapped empty objects (`{"Leaf":{}}`) instead of strings |
| `javaEnumValueNameMapper` | `identity` | Transform Java enum constant names |
| `bigIntDigitsLimit` | `308` | Maximum digits for decoded `BigInt` (DoS protection) |
| `requireDiscriminatorFirst` | `true` | When `false`, the discriminator field may appear anywhere in the object |
| `emptyCollectionsAsDefault` | `true` | An empty array/object for a collection/map field with a default decodes as that default (`withEmptyCollectionsAsEmpty` sets `false`) |

### Annotations

| Annotation | Description |
|---|---|
| `@fieldName(name)` | Rename a field in the UBJson representation |
| `@transientField` | Exclude a field from encoding/decoding (requires default value) |
| `@stringified` | No effect (UBJson numbers are typed binary values, there is no string form) |

### Type support

| Type | Support |
|---|---|
| Primitives (`Int`, `Long`, `Double`, `Float`, `Boolean`, `Byte`, `Short`, `Char`) | Yes (native UBJson types) |
| `String` | Yes |
| `BigDecimal`, `BigInt` | Yes (with configurable DoS protection) |
| `Option[A]` | Yes (null for `None`; `Option[Option[A]]` as the ADT `Option`, or plain with `skipNestedOptionValues`) |
| Collections (`List`, `Vector`, etc.) | Yes (via Hearth `IsCollection`) |
| `Map[K, V]` (string keys) | Yes (UBJson object) |
| `Map[K, V]` (non-string keys: `Int`, `Long`, etc.) | Yes (array of key-value pairs) |
| Case classes | Yes (UBJson object with field names, all parameter lists) |
| Plain (non-case) classes | Yes (primary-constructor `val`/`var` parameters) |
| Scala 3 `T \| Null` | Yes (`null` or a `T`) |
| Nested sealed hierarchies | Yes (flattened to their leaves) |
| Value classes | Yes (automatic unwrapping) |
| Sealed traits (wrapper encoding) | Yes (single-key object wrapping subtype name) |
| Sealed traits (discriminator encoding) | Yes (via `discriminatorFieldName` config) |
| Case object enums | Yes (object or string encoding via `enumAsStrings`) |
| Mixed enums (case objects + case classes) | Yes |
| Singletons (case objects) | Yes |
| Generic types (`Box[A]`, `Pair[A, B]`) | Yes |
| Higher-kinded types (`F[_]`) | Yes |
| Recursive types | Yes |
| Nested structures (arbitrary depth) | Yes |
| Type aliases | Yes |

### Codec extensions

`UBJsonValueCodecExtensions` provides `map` and `mapDecode` combinators for transforming existing codecs, enabling support for custom types by mapping to/from supported types.

---

## yaml-derivation

**Replaces:** VirtusLab `scala-yaml` built-in `derives YamlEncoder`/`YamlDecoder`/`YamlCodec`

### Configuration

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Field name transforms (snake/kebab/pascal/screaming) | No | Yes | Improvement |
| Constructor name transforms | No | Yes | Improvement |
| Discriminator config | No | Yes (`withDiscriminator`) | Improvement |
| Single-key wrapper for subtypes | No | Yes (`withWrappedSubtypes`) | Improvement |
| Constructor defaults for absent fields | Always | On by default (`withoutUseDefaults` turns them off) | Parity |
| `enumAsStrings` | No (buggy for enums) | Yes | Improvement |

### Type classes

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| `YamlEncoder[A]` | Yes (Scala 3 only!) | Yes (Scala 2+3) | Improvement |
| `YamlDecoder[A]` | Yes (Scala 3 only!) | Yes (Scala 2+3) | Improvement |
| `YamlCodec[A]` | Yes (Scala 3 only!) | Yes (`KindlingsYamlCodec`, Scala 2+3) | Improvement |

### Annotations

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Per-field rename | None | `@fieldName` | Improvement |
| Transient field | None | `@transientField` | Improvement |

### Type support

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Sealed trait handling | Untagged: encoded without type info, decoded by trying each subtype in order (ambiguous for overlapping subtypes, #363) | The same by default; opt-in `withWrappedSubtypes` / `withDiscriminator` keep the type | Parity (default) / Improvement (opt-in) |
| Enum support | Buggy ([#363](https://github.com/VirtusLab/scala-yaml/issues/363)) | Works | Improvement |
| Option encoding | Buggy (documented) | Works | Improvement |
| Recursive types | No: `derives` / a `given` for a recursive type never finishes initializing (the derived encoder summons its fields' instances, i.e. itself, in its constructor) | Yes | Improvement |
| Value classes | No: not derivable (no `Mirror` for a value class) | Automatic unwrapping | Improvement |
| `private val` constructor fields | Written and read | Written (read by position) and read | Parity |
| Collections, `Option`s and maps of derived types | Through the element's `given` instance | Derived in place; same nodes and same failures (`Could't construct scala.collection.immutable.List from ...`, also for a null) | Parity |
| Map keys | Any type with an instance (`forMap[K, V]`); `String` keys reject a null and non-scalars, and are written with non-printable characters escaped | The same; keys without an instance are derived (e.g. value classes) | Parity / Improvement |
| `Char` fields | Encoder only | Encoder and decoder | Improvement |
| `BigInt` / `BigDecimal` fields | Decoder only | Decoder (scala-yaml's) and encoder | Improvement |
| `Any`, `Map[Any, Any]` fields | Decoder only (`YamlDecoder.forAny`: tags, aliases, precise doubles) | The same instance is used | Parity |
| Tuples, `Either` | Derived as the case classes they are (`_1`/`_2`, `value`) | Same | Parity |
| Java enums | No (Mirror-based derivation cannot) | Yes | Improvement |
| Scala 3 named tuples | No | Yes | Improvement |
| `Map[K, V]` | No special-casing | Yes (via Hearth `IsMap`) | Improvement |

### Checked against upstream (oracle)

`upstream-oracle-tests` (`scala-3/YamlOracleSpec`, Scala 3 only, as scala-yaml's derivation) derives the same fixtures
with Kindlings (default `YamlConfig`) and with scala-yaml's `YamlCodec.derived`, and compares the encoded YAML nodes
(values and tags) and the decoded values of valid and invalid inputs, including failure messages. Products, `Option`
fields (an absent one is `None`), constructor defaults, primitives, collections, maps, nested case classes, sealed traits
(untagged, overlapping subtypes and case objects attempted in declaration order) and Scala 3 enums have **no
differences**.

`scala-3/YamlUpstreamSuitesOracleSpec` runs scala-yaml's own derivation tests (`DecoderSuite`, `DecoderErrorsSuite`,
`DockerYamlDecoderSuite`, `ConstructSuite`, `BaseYamlEncoderSuite`, `YamlEncoderSuite`) through both, comparing the
nodes (values, tags, order of the keys), the printed YAML, the decoded values and the complete failures (message with
the position, `expected`): numbers (ranges, octal/hexadecimal, underscores, non-finite), `Option`s and nulls
(`!!null` for a non-optional field), sequences, mappings, nested case classes, user-provided codecs, default
parameters (evaluated lazily, on every use), `Any`, more than 4 fields, collections / options / maps of derived types
with nodes of the wrong kind, maps with `Int` keys, big numbers, generic case classes, private constructor fields,
tuples and `Either`. The only difference:

| Difference | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Fields after a failing field | Every field is decoded (and the defaults of absent fields are evaluated), then the first failure is reported | The same failure; later fields are not decoded and their defaults are not evaluated | Design decision (no work after a failure) |

The suites also check that the opt-ins differ as intended:

| Opt-in | scala-yaml | Kindlings |
|---|---|---|
| `withoutUseDefaults` | Absent field with a constructor default uses it | Fails (`Key x doesn't exist in parsed document`) |
| `withWrappedSubtypes` | Subtype written untagged; decoding tries every subtype | Single-key mapping `{ Subtype: { ... } }` |
| `withDiscriminator(field)` | (as above) | The subtype's name in `field` |
| `withEmptyCollectionsAsFlow` | `toYamlString`: an empty collection printed as an empty value, which does not decode back | Printed as `[]` / `{}` (also nested), decodes back |

Kindlings additions without a scala-yaml counterpart: union types are always wrapped; Java enum / `scala.Enumeration`
values are written as their names.

Printing is scala-yaml's (`toYamlString` prints the encoded node with scala-yaml's `Node#asYaml`), so it behaves the
same, including one upstream limitation the oracle pins: an empty sequence or mapping is printed as an empty value
(`children: ` instead of `children: []`), which reads back as `null`, so YAML printed from a value with an empty
collection field does not decode back (`Could't construct scala.collection.immutable.List from null`). Decoding from the
encoded `Node` (`KindlingsYamlDecoder.decode(KindlingsYamlEncoder.encode(value))`) round-trips, as does YAML using the
flow style `[]` / `{}`, which the opt-in `withEmptyCollectionsAsFlow` makes `toYamlString` print.

### Cross-compilation

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Scala 2 support | No (Scala 3 only) | Yes | Improvement |
| Scala 3 Native | No ([#324](https://github.com/VirtusLab/scala-yaml/issues/324)) | Yes | Improvement |
| Cross-platform | JVM + JS (Scala 2 Native only) | JVM + JS + Native | Improvement |

### Configuration (cont.)

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| `useDefaults` | Always on, not configurable (compile-time macro reflection) | On by default, configurable (`withoutUseDefaults`), compile-time | Improvement |

---

## xml-derivation

**Original type class** — no replacement target. Provides compile-time derivation of XML encoders and decoders using `scala.xml.Elem` as the XML representation. Fields can be mapped to XML elements, attributes, or text content via annotations. Decoders return `Either[XmlDecodingError, A]` for safe error handling. Cross-compiled for Scala 2.13 + 3, JVM + JS + Native.

### Type classes

| Type class | Description |
|---|---|
| `XmlEncoder[A]` | Encodes a value to `scala.xml.Elem` with a given element name |
| `XmlDecoder[A]` | Decodes a `scala.xml.Elem` to `Either[XmlDecodingError, A]` |
| `XmlCodec[A]` | Combined encoder + decoder (`KindlingsXmlCodec` derives both) |

### Configuration

| Config option | Default | Description |
|---|---|---|
| `defaultFieldMode` | `XmlFieldMode.Element` | Default mapping for fields: `Element` (child elements) or `Attribute` (XML attributes, for single text values: primitives, `String`, `BigDecimal`, `BigInt`, value types over them and `Option`s of these; collections, maps, case classes and sealed traits stay child elements). Encoder and decoder follow the same rule |
| `fieldNameMapper` | `identity` | Transform field names (supports snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE) |
| `constructorNameMapper` | `identity` | Transform sealed trait subtype names for discriminator values |
| `discriminatorAttribute` | `Some("type")` | Attribute name for sealed trait discrimination (`None`: the subtype is an element named after it, inside the value's element); `@xmlDiscriminator(name)` on a sealed type overrides it for that type |
| `enumAsStrings` | `false` | Write enums whose cases are all singletons (case objects, parameterless Scala 3 enum cases, Java enums, `Enumeration`s) as the case name: the text of the element, or - with `@xmlAttribute` / `defaultFieldMode = Attribute` - an attribute. With it, a sealed trait with a case class is a compile-time error (as in circe-derivation; a runtime error when the configuration is only known at runtime) |
| `useDefaults` | `false` | Use constructor default values for missing fields (child elements or attributes) during decoding |
| `transientNone` | `false` | Omit `None` fields during encoding |
| `transientEmpty` | `false` | Omit empty collections during encoding (empty strings are still written) |

### Annotations

| Annotation | Description |
|---|---|
| `@xmlName(name)` | Rename a field/subtype in the XML representation (not transformed by the name mappers) |
| `@xmlDiscriminator(name)` | On a sealed trait / Scala 3 enum: the discriminator attribute of that type, whatever `discriminatorAttribute` is (also when it is `None`) |
| `@transientField` | Exclude a field from encoding/decoding (requires default value) |
| `@xmlAttribute` | Force a field to be encoded/decoded as an XML attribute. Needs a single text value (a text type, a value type over one, an enum of singletons, a type with a user-provided instance, or an `Option` of these); anything else is a compile-time error |
| `@xmlElement` | Force a field to be encoded/decoded as a child XML element |
| `@xmlContent` | Map a field to the text content of the enclosing element (a single text value, as for `@xmlAttribute`); the other fields are attributes |
| `@xmlWrapper(name)` | Put the field inside an additional element with the given name: a collection as one element per item named after the field (`<items><item>..</item><item>..</item></items>`), any other field as its element |
| `@xmlUnwrapped` | Write a collection (or a map) as one element per item named after the field, directly in the enclosing element |

### Type support

| Type | Support |
|---|---|
| Primitives (`Int`, `Long`, `Double`, `Float`, `Boolean`, `Byte`, `Short`, `Char`) | Yes (text representation, surrounding whitespace ignored; a `Char` is kept exactly) |
| `String` | Yes (whitespace kept exactly; an empty element or attribute value is `""`) |
| `BigDecimal`, `BigInt` | Yes (text representation, surrounding whitespace ignored) |
| `Option[A]` | Yes (omitted or present) |
| Collections (`List`, `Vector`, etc.) | Yes (via Hearth `IsCollection`) |
| `Map[K, V]` | Yes (via Hearth `IsMap`) |
| Case classes | Yes (fields as child elements or attributes) |
| Value classes | Yes (automatic unwrapping) |
| Sealed traits | Yes (discriminator attribute, or an element named after the subtype; nested sealed traits are flattened) |
| Enums of singletons (case objects, Scala 3 enums, Java enums, `Enumeration`) | Yes (discriminator / element encoding; with `enumAsStrings` the case name, also as an attribute or as text content - `@xmlAttribute` / `@xmlContent` on an enum require `enumAsStrings`) |
| Singletons (case objects) | Yes (an empty element) |
| Generic types (`Box[A]`, `Pair[A, B]`) | Yes |
| Nested structures (arbitrary depth) | Yes |

### Error handling

`XmlDecodingError` is a sealed hierarchy providing structured errors:

| Error type | Description |
|---|---|
| `MissingAttribute` | Required XML attribute not found |
| `MissingElement` | Required child element not found |
| `InvalidValue` | Text content cannot be parsed to expected type |
| `UnexpectedElement` | Unknown child element encountered (for hand-written decoders: derived decoders ignore unknown elements) |
| `MissingContent` | Element has no text content |
| `UnknownDiscriminator` | Discriminator value doesn't match any known subtype |
| `MissingDiscriminator` | Discriminator attribute not found on element |
| `Multiple` | Aggregation of multiple errors |
| `General` | Catch-all for other error conditions |

---

## fast-show-pretty

**Original type class** — no replacement target. Provides configurable pretty-printing with indentation support for case classes, collections, maps, and primitives. Cross-compiled for Scala 2.13 + 3, JVM + JS + Native.

### Annotations

| Annotation | Target | Description |
|---|---|---|
| `@sensitiveData` | Field or type | Replaces the rendered value with `[redacted]` |
| `@sensitiveData("reason")` | Field or type | Replaces the rendered value with `[redacted: reason]` |

---

## jsoniter-json

**Replaces:** jsoniter-scala-circe's `JsoniterScalaCodec.jsonC3c` / `JsoniterScalaCodec.jsonCodec(...)` with circe's
`Json` / `JsonNumber` / `JsonObject`

Checked against jsoniter-scala-circe and circe's JSON model by the `upstream-oracle-tests` module
(`JsoniterJsonOracleSpec`): parsing (accepted inputs, structure, key order, number text, nesting depth), printing
(escaping, number formatting), numeric conversions of parsed and constructed numbers, `JsonNumber.fromString`,
equality, and the `Json`/`JsonObject` API. `JsoniterJsonCodecOracleSpec` runs the cases of jsoniter-scala-circe's own
`JsoniterScalaCodecSpec` through both: the default codec and every option of `jsonCodec`, comparing the JSON written
and the failure messages (`JsonCodecOptionsSpec` asserts upstream's expected values on JVM, Scala.js and Scala Native).

| Feature | circe + jsoniter-scala-circe | Kindlings | Status |
|---|---|---|---|
| Parsing and printing | Numbers keep their text (also `-0`), max depth 128 | Same (`JsonCodec.withMaxDepth(n)` for another limit) | Parity |
| Number precision and limits | Read exactly (`MathContext.UNLIMITED`); 308 or more digits, or a scale of 6178 or more, rejected | Same | Parity |
| Failure messages | `expected JSON value`, `expected '}' or ','`, `expected ']' or ','`, `depth limit exceeded`, `value exceeds limit for ...` | Same | Parity |
| `jsonCodec(maxDepth = ..)` | Depth limit for reading and writing | `JsonCodec.jsonCodec(maxDepth = ..)` (or `withMaxDepth`) | Parity |
| `jsonCodec(doSerialize = ..)` | Filter of object fields (by value) when writing | `JsonCodec.jsonCodec(doSerialize = ..)` | Parity |
| `jsonCodec(numberParser = ..)`, `numberParser(mc, scaleLimit, digitsLimit)` | Custom number parsing; rounding and limits | `JsonCodec.jsonCodec(numberParser = ..)`, `JsonCodec.numberParser(mc, scaleLimit, digitsLimit)` | Parity |
| `jsonCodec(numberSerializer = ..)` | Custom number writing | `JsonCodec.jsonCodec(numberSerializer = ..)` | Parity |
| JavaScript-compatible number serializer | `jsCompatibleNumberSerializer`: integers outside `[-2^52, 2^52)` and decimals with more than 52 bits or a scale outside `[-256, 256]` as strings; numbers made from a `Double`/`Float` always as numbers | `JsonCodec.jsCompatibleNumberSerializer`: same, for parsed and constructed numbers (a `JsonNumber` keeps the `Long` / `Float` / `Double` / `BigDecimal` / text it was made from, like circe's) | Parity |
| `jsonCodec(sortKeys = true)` | Fields sorted by key when writing | `JsonCodec.jsonCodec(sortKeys = true)` (duplicate keys keep their order) | Parity |
| `jsonCodec(initialSize = ..)` | Size hint for the map of fields | Not applicable (a `JsonObject` is a vector of fields) | Not applicable |
| `CirceCodecs` (circe `Encoder`/`Decoder`s reading numbers from strings, `java.time` types, ...) | Instances of circe's type classes | Not applicable: this module has no `Encoder`/`Decoder` type classes (use `kindlings-jsoniter-derivation` codecs) | Not applicable |
| `toInt` / `toLong` / `toBigInt` / `toBigDecimal` | Exact, for integral values however written | Same | Parity |
| `toDouble` / `toFloat` | Plain values (infinity when out of range) | `Option` | Difference (API) |
| Duplicate keys | One value per key (last wins) | Kept (ordered fields; `apply` returns the last value; `add` appends) | Difference (design) |
| Equality | Numbers by value, objects as maps | Structural (number text, ordered fields) | Difference (design) |
| `JsonNumber.fromString` | Also accepts `01`, `.5` (and writes them as invalid JSON) | JSON numbers only | Difference (stricter) |
