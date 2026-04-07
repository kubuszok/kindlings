# Feature Parity: Kindlings vs Original Libraries

This document tracks feature parity between Kindlings derivation modules and the original libraries they replace, as well as UX improvements Kindlings provides.

Legend: **Parity** = feature matches original, **Improvement** = Kindlings does it better, **Gap** = feature not yet implemented.

---

## avro-derivation

**Replaces:** `com.sksamuel.avro4s` (v4 for Scala 2, v5 for Scala 3 — separate incompatible versions)

### Configuration

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Namespace config | Yes | Yes | Parity |
| Field name transforms (`FieldMapper`) | Yes (`SnakeCase`, custom) | Yes (4 built-in strategies) | Parity |
| Constructor name transforms | Not built-in | Yes | Improvement |
| Decimal config (precision/scale) | `@AvroScalePrecision` or implicit `ScalePrecision` | `AvroConfig.decimalConfig` | Parity (different API) |

### Type classes

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| `SchemaFor[A]` | Yes | Yes (`AvroSchemaFor`) | Parity |
| `Encoder[A]` | Yes | Yes (`AvroEncoder`) | Parity |
| `Decoder[A]` | Yes | Yes (`AvroDecoder`) | Parity |

### Annotations

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@AvroName` / `@fieldName`) | Yes | Yes | Parity |
| Documentation (`@AvroDoc` / `@avroDoc`) | Yes | Yes | Parity |
| Namespace (`@AvroNamespace` / `@avroNamespace`) | Yes | Yes | Parity |
| Custom properties (`@AvroProp` / `@avroProp`) | Yes | Yes (stackable) | Parity |
| Aliases (`@AvroAlias` / `@avroAlias`) | Yes | Yes (stackable) | Parity |
| Fixed size (`@AvroFixed` / `@avroFixed`) | Yes | Yes | Parity |
| Error type (`@AvroError` / `@avroError`) | Yes | Yes | Parity |
| Transient field (`@AvroTransient` / `@transientField`) | Yes | Yes | Parity |
| Default values (`@AvroDefault`) | From constructor defaults (automatic) | `@avroDefault(json)` (explicit JSON string) | Parity (different API) |
| Subtype ordering (`@AvroUnionPosition` / `@avroSortPriority`) | Yes | Yes | Parity |
| `@AvroNoDefault` (suppress defaults) | Yes | Yes (`@avroNoDefault`) | Parity |
| `@AvroErasedName` (disable generic name encoding) | Yes | Yes (`@avroErasedName`) | Parity |
| `@AvroEnumDefault` | Yes | Yes (`@avroEnumDefault`) | Parity |

### Type support

| Feature | avro4s | Kindlings | Status |
|---|---|---|---|
| UUID | Yes | Yes | Parity |
| Instant | Yes | Yes | Parity |
| LocalDate | Yes | Yes | Parity |
| LocalTime | Yes | Yes | Parity |
| LocalDateTime | Yes | Yes | Parity |
| BigDecimal | BYTES/FIXED with decimal logical type | BYTES with decimal logical type (when DecimalConfig provided); STRING fallback | Parity |
| Value classes | Unwrapped | Unwrapped | Parity |
| Case classes → RECORD | Yes | Yes | Parity |
| Sealed case objects → ENUM | Yes | Yes | Parity |
| Sealed traits → UNION | Yes | Yes | Parity |
| `Either[A,B]` → UNION | Yes | Yes | Parity |
| `Option[T]` → UNION(null,T) | Yes | Yes | Parity |
| Recursive types | Yes (with caveats) | Yes | Parity |
| Shapeless Coproduct → UNION | Yes | No | Gap |

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
| `OffsetDateTime`, `java.sql.Date`, `java.sql.Timestamp` | Upstream supports additional temporal types |
| Timestamp precision variants (`TimestampMicros`, `TimestampNanos`) | Upstream supports micros/nanos; Kindlings uses millis |
| Byte collection special-casing (`Array[Byte]` → `BYTES`) | Upstream auto-detects byte arrays/collections and maps to Avro BYTES |
| Decoder type widening | Upstream widens `Int` → `Long`, `Float` → `Double` during decoding |
| Generic type parameter names in schema | Upstream encodes generic type params in schema name (e.g., `Box[Int]`); Kindlings erases them (design decision) |

### `@avroSortPriority` note

Kindlings uses `Int` priority (upstream avro4s uses `Float`). Higher priority values appear first in UNION/ENUM ordering, matching upstream semantics.

---

## cats-derivation

**Replaces:** [kittens](https://github.com/typelevel/kittens) — automatic/semi-automatic type class derivation for Cats

### Type classes — monomorphic (kind `*`)

| Type class | kittens (Scala 2) | kittens (Scala 3) | Kindlings | Status |
|---|---|---|---|---|
| `cats.Show` | Yes | Yes | Yes | Parity |
| `cats.kernel.Eq` | Yes | Yes | Yes | Parity |
| `cats.kernel.Order` | Yes | Yes | Yes | Parity |
| `cats.kernel.PartialOrder` | Yes | Yes | Yes | Parity |
| `cats.kernel.Hash` | Yes | Yes | Yes | Parity |
| `cats.kernel.Semigroup` | Yes | Yes | Yes | Parity |
| `cats.kernel.Monoid` | Yes | Yes | Yes | Parity |
| `cats.kernel.CommutativeSemigroup` | Yes | Yes | Yes | Parity |
| `cats.kernel.CommutativeMonoid` | Yes | Yes | Yes | Parity |
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
| `cats.NonEmptyAlternative` | No | No | Yes | Improvement |
| `cats.Alternative` | No | No | Yes | Improvement |
| `alleycats.ConsK` | Yes | No ([#489](https://github.com/typelevel/kittens/issues/489)) | Yes | Improvement |

### Type support

| Feature | kittens | Kindlings | Status |
|---|---|---|---|
| Case classes (products) | Yes | Yes | Parity |
| Sealed traits (coproducts) — Show, Eq, Order, Hash | Yes | Yes | Parity |
| Sealed trait with case objects only | Yes | Yes | Parity |
| Nested container fields (e.g., `List[A]` in `F[A]`) for ConsK | Scala 2 only (Shapeless) | Yes (both Scala 2+3) | Improvement |
| Head+tail shift pattern for ConsK (e.g., `NEL[A](head: A, tail: List[A])`) | Scala 2 only | Yes (both Scala 2+3) | Improvement |

### Derivation API

| Feature | kittens | Kindlings | Status |
|---|---|---|---|
| Semi-automatic (`TypeClass.derived`) | Yes | Yes | Parity |
| Automatic (`import derived._`) | Yes (Scala 3 only for some) | No | Gap |
| Unified Scala 2+3 API | No — Shapeless on 2, Mirrors on 3 | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | JVM focus | JVM + JS + Native | Improvement |

### Not ported

| Feature | Notes |
|---|---|
| `cats.Show` for enums using ordinal | kittens uses ordinal for Show of coproducts; Kindlings uses class name + fields |
| `cats.kernel.BoundedSemilattice` | Niche type class — not ported |
| `cats.kernel.Band` | Niche type class — not ported |
| `cats.kernel.Group` / `CommutativeGroup` | Would require inverse operation — not derivable from field instances alone |
| Automatic derivation | kittens Scala 3 supports `derives`; Kindlings uses explicit `.derived` calls |

---

## circe-derivation

**Replaces:** `circe-generic-extras` (Scala 2 only, community-maintained) + circe's Scala 3 `ConfiguredEncoder`/`ConfiguredDecoder`/`ConfiguredCodec`

### Configuration

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Field name transforms (snake/kebab/pascal/screaming) | Yes | Yes | Parity |
| Constructor name transforms | Yes | Yes | Parity |
| `useDefaults` | Yes (Scala 3 needs `-Yretain-trees`) | Yes | Parity |
| Discriminator field | Yes | Yes | Parity |
| Strict decoding | Yes | Yes | Parity |
| `enumAsStrings` config flag | Scala 2: `deriveEnumeration*`; Scala 3: `EnumEncoder`/`EnumDecoder` (parameterless only) | Yes (unified config flag, both Scala 2+3) | Improvement |

### Type classes

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| `Encoder[A]` | Yes | Yes | Parity |
| `Encoder.AsObject[A]` | Yes | Yes (`deriveAsObject`) | Parity |
| `Decoder[A]` | Yes | Yes | Parity |
| `Codec.AsObject[A]` | Scala 3 only | Yes (`KindlingsCodecAsObject`) | Parity |

### Annotations

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Per-field rename | `@JsonKey` (Scala 2 only! No Scala 3 equivalent) | `@fieldName` (both Scala 2+3) | Improvement |
| Transient field | No built-in annotation | `@transientField` (requires default value) | Improvement |

### Type support

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Value class unwrapping | `deriveUnwrapped*` (Scala 2 only! No Scala 3 equivalent) | Automatic (both Scala 2+3) | Improvement |
| Recursive types | Scala 2: needs Shapeless `Lazy`; Scala 3: broken with auto-derivation ([#1980](https://github.com/circe/circe/issues/1980)) | Works without wrappers | Improvement |
| Non-string map keys | Manual `KeyEncoder`/`KeyDecoder` | Derived (Int, Long, Double, Short, Byte, value classes) | Improvement |
| `Option` null vs absent | No distinction | Yes | Improvement |
| Scala 3 enums | Parameterless only (`EnumEncoder`/`EnumDecoder`) | Full support via `enumAsStrings` | Improvement |

### Cross-compilation

| Feature | circe | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | No — different modules, different APIs, feature gaps on Scala 3 | Yes | Improvement |
| Cross-platform (JVM/JS/Native) | JVM focus (Shapeless-based on Scala 2) | JVM + JS + Native | Improvement |
| Compile-time performance | Shapeless = slow, stack overflows on deep types | Hearth macros | Improvement |
| Error messages | "could not find Lazy implicit value..." | Clear messages from Hearth | Improvement |

### Not ported

| Feature | Notes |
|---|---|
| `@ConfiguredJsonCodec` macro annotation | Minor — `derives` / explicit derivation covers the same ground |
| `incomplete`/`patch` decoders | Niche API for partial decoding — not ported |
| `ExtrasDecoder` | Extension of Decoder with extra metadata — not ported |
| `@JsonNoDefault` equivalent | Upstream suppresses default for specific fields; Kindlings `@transientField` serves a different purpose (excludes from encoding entirely) |

---

## jsoniter-derivation

**Replaces:** `com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker`

### Configuration

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Field name mapper | Yes | Yes | Parity |
| ADT leaf class name mapper | Yes | Yes | Parity |
| Discriminator field | Yes (default `Some("type")`) | Yes | Parity |
| Skip unexpected fields | Yes (default `true`) | Yes | Parity |
| Enum as strings | Yes | Yes | Parity |
| Map as array | Yes | Yes | Parity |
| `transientDefault` | Yes | Yes | Parity |
| `transientEmpty` | Yes | Yes | Parity |
| `transientNone` | Yes | Yes | Parity |
| `isStringified` (global) | Yes | Yes | Parity |
| `decodingOnly` / `encodingOnly` | Yes | Yes | Parity |
| `requireCollectionFields` | Yes | Yes | Parity |
| `requireDefaultFields` | Yes | Yes | Parity |
| `circeLikeObjectEncoding` | Yes | Yes (case objects encoded as `{"Name":{}}` matching upstream) | Parity |
| `checkFieldDuplication` | Yes | Yes | Parity |
| `bigDecimalPrecision`/`bigDecimalScaleLimit`/`bigDecimalDigitsLimit` | Yes (DoS protection) | Yes | Parity |
| `mapMaxInsertNumber` / `setMaxInsertNumber` | Yes (DoS protection) | Yes | Parity |
| `useScalaEnumValueId` | Yes | Yes | Parity |

### Type classes

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| `JsonValueCodec[A]` | Yes | Yes | Parity |
| `JsonCodec[A]` | Yes | Yes | Parity |
| `JsonKeyCodec[A]` | Yes | Yes (`deriveKeyCodec`) | Parity |

### Annotations

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Per-field rename (`@named` / `@fieldName`) | Yes | Yes | Parity |
| Transient field (`@transient` / `@transientField`) | Yes | Yes (requires default value) | Parity |
| `@stringified` | Yes | Yes | Parity |

### Type support

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Non-string map keys | Yes (extensive) | Yes (numeric + value types + BigDecimal/BigInt) | Parity |
| Value class unwrapping | Yes (+ `inlineOneValueClasses`) | Automatic (always) | Parity |
| Recursive types | Yes (`allowRecursiveTypes = true`) | Yes (no flag needed) | Improvement |
| java.time built-in codecs | Yes (ISO-8601) | Yes (ISO-8601: Instant, LocalDate, LocalTime, LocalDateTime, OffsetDateTime, ZonedDateTime, Duration, Period) | Parity |

### Cross-compilation

| Feature | jsoniter-scala | Kindlings | Status |
|---|---|---|---|
| Unified Scala 2+3 API | Separate macro implementations | Unified API | Improvement |
| Cross-platform | JVM + JS + Native | JVM + JS + Native | Parity |

### Not ported

| Feature | Notes |
|---|---|
| Convenience factories (`makeCirceLike`, etc.) | Pre-configured codec makers — trivially achievable with `JsoniterConfig.default.withCirceLikeObjectEncoding` |
| `requireDiscriminatorFirst` | Upstream allows discriminator anywhere in object when `false`; Kindlings always requires discriminator first (removed dead config field) |
| `javaEnumValueNameMapper` | Separate mapper for Java enum values — not ported |
| `requireCollectionFields` buffering | Config field exists but full buffering behavior not wired |
| Additional built-in types | Upstream supports `UUID`, `BitSet`, `Either`, `Unit`, and 8+ java.time types natively; Kindlings covers `Instant`, `LocalDate`, `LocalTime`, `LocalDateTime`, `OffsetDateTime`, `ZonedDateTime`, `Duration`, `Period` |

### Default value differences

| Config field | Upstream default | Kindlings default | Notes |
|---|---|---|---|
| `mapMaxInsertNumber` | `1024` | `1024` | Same |
| `setMaxInsertNumber` | `1024` | `1024` | Same |
| `transientDefault` | `true` | `false` | Kindlings requires explicit opt-in |
| `transientEmpty` | `false` | `false` | Same |
| `transientNone` | `false` | `false` | Same |

---

## pureconfig-derivation

**Replaces:** PureConfig's auto/semi-automatic derivation surfaces:
- `pureconfig.generic.semiauto.{deriveReader, deriveWriter, deriveConvert}` (Scala 2 + Scala 3, Shapeless / Magnolia based)
- `import pureconfig.generic.auto._` (Scala 2 only — fully recursive Shapeless auto-derivation)
- `pureconfig.generic.derivation.default` (Scala 3 only — native `derives ConfigReader` syntax)

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
| Semi-automatic (`derive[A]`) | `semiauto.deriveReader[A]` (Scala 2 + 3) | `KindlingsConfigReader.derive[A]` | Parity |
| Automatic (full recursion via import) | `pureconfig.generic.auto._` (Scala 2 only — Shapeless) | `KindlingsConfigReader.derived[A]` (sanely-automatic, both Scala 2 + 3) | Improvement |
| Scala 3 native `derives` clause | `derives ConfigReader` via `pureconfig.generic.derivation.default` (Scala 3 only) | Same `derives` syntax via `KindlingsConfigReader` (both Scala 2 + 3) | Improvement |
| Unified Scala 2 + 3 API | No — three separate derivation modules with overlapping APIs and feature gaps | Yes — single API, same call shape on both | Improvement |
| `ConfigConvert` derivation on Scala 3 | Documented as not yet supported in `pureconfig.generic.derivation.default` | Yes (via inline composition of Reader + Writer) | Improvement |
| Recursive types | Works on Scala 2 (Shapeless `Lazy`); Scala 3 native derivation has limitations | Works without `Lazy` or wrappers on both | Improvement |

### Configuration

PureConfig configures derivation via implicit `ProductHint[A]` and `CoproductHint[A]` values placed in the type's companion object, plus the upstream `ConfigFieldMapping` / `CamelCase` / `KebabCase` / etc. naming conventions. Kindlings ships **both layers**: a global `PureConfig` config class threaded as an implicit (matching the convention used by `kindlings-circe-derivation` and `kindlings-yaml-derivation`) **and** type-indexed `KindlingsProductHint[A]` / `KindlingsCoproductHint[A]` for per-type overrides. Defaults match upstream PureConfig exactly: fields are `ConfigFieldMapping(CamelCase, KebabCase)`, subtypes are `ConfigFieldMapping(PascalCase, KebabCase)`, the discriminator is `Some("type")`, `useDefaults` is `true`, and `allowUnknownKeys` is `true`.

| Feature | PureConfig | Kindlings | Status |
|---|---|---|---|
| Field name transforms (camelCase ↔ snake_case ↔ kebab-case ↔ …) | `ConfigFieldMapping` + `ProductHint` per type | Same `pureconfig.ConfigFieldMapping` reused; placed on `PureConfig.transformMemberNames` for the global default and on `KindlingsProductHint[A].transformMemberNames` for per-type overrides | Parity |
| Constructor (subtype) name transforms | `FieldCoproductHint.transformConstructorNames` per ADT | Same `ConfigFieldMapping` reused; placed on `PureConfig.transformConstructorNames` globally and on `KindlingsCoproductHint.Field[A]` / `KindlingsCoproductHint.Wrapped[A]` per ADT | Parity |
| Discriminator field | `FieldCoproductHint(fieldName)` (default `"type"`) | `PureConfig.discriminator = Some("type")` global default; `KindlingsCoproductHint.Field[A](fieldName = …)` per ADT override | Parity |
| Single-key sealed-trait wrapping | Custom `CoproductHint` (or `EnumCoproductHint`) | `PureConfig.discriminator = None` (`.withWrappedSubtypes`) globally; `KindlingsCoproductHint.Wrapped[A]()` per ADT | Parity |
| Use case-class default values for missing fields | `ProductHint(useDefaultArgs = true)` (default) | `PureConfig.useDefaults = true` (default, matches upstream); `KindlingsProductHint[A](useDefaults = …)` per type | Parity |
| Strict mode (reject unknown keys) | `ProductHint(allowUnknownKeys = false)` per type | `PureConfig.allowUnknownKeys = false` global (`.withStrictDecoding`); `KindlingsProductHint[A](allowUnknownKeys = false)` per type — failures use the same `KeyNotFound`-style `ConvertFailure` shape upstream produces | Parity |
| Per-type customization (different config per type) | Yes (per-companion `ProductHint` / `CoproductHint`) | Yes (`implicit val hint: KindlingsProductHint[MyType]` / `KindlingsCoproductHint.Field[MySealed]` resolved by the macro at derivation site, overriding the global `PureConfig`) | Parity |

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
| `List`/`Vector`/`Seq`/`Set`/`Array` | Yes (via `CollectionReaders`) | Reused from upstream | Parity |
| `Map[String, A]` | Yes | Reused from upstream | Parity |
| `URL`, `URI`, `Path`, `File`, `UUID`, `Pattern`, `Regex` | Yes | Reused from upstream | Parity |
| Java time (`Instant`, `Duration`, `Period`, `ZoneOffset`, `ZoneId`, `Year`, …) | Yes | Reused from upstream | Parity |
| `FiniteDuration`, `ConfigMemorySize` | Yes | Reused from upstream | Parity |
| Case classes → HOCON object | Via Shapeless / Magnolia | Via Hearth macro (cleaner generated code, faster compile) | Parity |
| Sealed traits → discriminator object | Via `FieldCoproductHint` | Via macro + `discriminator` config | Parity |
| Sealed traits → single-key wrapping | Via custom `CoproductHint` | Via macro + `withWrappedSubtypes` | Parity |
| Case-object enums (`North`/`South`/…) → HOCON string | `deriveEnumerationReader[E]` (separate API) | Same `derive[E]` (auto-detected — case-object children encode as strings when no discriminator is set) | Improvement |
| Singleton (`case object`) members | Yes | Yes | Parity |
| Value classes / `AnyVal` wrappers | Manual `ConfigReader.fromString` per type | Automatic unwrapping via `IsValueType` (covers `AnyVal`, `extends AnyVal`, refined types via `refined-integration`, opaque types) | Improvement |
| Refined types (`eu.timepit.refined`) | Separate `pureconfig-refined` module | Automatic via `kindlings-refined-integration` (`Either[String, A]` wrap detected by `IsValueType`) | Improvement |
| Iron (Scala 3 opaque types with constraints) | Separate `pureconfig-iron` module | Automatic via `kindlings-iron-integration` | Improvement |
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

To prevent infinite macro expansion, the kindlings macro filters out its own `derived` companion methods during implicit search (`KindlingsConfigReader.type` and `KindlingsConfigConvert.type` on the reader side; `KindlingsConfigWriter.type` and `KindlingsConfigConvert.type` on the writer side). Users who simultaneously import `pureconfig.generic.auto._` (Scala 2) or `pureconfig.generic.derivation.default._` (Scala 3) will get whichever derivation rule is more specific in implicit resolution; if both are in scope and you want the kindlings derivation, do not also import the upstream auto-derivation surfaces.

### Not ported

| Feature | Notes |
|---|---|
| `FirstSuccessCoproductHint` | The `KindlingsCoproductHint.FirstSuccess[A]` case is reserved in the ADT but the macro currently treats it the same as the discriminator-based `Field` variant. The upstream "try every subtype reader in sequence until one succeeds" pattern is intentionally not wired up because it's brittle and slow; if a use case shows up we can implement it without changing the public surface. |
| `pureconfig-cats` / `pureconfig-magnolia` / `pureconfig-shapeless` modules | External ecosystem add-ons. Cats data types (`NonEmptyList`, etc.) are reachable via `kindlings-cats-integration`. |
| `pureconfig-yaml` / `pureconfig-circe` etc. format adapters | Out of scope — kindlings ships separate `kindlings-yaml-derivation` and `kindlings-circe-derivation` modules. |
| `ConfigSource` API (`ConfigSource.file`, `ConfigSource.url`, …) | Not duplicated — users keep using `pureconfig.ConfigSource.*` and pass our derived instances to it. |
| Custom `FailureReason` subtypes | Inherited from upstream when summoning user-provided `ConfigReader`s. The kindlings macro itself reports `KeyNotFound`, `WrongType`, and (for strict mode) the standard PureConfig unknown-key failure via the upstream factories. |

---

## sconfig-derivation

**Original type class** — no replacement target. PureConfig's underlying parser, `com.typesafe:config`, is JVM-only, which locks PureConfig out of Scala.js and Scala Native projects. `sconfig-derivation` fills that gap by deriving config readers/writers/codecs against [`org.ekrich:sconfig`](https://github.com/ekrich/sconfig) — a Scala port of typesafe-config that runs on JVM, Scala.js, and Scala Native — while sharing the same HOCON syntax, the same `Config` / `ConfigValue` shape, and the same derivation knobs as `kindlings-pureconfig-derivation` (so users can mentally swap between the two depending on their target platform).

Because no upstream library does derivation against sconfig today, this module is "exceeds upstream" by definition: there is nothing else to compare against. The tables below describe the surface area we ship.

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
| `ConfigDecodingError.Missing(path, key)` | Required key absent on a HOCON object |
| `ConfigDecodingError.WrongType(path, expected, actual)` | Value present but the wrong `ConfigValueType` |
| `ConfigDecodingError.CannotConvert(path, message, cause)` | Catch-all for `String => A` conversion failures (parse errors, refined-type validation, …) |
| `ConfigDecodingError.Multiple(errors)` | Aggregates multiple errors so one decode reports every problem at once |
| `withParentPath(parent)` | Extends the path on the way up the call stack so nested-field errors look like `Missing("parent.child")` |

### Derivation API

| Feature | Description |
|---|---|
| `ConfigReader.derive[A]` / `ConfigWriter.derive[A]` / `ConfigCodec.derive[A]` | Semi-automatic |
| `ConfigReader.derived[A]` / `ConfigWriter.derived[A]` / `ConfigCodec.derived[A]` | Sanely-automatic — implicit on Scala 3 (`given`), explicit non-implicit on Scala 2 (to avoid clashing with the built-in instances under Scala 2 implicit-search rules) |
| `derives ConfigReader` (Scala 3) | Yes |
| Inline composition of `ConfigCodec` from `ConfigReader.derive` + `ConfigWriter.derive` | Scala 3 — sidesteps sibling-splice isolation by running each derivation as its own top-level macro expansion |
| Single-macro Codec (`ConfigCodec.derive`) | Scala 2 — combines reader + writer in one macro expansion (no sibling-splice issue on Scala 2) |
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
| `CoproductHint.FirstSuccess[A]` | (no parameters) | upstream `pureconfig.FirstSuccessCoproductHint[A]` — type is reserved in the ADT, currently treated as discriminator-based at the macro level |

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
| `Option[A]` | Yes | Yes | `null` / `ConfigValueType.NULL` ↔ `None`; otherwise recurses into `inner` |

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
| Sealed traits with discriminator | Manual dispatch | Macro `HandleAsEnumRule` (`SConfig.discriminator`) | Default `"type"` |
| Sealed traits with single-key wrapping | Manual dispatch | Macro `HandleAsEnumRule` (`SConfig.withWrappedSubtypes`) | |
| Case-object enums → string | Manual | Auto-detected when all subtypes are `case object`s and no discriminator is set | |
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

### Annotations

| Feature | Tapir built-in | Kindlings | Status |
|---|---|---|---|
| `@description` | Yes | Yes | Parity |
| `@title` | Yes | Yes | Parity |
| `@encodedName` | Yes | Yes | Parity |
| `@format` | Yes | Yes | Parity |
| `@hidden` | Yes (was broken on Scala 3) | Yes | Parity |
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

### Not ported

| Feature | Notes |
|---|---|
| `Schema.derivedEnumeration` | Specific enum schema derivation |
| `Schema.oneOfWrapped` | Manual union schema builder |
| `.modify(_.path)` post-derivation | Tapir core feature, not a derivation concern |
| Value class unwrapping in schemas | Upstream unwraps value classes to their inner type's schema; Kindlings wraps them in an `SProduct` |

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
| `adtLeafClassNameMapper` | `identity` | Transform sealed trait subtype names |
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
| `bigDecimalDigitsLimit` | `308` | Maximum digits for BigDecimal/BigInt (DoS protection) |
| `mapMaxInsertNumber` | `Int.MaxValue` | Maximum map entries (DoS protection) |
| `setMaxInsertNumber` | `Int.MaxValue` | Maximum set entries (DoS protection) |

### Annotations

| Annotation | Description |
|---|---|
| `@fieldName(name)` | Rename a field in the UBJson representation |
| `@transientField` | Exclude a field from encoding/decoding (requires default value) |
| `@stringified` | Encode a numeric field as a string |

### Type support

| Type | Support |
|---|---|
| Primitives (`Int`, `Long`, `Double`, `Float`, `Boolean`, `Byte`, `Short`, `Char`) | Yes (native UBJson types) |
| `String` | Yes |
| `BigDecimal`, `BigInt` | Yes (with configurable DoS protection) |
| `Option[A]` | Yes (null for `None`) |
| Collections (`List`, `Vector`, etc.) | Yes (via Hearth `IsCollection`) |
| `Map[K, V]` (string keys) | Yes (UBJson object) |
| `Map[K, V]` (non-string keys: `Int`, `Long`, etc.) | Yes (array of key-value pairs) |
| Case classes | Yes (UBJson object with field names) |
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
| Discriminator config | No | Yes | Improvement |
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
| Sealed trait handling | Brute-force try-each (ambiguous, O(n)) | Discriminator-based | Improvement |
| Enum support | Buggy ([#363](https://github.com/VirtusLab/scala-yaml/issues/363)) | Works | Improvement |
| Option encoding | Buggy (documented) | Works | Improvement |
| Recursive types | Undocumented | Yes | Improvement |
| Value classes | Not documented | Automatic unwrapping | Improvement |

### Cross-compilation

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| Scala 2 support | No (Scala 3 only) | Yes | Improvement |
| Scala 3 Native | No ([#324](https://github.com/VirtusLab/scala-yaml/issues/324)) | Yes | Improvement |
| Cross-platform | JVM + JS (Scala 2 Native only) | JVM + JS + Native | Improvement |

### Configuration (cont.)

| Feature | scala-yaml | Kindlings | Status |
|---|---|---|---|
| `useDefaults` | Yes (runtime reflection) | Yes (compile-time) | Improvement |

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
| `defaultFieldMode` | `XmlFieldMode.Element` | Default mapping for fields: `Element` (child elements) or `Attribute` (XML attributes) |
| `fieldNameMapper` | `identity` | Transform field names (supports snake_case, kebab-case, PascalCase, SCREAMING_SNAKE_CASE) |
| `constructorNameMapper` | `identity` | Transform sealed trait subtype names for discriminator values |
| `discriminatorAttribute` | `Some("type")` | Attribute name for sealed trait discrimination (`None` to disable) |
| `enumAsStrings` | `false` | Encode case object enums as text content strings |
| `useDefaults` | `false` | Use constructor default values for missing fields during decoding |
| `transientNone` | `false` | Omit `None` fields during encoding |
| `transientEmpty` | `false` | Omit empty collections during encoding |

### Annotations

| Annotation | Description |
|---|---|
| `@xmlName(name)` | Rename a field/subtype in the XML representation |
| `@transientField` | Exclude a field from encoding/decoding (requires default value) |
| `@xmlAttribute` | Force a field to be encoded/decoded as an XML attribute |
| `@xmlElement` | Force a field to be encoded/decoded as a child XML element |
| `@xmlContent` | Map a field to the text content of the enclosing element |
| `@xmlWrapper(name)` | Wrap a collection field in an additional element with the given name |
| `@xmlUnwrapped` | Inline collection elements directly without a wrapper element |

### Type support

| Type | Support |
|---|---|
| Primitives (`Int`, `Long`, `Double`, `Float`, `Boolean`, `Byte`, `Short`, `Char`) | Yes (text representation) |
| `String` | Yes |
| `BigDecimal`, `BigInt` | Yes (text representation) |
| `Option[A]` | Yes (omitted or present) |
| Collections (`List`, `Vector`, etc.) | Yes (via Hearth `IsCollection`) |
| `Map[K, V]` | Yes (via Hearth `IsMap`) |
| Case classes | Yes (fields as child elements or attributes) |
| Value classes | Yes (automatic unwrapping) |
| Sealed traits | Yes (discriminator attribute) |
| Case object enums | Yes (object or string encoding via `enumAsStrings`) |
| Singletons (case objects) | Yes |
| Generic types (`Box[A]`, `Pair[A, B]`) | Yes |
| Nested structures (arbitrary depth) | Yes |

### Error handling

`XmlDecodingError` is a sealed hierarchy providing structured errors:

| Error type | Description |
|---|---|
| `MissingAttribute` | Required XML attribute not found |
| `MissingElement` | Required child element not found |
| `InvalidValue` | Text content cannot be parsed to expected type |
| `UnexpectedElement` | Unknown child element encountered |
| `MissingContent` | Element has no text content |
| `UnknownDiscriminator` | Discriminator value doesn't match any known subtype |
| `MissingDiscriminator` | Discriminator attribute not found on element |
| `Multiple` | Aggregation of multiple errors |
| `General` | Catch-all for other error conditions |

---

## fast-show-pretty

**Original type class** — no replacement target. Provides configurable pretty-printing with indentation support for case classes, collections, maps, and primitives. Cross-compiled for Scala 2.13 + 3, JVM + JS + Native.
