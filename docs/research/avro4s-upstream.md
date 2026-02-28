# Avro4s Upstream Library Research

## Library Version & Build Info
- **avro4s**: 5.0.x (master branch, latest release 5.0.14 from Dec 2024)
- **Apache Avro**: 1.11.5
- **Scala**: 3.3.7 (v5 is Scala 3 only; v4.x on `release/4.0.x` supports Scala 2.12/2.13, maintenance only)
- **Magnolia**: 1.3.18 (magnolia1 for Scala 3)
- **Shapeless**: 2.3.7 (for Coproduct support)
- **Java target**: 1.8

## Architecture Overview

### Core Typeclasses

Three main typeclasses, all Serializable:

| Typeclass | Signature | Purpose |
|---|---|---|
| `SchemaFor[T]` | `def schema: Schema` | Generates Avro `Schema` for type `T` |
| `Encoder[T]` | `def encode(schema: Schema): T => AnyRef` | Encodes Scala value to Avro-compatible JVM object |
| `Decoder[T]` | `def decode(schema: Schema): Any => T` | Decodes Avro value to Scala type `T` |
| `Codec[T]` | `extends Encoder[T] with Decoder[T]` | Convenience combining both directions |

**Functional combinators:**
- `Encoder` has `contramap[U](f: U => T): Encoder[U]`
- `Decoder` has `map[U](f: T => U): Decoder[U]`
- `SchemaFor` has `map[U](fn: Schema => Schema): SchemaFor[U]`, `forType[U]`, and `withFieldMapper(mapper: FieldMapper)`

### Entry Points

```scala
// Schema generation
val schema: Schema = AvroSchema[MyCaseClass]

// Record conversion
val record = ToRecord[T](schema).to(value)
val value  = FromRecord[T](schema).from(record)

// Stream I/O (three formats)
AvroOutputStream.binary[T] / .json[T] / .data[T]
AvroInputStream.binary[T]  / .json[T] / .data[T]
```

### Derivation Engine

Uses **Magnolia 1** (magnolia1) for compile-time derivation via `AutoDerivation[SchemaFor]`, `AutoDerivation[Encoder]`, `AutoDerivation[Decoder]`.

**Magnolia dispatch logic** (`magnolia.scala`):
- `join` (case classes): routes to `Records.schema(ctx)` for records, or `ValueTypes.schema(ctx)` for value types (AnyVal)
- `split` (sealed traits): determines shape via `DatatypeShape`:
  - **Single-element union**: bypasses union wrapper, delegates directly to subtype's typeclass
  - **Enum** (all case objects): delegates to `SealedTraits.schema(ctx)` -- generates Avro ENUM
  - **Union** (case classes present): delegates to `TypeUnions.schema(ctx)` -- generates Avro UNION

### Output Formats

```scala
enum AvroFormat:
  case Binary  // minimal size, no embedded schema
  case Json    // JSON representation
  case Data    // standard Avro with embedded schema
```

---

## Type Support

### Primitives

| Scala Type | Avro Schema Type | Notes |
|---|---|---|
| `Boolean` | `boolean` | |
| `Int` | `int` | |
| `Long` | `long` | |
| `Float` | `float` | |
| `Double` | `double` | |
| `Byte` | `int` | Reuses int schema; encoder wraps as `java.lang.Byte` |
| `Short` | `int` | Reuses int schema; encoder wraps as `java.lang.Short` |
| `String` | `string` | Default Avro string; also supports `JavaStringSchemaFor` variant with `"avro.java.string": "String"` property |
| `ByteBuffer` | `bytes` | |

**Decoder type widening**: Decoders accept narrower numeric types and widen them:
- `Long` decoder accepts Byte, Short, Int, Long
- `Int` decoder accepts Byte, Short, Int
- `Short` decoder accepts Byte, Short, Int (via `shortValue()`)
- `Byte` decoder accepts Byte, Int (via `byteValue()`)

### Strings

Three string schema variants:
1. **Default** (`StringSchemaFor`): standard Avro string type
2. **JavaString** (`JavaStringSchemaFor`): adds `"avro.java.string": "String"` property -- tells Avro to use `java.lang.String` instead of `Utf8`
3. **Fixed-length** (`fixedStringSchemaFor(name, size)`): creates FIXED schema

Also supported: `Utf8`, `CharSequence` (reuse `StringSchemaFor.forType`)

### Byte Collections

All map to Avro `bytes` type:

| Scala Type | Avro Schema |
|---|---|
| `Array[Byte]` | `bytes` |
| `List[Byte]` | `bytes` |
| `Seq[Byte]` | `bytes` |
| `Vector[Byte]` | `bytes` |
| `ByteBuffer` | `bytes` |

Top-level `Array[Byte]` and `ByteBuffer` are also supported.

### Collections

| Scala Type | Avro Schema | Notes |
|---|---|---|
| `List[T]` | `array` | Items schema from `SchemaFor[T]` |
| `Seq[T]` | `array` | |
| `Set[T]` | `array` | |
| `Vector[T]` | `array` | |
| `Array[T]` | `array` | For `T != Byte`; `Array[Byte]` maps to `bytes` |

**Tested with**: primitives, nested records, tuples, maps inside collections

### Maps

| Scala Type | Avro Schema | Notes |
|---|---|---|
| `Map[String, V]` | `map` | Keys must be `String`; values from `SchemaFor[V]` |

**Tested combinations**:
- `Map[String, String]`, `Map[String, Int]`, `Map[String, Double]`
- `Map[String, Nested]` (record values)
- `Map[String, Option[Boolean]]` (nullable values)
- `Map[String, Seq[String]]` (nested collections)
- `Map[String, Seq[Nested]]` (nested records in nested collections)
- Unicode keys supported (e.g., `"c"`, `"avro4s"` in Chinese)

### Options

`Option[T]` maps to a **union with null**: `["null", T]`

| Pattern | Avro Schema |
|---|---|
| `Option[String]` | `["null", "string"]` |
| `Option[Either[String, Boolean]]` | `["null", "string", "boolean"]` -- flattened |
| `Option[SealedTraitEnum]` | `["null", {"type": "enum", ...}]` |
| `Option[SealedTraitUnion]` | `["null", recordA, recordB, ...]` |
| `None.type` | `"null"` |
| `Option[T]` with default `Some(x)` | Default value placed first per Avro spec |

**Encoding**: `Some(value)` encodes the inner value directly; `None` encodes as `null`.

**Decoding**: `null` decodes to `None`; non-null decodes to `Some(value)`.

**Edge case (Issue #883)**: Generating `SchemaFor[Option[SealedTrait]]` must NOT mutate the original `SchemaFor[SealedTrait]` schema. The original union `[A, B]` must remain size 2 even after `Option` wraps it as `[null, A, B]` (size 3).

**Edge case (Issue #885)**: `Option[SealedTrait]` with only 1 subtype works correctly for round-trip.

**Edge case (Issue #890)**: Nested default `None` values: `case class Inner(a: Option[String] = None); case class Outer(b: Inner = Inner())` -- round-trips correctly.

### Either

`Either[A, B]` maps to **Avro union**: `[A, B]`

| Pattern | Avro Schema |
|---|---|
| `Either[String, Double]` | `["string", "double"]` |
| `Either[RecordA, RecordB]` | `[recordA, recordB]` |
| `Either[String, Option[Int]]` | `["null", "string", "int"]` -- flattened, null first |

Records in Either can have `@AvroNamespace` annotations.

### Sealed Traits

**Two modes determined at derivation time by `DatatypeShape`:**

#### 1. Enum Mode (all subtypes are case objects)

```scala
sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color
// Schema: {"type": "enum", "name": "Color", "symbols": ["Red", "Green", "Blue"]}
```

- Encoded as `GenericData.EnumSymbol`
- `@AvroSortPriority` controls symbol ordering (higher priority = earlier)
- `@AvroName` on case objects overrides symbol names
- `@AvroNamespace` on the trait overrides namespace
- `@AvroEnumDefault` sets the default enum value (important for schema evolution)
- Supported at both top-level and nested-in-record levels

#### 2. Union Mode (any subtype is a case class)

```scala
sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Square(side: Double) extends Shape
// Schema: [{"type": "record", "name": "Circle", ...}, {"type": "record", "name": "Square", ...}]
```

- Each subtype becomes a record schema in the union
- `@AvroUnionPosition(n)` controls subtype ordering in the union
- Subtypes without position annotation are placed after positioned ones
- Encoded as the subtype's `ImmutableRecord` with its own schema
- Decoded by matching `GenericContainer` schema name against subtype schemas
- Decoder throws `Avro4sDecodingException` if schema name not found

**Mixed hierarchies** (case objects + case classes in same sealed trait): treated as union mode.

**Trait subtypes with same field names**: supported -- fields resolve correctly because each subtype has its own record schema.

### Java Enums

Supported with dedicated annotations:
- `@AvroJavaName` -- override enum name
- `@AvroJavaNamespace` -- override namespace
- `@AvroJavaProp` -- add custom properties
- `@AvroJavaEnumDefault` -- set default value (field-level annotation)

Java enum constants extracted via `runtimeClass.getEnumConstants`.

### Scala 3 Enums

Supported via compile-time macro using `scala.quoted`:
- Simple enums (no parameters)
- Extending enums (explicit inheritance)
- Parametrized enums (with Int parameters)
- Annotated versions with `@AvroNamespace` and `@AvroName`
- Round-trip encode/decode tested

**Note**: `ScalaEnums.scala` macro currently appears incomplete (returns intType placeholder in source) but test files show working enum support through Magnolia sealed trait derivation.

### Scala 2 Enumeration

`ScalaEnumSchemaFor[MyEnum.Value](defaultValue)` -- creates enum schema with optional default. Supported via the `ScalaEnums` object which builds schemas, encoders, and decoders using runtime reflection (`runtimeMirror`).

### Value Classes (AnyVal)

```scala
case class MyId(value: String) extends AnyVal
```

- Schema: same as the wrapped type (e.g., `"string"`)
- `@AvroFixed(size)` annotation can override to produce FIXED schema
- Supported at top-level and nested in records
- Optional value types `Option[MyValueType]` work correctly (Issue #191)
- Encoder unwraps the value; decoder wraps via `rawConstruct`

### Tuples

Tuples (2 through 5+) map to **Avro records** with namespace `"scala"`:

| Scala Type | Avro Schema |
|---|---|
| `(A, B)` | `{"type": "record", "name": "Tuple2", "namespace": "scala", "fields": [{"name": "_1", ...}, {"name": "_2", ...}]}` |
| `(A, B, C)` | Similar with `_1`, `_2`, `_3` |
| up to Tuple5 | Tested; Tuple6 has a bug (field `_6` references wrong type param) |

### UUID

- Schema: string with logical type `"uuid"`
- `Option[UUID]` as union `["null", uuid_string]`
- `Seq[UUID]` as array of uuid strings
- UUID with default values supported

### BigDecimal

Default schema: `bytes` with `decimal` logical type (precision=8, scale=2 by default).

**Configuration via `ScalePrecision`:**
```scala
given ScalePrecision = ScalePrecision(scale = 3, precision = 9)
```

**Alternative representations:**
- `BigDecimals.AsString` -- encodes as Avro `string` type
- Custom FIXED schema -- `Schema.createFixed("bigdecimal", null, null, 55)`

**Round-trip behavior with scale:**
- Values padded to configured scale: `123.45` with scale=3 becomes `123.450`
- Values rounded: `123.4567` with scale=3 becomes `123.457`
- Exact values preserved: `123.456` stays `123.456`

**Optional BigDecimal**: union `["null", decimal_bytes]`
**Seq[BigDecimal]**: array of decimal_bytes

### Date/Time Types (Logical Types)

| Scala/Java Type | Avro Schema | Logical Type |
|---|---|---|
| `LocalDate` | `int` | `date` |
| `java.sql.Date` | `int` | `date` |
| `LocalTime` | `long` | `time-micros` |
| `Instant` | `long` | `timestamp-millis` |
| `java.sql.Timestamp` | `long` | `timestamp-millis` |
| `LocalDateTime` | `long` | `timestamp-nanos` (custom logical type) |
| `OffsetDateTime` | `string` | `datetime-with-offset` (custom logical type) |

**Encoder precision variants** (for Instant, LocalDateTime, Timestamp):
- `timestamp-millis`: epoch milliseconds
- `timestamp-micros`: epoch microseconds
- `timestamp-nanos`: epoch nanoseconds (seconds * 1e9 + nanos)

**Default value handling for Instant**:
- `Instant.MAX` maps to `Long.MaxValue`
- `Instant.MIN` maps to `Long.MinValue`
- Normal instants convert to epoch millis

### Generic Types

```scala
case class Generic[T](t: T)
// Generic[String] and Generic[Int] produce different schemas
// Schema names include type parameter info: "Generic__String", "Generic__Int"
```

- `@AvroErasedName` annotation disables type parameter encoding in schema name -- uses raw name only
- When erased, different type param instantiations collide (causing schema parse errors)
- Nested generics tested: `case class SameGenericWithDifferentTypeArgs(gi: Generic[Int], gs: Generic[String])`

### Shapeless Coproducts

```scala
type ISBG = Int :+: String :+: Boolean :+: Gimble :+: CNil
```

- Maps to Avro UNION
- Schema, encoder, decoder all provided
- `CNilEncoder` throws exception (unreachable)
- Decoding uses type-guarded dispatch (`PartialFunction`)
- Optional coproducts: `Option[Coproduct]` wraps in nullable union
- Coproducts containing ADTs work (Issue #318)
- Nested coproducts (coproduct of coproducts) supported

### Recursive Types

Tested patterns:
- **Self-recursive via List**: `case class ListTree[+T](value: T, children: Seq[ListTree[T]] = Seq.empty)`
- **Self-recursive via Map**: `case class MapTree[+T](value: T, children: Map[String, MapTree[T]] = Map.empty)`
- **Self-recursive via Option**: `case class OptionTree[T](value: T, left: Option[OptionTree[T]], right: Option[OptionTree[T]])`
- **Mutual recursion**: `case class MutRec1(payload: Int, children: List[MutRec2])` / `case class MutRec2(payload: String, children: List[MutRec1])`
- **Either-based tree**: `type EitherTree[T] = Either[EitherBranch[T], EitherLeaf[T]]`

---

## Annotations

| Annotation | Target | Parameters | Purpose |
|---|---|---|---|
| `@AvroName(name)` | Class, Field | `name: String` | Override name in schema (affects serialization/deserialization) |
| `@AvroNamespace(namespace)` | Class, Field | `namespace: String` | Override namespace; field-level overrides class-level |
| `@AvroDoc(doc)` | Class, Field | `doc: String` | Attach documentation to schema records/fields |
| `@AvroAlias(alias)` | Class, Field | `alias: String` | Add alias name; multiple aliases supported (stackable) |
| `@AvroProp(key, value)` | Class, Field | `key: String`, `value: String \| JsonNode` | Add custom properties; supports JSON values |
| `@AvroTransient` | Field | none | Exclude field from schema/serialization entirely |
| `@AvroNoDefault` | Field | none | Suppress Scala default value in schema |
| `@AvroFixed(size)` | Field, Value class | `size: Int` | Override schema to FIXED type with given byte size |
| `@AvroError` | Class, Field | none | Mark record as Avro error type |
| `@AvroSortPriority(priority)` | Field (enum/union subtypes) | `priority: Float` | Control ordering in enums/unions (higher = earlier) |
| `@AvroUnionPosition(position)` | Class (sealed trait subtypes) | `position: Int` | Explicit position in union schema (ascending order) |
| `@AvroEnumDefault(default)` | Enum | `default: Any` | Set default enum value for schema evolution |
| `@AvroErasedName` | Class (generic) | none | Disable type parameter info in schema record name |
| `@AvroScalePrecision(scale, precision)` | Field | `scale: Int`, `precision: Int` | Configure decimal logical type |

**Java enum annotations** (in Java source files):
| Annotation | Purpose |
|---|---|
| `@AvroJavaName` | Override Java enum name |
| `@AvroJavaNamespace` | Override Java enum namespace |
| `@AvroJavaProp` | Add custom properties to Java enum |
| `@AvroJavaEnumDefault` | Set default value for Java enum field |

### Annotation Precedence

- Field-level `@AvroNamespace` takes precedence over class-level `@AvroNamespace`
- `@AvroName` overrides field mapping transformations (Issue #396)
- `@AvroName` on enum symbols: if name equals the enumeration name, it is skipped to avoid name clash

---

## Schema Derivation Features

### Record Schema Generation (case classes)

Process in `Records.schema`:
1. Create record via `Schema.createRecord()` with sanitized name and optional `@AvroDoc`
2. Iterate parameters, filtering `@AvroTransient` fields
3. For each field:
   - Apply `@AvroName` for custom naming
   - Apply `@AvroDoc`, `@AvroAlias`, `@AvroProp`
   - Resolve type (check `@AvroFixed` override)
   - Resolve default values via `DefaultResolver`
   - If union schema: move default type to first position (null default = null first)
   - If `@AvroNamespace` on field: recursively override namespace in nested schemas
4. Set fields on record, return `SchemaFor[T]`

### Default Value Handling

**Scala defaults automatically picked up** unless `@AvroNoDefault` is present.

Tested default types:
- Primitives: String, Int, Boolean, Double, Long, Float
- Collections: Map, Set, Seq (default empty or populated)
- Optional collections: `Option[Seq[...]]`, `Option[Set[...]]`, `Option[Map[...]]`
- Temporal: Instant
- Sealed trait case objects as defaults
- Sealed trait case classes as defaults
- Optional sealed trait hierarchies with default `None`
- Nested defaults: `case class Outer(b: Inner = Inner())` where `Inner` has its own defaults

**DefaultResolver** converts Scala values to Avro-compatible types:
- `None` -> `JsonProperties.NULL_VALUE`
- `Some(x)` -> recursive resolution
- `UUID`, `Utf8` -> String
- `Instant` -> epoch millis (Long); `Instant.MAX` -> `Long.MaxValue`, `Instant.MIN` -> `Long.MinValue`
- `GenericFixed` -> byte array
- `BigDecimal` -> String
- `ByteBuffer` with decimal logical type -> Double via `DecimalConversion`
- `ByteBuffer` -> byte array
- Primitives -> Java boxed types
- Maps, Sequences, Sets -> Java equivalents via `.asJava`
- Products -> `CustomDefaults.customDefault` (handles enums and union defaults)

### Field Mappers

| Mapper | Transformation | Example |
|---|---|---|
| `DefaultFieldMapper` | No change | `firstName` -> `firstName` |
| `PascalCase` | Capitalize first char | `firstName` -> `FirstName` |
| `SnakeCase` | camelCase to snake_case | `firstName` -> `first_name` |
| `LispCase` | camelCase to lisp-case | `firstName` -> `first-name` |

Applied via `SchemaFor[T].withFieldMapper(SnakeCase)`.

### SchemaFor Typeclass Overrides

Custom schemas can be provided by defining implicit `SchemaFor[T]`:

```scala
given SchemaFor[String] = JavaStringSchemaFor        // Override string representation
given SchemaFor[BigDecimal] = BigDecimals.AsString    // BigDecimal as string
given SchemaFor[Foo] = SchemaFor(customSchema)        // Completely custom schema
```

Value type overrides also supported for both nested and top-level usage.

### Schema Merge

`AvroSchemaMerge.apply(name, namespace, schemas)` merges multiple schemas:
- Fields become optional when either source marks them as such
- Nullable fields with defaults are preserved

---

## Encoder Features

### Record Encoding

- Case class fields encoded into `ImmutableRecord` with field values as `Vector[AnyRef]`
- `@AvroTransient` fields excluded (encoder skips them)
- `@AvroName` respected for field naming
- Nested records encoded recursively
- `SchemaUpdate` pattern allows external schema to drive encoding modifications

### Type-specific Encoding

| Type | Encoded As |
|---|---|
| `String` | `Utf8` |
| `Int` | `java.lang.Integer` |
| `Long` | `java.lang.Long` |
| `Boolean` | `java.lang.Boolean` |
| `Float` | `java.lang.Float` |
| `Double` | `java.lang.Double` |
| `UUID` | `Utf8` (string representation) |
| `BigDecimal` | Byte array via `DecimalConversion.toBytes()`, or Utf8 if AsString |
| `Instant` | `java.lang.Long` (epoch millis/micros/nanos depending on logical type) |
| `LocalDate` | `java.lang.Integer` (epoch day) |
| `LocalTime` | `java.lang.Long` (nanos converted per logical type) |
| `OffsetDateTime` | String (ISO format) |
| Sealed trait enum | `GenericData.EnumSymbol` |
| Sealed trait union | Encoded as subtype's record |
| Value class | Unwrapped inner value |
| Tuple | Record with `_1`, `_2`, etc. fields |

---

## Decoder Features

### Record Decoding

- Generic records decoded by matching field names to case class parameters
- **Field reordering**: decoder handles schema field order different from case class parameter order
- `@AvroTransient` fields: position `-1`, triggers default value or Option handling
- `@AvroName` respected for field lookup
- Missing fields with defaults: Scala defaults applied (Issue #110)
- Missing `Option[T]` fields: decoded as `None`

### Type-Guarded Decoding

`TypeGuardedDecoding[T]` provides `guard(schema): PartialFunction[Any, Boolean]` for safe type matching:

| Type | Accepts |
|---|---|
| `String` | `String`, `Utf8` |
| `Boolean` | `Boolean` |
| `Int` | `Int` |
| `Long` | `Long` |
| `Double` | `Double` |
| `UUID` | `String`, `Utf8` |
| `ByteBuffer` | `ByteBuffer`, `Array[Byte]` |
| `Array[Byte]` | `ByteBuffer`, `Array[Byte]` |
| `Map[String, T]` | Java `Map`, `HashMap` |
| `List[T]`, `Seq[T]` | arrays, Java collections, Iterables |
| Generic records | `GenericContainer` with matching schema name |

Used for union type dispatch: each branch tested against the value to find the matching decoder.

### Fast vs Safe Decoding

`FieldDecoder` provides two methods:
- `fastDecodeFieldValue`: position-based field lookup (by index)
- `safeDecodeFieldValue`: name-based field lookup (by field name)

Falls back to defaults or null-decoding for missing fields.

---

## Logical Types

### Built-in Logical Types

| Logical Type | Backing Type | Scala Type |
|---|---|---|
| `date` | `int` | `LocalDate`, `java.sql.Date` |
| `time-micros` | `long` | `LocalTime` |
| `timestamp-millis` | `long` | `Instant`, `java.sql.Timestamp` |
| `timestamp-nanos` | `long` | `LocalDateTime` (custom logical type) |
| `datetime-with-offset` | `string` | `OffsetDateTime` (custom logical type) |
| `decimal(precision, scale)` | `bytes` | `BigDecimal` |
| `uuid` | `string` | `UUID` |

### Custom Logical Types

Two custom logical types defined in `logicals.scala`:

1. **`TimestampNanosLogicalType`**: extends `LogicalType("timestamp-nanos")`, validates backing type is `LONG`
2. **`OffsetDateTimeLogicalType`**: extends `LogicalType("datetime-with-offset")`, validates backing type is `STRING`

Both throw `IllegalArgumentException` on type mismatch.

---

## Schema Evolution

### Backward Compatibility (reader has newer schema)

Tested in `SchemaEvolutionTest`:

1. **New field with default**: Reader schema has `age: Int = 18`, writer has only `name: String` -- default `18` applied
2. **Schema-level default**: Missing field `b` with schema default `"foo"` is applied
3. **New Optional field**: Missing `Option[T]` field decoded as `None`

### Forward Compatibility

Tested in `Github587`:
- Union evolution with `@AvroUnionPosition` for stable ordering
- New subtypes in V2 unknown to V1 decoded as fallback (`Unknown` case object at position 0)
- Modified subtypes (added fields with defaults) decoded with available fields

### Enum Evolution (EnumSchemaCompatibilityTest)

**Without defaults**: Adding new enum value is forward-compatible but NOT backward-compatible.
**With defaults** (`ScalaEnumSchemaFor[T](defaultValue)`): Both forward and backward compatible -- unknown symbols resolve to default.

### Union Evolution Edge Cases (Github587 -- detailed)

Schema V1 and V2 with `@AvroUnionPosition` annotations:

| Direction | Scenario | Result |
|---|---|---|
| V2 -> V1 (backward) | Banana (unchanged) | Decoded correctly |
| V2 -> V1 (backward) | Mango (extra field in V2) | Decoded, extra field dropped |
| V2 -> V1 (backward) | Apple (new in V2, not in V1) | Falls back to `Unknown` |
| V1 -> V2 (forward) | Banana (unchanged) | Decoded correctly |
| V1 -> V2 (forward) | Mango (fewer fields in V1) | Falls back to `Unknown` |
| V1 -> V2 (forward) | Orange (new required field has default in V2) | Decoded, default applied |
| V1 -> V2 (forward) | Lemon (field removed in V2) | Falls back to `Unknown` |

---

## Edge Cases Tested (GitHub Issues)

| Issue | Description | Test |
|---|---|---|
| #69 | Generic type schema derivation: `Message[MyRecord]` | Works with type parameter info in name |
| #110 | Default value picked up during FromRecord conversion | Scala default applied for missing field |
| #191 | AnyVal wrapped in Option field round-trip | `Option[SN]` where `SN extends AnyVal` works |
| #260 | Schema generation determinism (commented out) | Schema fingerprints should be identical across runs |
| #292 | ADT with type-parametrized values breaks derivation | Generic in sealed hierarchy works |
| #318 | Coproduct containing ADT | `MyAdt :+: Boolean :+: CNil` works |
| #396 | `@AvroName` overrides FieldMapper for schema, encoding, decoding | Field mapping + annotation coexist |
| #484 | Serializable Scala Enum Decoder | Decoder survives Java serialization round-trip |
| #587 | Union evolution with `@AvroUnionPosition` | Forward/backward compatibility with fallback |
| #883 | Option schema must not mutate original sealed trait schema | Union size preserved after Option wrapping |
| #885 | `Option[SealedTrait]` with single subtype | Round-trips correctly |
| #890 | Nested default `None` values | `Outer(b: Inner = Inner())` with `Inner(a: Option[String] = None)` |

---

## Error Handling

### Exception Hierarchy

```
Avro4sException(message: String) extends Exception
  +-- Avro4sConfigurationException(message: String)
  +-- Avro4sEncodingException(message: String)
  +-- Avro4sDecodingException(message: String, value: Any)  // captures problematic value
```

### Error Conditions

- **Union decode failure**: `Avro4sDecodingException` with available schema names listed
- **Non-GenericContainer in union**: throws for unsupported types
- **Incompatible logical type**: `Avro4sConfigurationException` for unsupported timestamp precision
- **CNil encoder**: throws (unreachable coproduct tail)
- **Schema validation**: `require(schema.isUnion)` for union encoders/decoders; `require(schema.getType == ENUM)` for enum decoders

---

## Scala 3 Specific Features

### Supported

- **Scala 3 enums**: simple, extending, parametrized, annotated (schema, encoder, decoder)
- **Given/using syntax**: all typeclass instances use Scala 3 `given` definitions
- **Inline + macro**: `AvroSchema` uses `inline def` for compile-time schema generation
- **Quoted macros**: `ScalaEnums` uses `scala.quoted` for enum schema compilation

### Not Supported / Open Issues

| Feature | Status | Issue |
|---|---|---|
| Scala 3 Union Types (`A | B`) | Not supported | [#811](https://github.com/sksamuel/avro4s/issues/811) -- open, labeled "abandoned" |
| Opaque Types | Not supported | [#827](https://github.com/sksamuel/avro4s/issues/827) -- open |
| Named Tuples | Not mentioned | No issue found |
| `IArray` | Not mentioned | No issue found |
| Literal Types | Not mentioned | No issue found |

---

## Test File Inventory

### Schema Tests (40 files in `schema/`)

```
ArraySchemaTest, AvroAliasSchemaTest, AvroDocSchemaTest, AvroErrorSchemaTest,
AvroFixedSchemaTest, AvroNameSchemaTest, AvroNamespaceSchemaTest, AvroNoDefaultTest,
AvroPropSchemaTest, AvroSchemaMergeTest, AvroSortPrioritySchemaTest,
AvroUnionPositionSchemaTest, BasicSchemasTest, BigDecimalSchemaTest,
ByteArraySchemaTest, CoproductSchemaTest, DateSchemaTest, DefaultValueRecordTest,
DefaultValueSchemaTest, EitherSchemaTest, EnumSchemaCompatibilityTest, EnumSchemaTest,
FieldMapperFieldTest, GenericSchemaTest, MapSchemaTest, NamespaceSchemaTest,
OptionSchemaTest, PrimitiveSchemaTest, SchemaForTypeclassOverrideTest,
SealedTraitSchemaTest, StringSchemasTest, TransientSchemaTest, TupleSchemaTest,
UUIDSchemaTest, UtfSchemaTest, ValueTypeSchemaTest
```

### Encoder Tests (24 files in `record/encoder/`)

```
ArrayEncoderTest, AvroNameEncoderTest, AvroTransientEncoderTest, BasicEncoderTest,
BigDecimalEncoderTest, ByteArrayEncoderTest, CoproductEncoderTest, DateEncoderTest,
EitherEncoderTest, EncoderTypeclassOverrideTest, EnumEncoderTest,
FieldMapperEncoderTest, FixedEncoderTest, MapEncoderTest, NestedStructEncoderTest,
OptionEncoderTest, ReorderFieldsEncoderTest, Scala3EnumEncoderTest,
SealedTraitEncoderTest, StringEncoderTest, StructEncoderTest, TupleEncoderTest,
UUIDEncoderTest, ValueTypeEncoderTest
```

### Decoder Tests (24 files in `record/decoder/`)

```
ArrayDecoderTest, AvroNameDecoderTest, BasicDecoderTest, BigDecimalDecoderTest,
ByteArrayDecoderTest, CoproductDecoderTest, DateDecoderTest,
DecoderTypeclassOverrideTest, EitherDecoderTest, EnumDecoderTest,
FieldMapperDecoderTest, FixedDecoderTest, MapDecoderTest, OptionDecoderTest,
ReorderFieldsDecoderTest, Scala3EnumDecoderTest, SchemaEvolutionTest,
SealedTraitDecoderTest, StringDecoderTest, StructDecoderTest, TransientDecoderTest,
TupleDecoderTest, UUIDDecoderTest, ValueTypeDecoderTest
```

### GitHub Issue Regression Tests (39 files in `github/`)

Issues covered: 12, 69, 110, 152, 180, 187, 191, 193, 202, 205, 234, 235, 247, 254, 260, 265, 273, 281, 284, 292, 295, 318, 330, 331, 346, 387, 389, 396, 408, 411, 415, 432, 484, 485, 510, 545, 587, 883, 885, 890

### Other Test Files

```
record/BigDecimalRoundTrip.scala
record/FromRecordTest.scala
record/LocalDateTimeRoundTrip.scala
record/ToRecordTest.scala
record/TypeGuardedDecoderTest.scala
Recursive.scala
SchemaForUtf8Test.scala
```

---

## Source File Structure

### Schemas (`schemas/`)
```
bigdecimals, bytes, collections, eithers, enums, logicals, magnolia, options,
primitives, records, scalaenums, sealedtraits, strings, temporal, tuples, unions, valuetypes
```

### Encoders (`encoders/`)
```
bigdecimals, bytes, collections, eithers, magnolia, options, primitives,
records, sealedtraits, strings, temporal, tuples, unions
```

### Decoders (`decoders/`)
```
bigdecimals, bytes, collections, eithers, magnolia, options, primitives,
records, sealedtraits, strings, temporal, unions
```

### Core Files
```
AvroSchema, Codec, CustomDefaults, DefaultResolver, Encoder, Decoder,
FieldMapper, FromRecord, ToRecord, ImmutableRecord, RecordFields, RecordFormat,
Records, SchemaFor, ScalaEnums, ShapelessCoproducts, SparkSchemas,
TypeGuardedDecoding, ValueTypes, annotations, exceptions, format
```

---

## Summary of Key Design Decisions

1. **Magnolia for derivation** -- compile-time, no runtime reflection for case class/sealed trait derivation
2. **Schema-driven encoding/decoding** -- `encode(schema)` and `decode(schema)` take the schema, enabling schema evolution
3. **Type-guarded decoding** -- PartialFunction-based dispatch for union types
4. **Annotation-first configuration** -- rich annotation set for schema customization
5. **Separate SchemaFor/Encoder/Decoder** -- can be independently overridden/customized
6. **Shapeless coproducts** for union types (Scala 3 union types not yet supported)
7. **ImmutableRecord** as the standard intermediate representation for records
8. **Three output formats** -- Binary, JSON, Data (with/without embedded schema)
