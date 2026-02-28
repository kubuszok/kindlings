# Tapir Schema Upstream Library Research

## Library Version
- **tapir**: 1.11.16 (used by kindlings), latest master is 1.13.9
- **Package**: `com.softwaremill.sttp.tapir` / `sttp.tapir`
- **Source**: https://github.com/softwaremill/tapir

---

## Schema Case Class

```scala
case class Schema[T](
  schemaType: SchemaType[T],
  name: Option[SName] = None,
  isOptional: Boolean = false,
  description: Option[String] = None,
  default: Option[(T, Option[Any])] = None,   // (value, optionalEncodedForm)
  format: Option[String] = None,
  encodedExample: Option[Any] = None,
  deprecated: Boolean = false,
  hidden: Boolean = false,
  validator: Validator[T] = Validator.pass[T],
  attributes: AttributeMap = AttributeMap.Empty
)
```

### Key Schema Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `description` | `(d: String): Schema[T]` | Set description |
| `title` | `(t: String): Schema[T]` | Set title via `Title` attribute |
| `format` | `(f: String): Schema[T]` | Set format string |
| `deprecated` | `(d: Boolean): Schema[T]` | Mark as deprecated |
| `hidden` | `(h: Boolean): Schema[T]` | Hide from documentation |
| `default` | `(t: T, encoded: Option[Any]): Schema[T]` | Set default value |
| `encodedExample` | `(e: Any): Schema[T]` | Set encoded example |
| `validate` | `(v: Validator[T]): Schema[T]` | Add validator |
| `encodedDiscriminatorValue` | `(v: String): Schema[T]` | Set discriminator attribute |
| `modifyUnsafe` | `[U](fields: String*)(modify: Schema[U] => Schema[U]): Schema[T]` | Modify nested schema by path |
| `attribute` | `[A](k: AttributeKey[A], v: A): Schema[T]` | Set custom attribute |
| `asOption` | `: Schema[Option[T]]` | Wrap in SOption |
| `asArray` | `: Schema[Array[T]]` | Wrap in SArray |
| `asIterable[C[X] <: Iterable[X]]` | `: Schema[C[T]]` | Wrap in SArray for collection |

### Schema Companion Factory Methods

| Method | Returns | Description |
|--------|---------|-------------|
| `string[T]` | `Schema[T]` | Schema with `SString` type |
| `binary[T]` | `Schema[T]` | Schema with `SBinary` type |
| `any[T]` | `Schema[T]` | Unrestricted schema (SCoproduct with empty subtypes) |
| `anyObject[T]` | `Schema[T]` | Object schema (SProduct with no fields) |
| `derived[T]` | `Schema[T]` | Semi-automatic derivation |
| `derivedSchema[T]` | Magnolia-based | Auto-derivation entry point |
| `wrapWithSingleFieldProduct[T]` | `Schema[T]` | Wraps schema in single-field product |
| `oneOfUsingField[E, V]` | macro | Discriminator from trait field |
| `oneOfWrapped[T]` | macro | Discriminator via wrapper objects |
| `derivedUnion[T]` | `Schema[T]` | Scala 3 union type derivation |
| `derivedEnumeration[T]` | `CreateDerivedEnumerationSchema[T]` | Enum schema builder |
| `derivedEnumerationValue[T]` | `Schema[T]` | Scala `Enumeration#Value` schema |
| `derivedEnumerationValueCustomise[T]` | `CreateDerivedEnumerationSchema[T]` | Customizable `Enumeration` schema |
| `derivedStringBasedUnionEnumeration[T]` | `Schema[T]` | Scala 3 literal union as string enum |
| `derivedWithTypeParameter[G[_], T]` | `Schema[G[T]]` | Generic class with named type param |

### Built-in Implicit Schemas

Provided for: `String`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Boolean`, `BigDecimal`, `java.math.BigDecimal`, `UUID`, `java.time.Instant`, `java.time.ZonedDateTime`, `java.time.OffsetDateTime`, `java.time.LocalDateTime`, `java.time.LocalDate`, `java.time.LocalTime`, `java.time.OffsetTime`, `java.time.Duration`, `java.io.File`, `java.io.InputStream`, `java.nio.ByteBuffer`, `Array[Byte]`, `Option[T]`, `Array[T]`, `List[T]`, `Set[T]`, `Vector[T]`, `Map[String, V]`, `Either[A, B]`.

---

## Schema Types (SchemaType Hierarchy)

```scala
sealed trait SchemaType[T] {
  def show: String
  def contramap[TT](g: TT => T): SchemaType[TT]
  def as[TT]: SchemaType[TT]
}
```

| Type | Parameters | When Used |
|------|-----------|-----------|
| `SString[T]` | (none) | Strings, enums with string encoding |
| `SInteger[T]` | (none) | Byte, Short, Int, Long |
| `SNumber[T]` | (none) | Float, Double, BigDecimal |
| `SBoolean[T]` | (none) | Boolean |
| `SBinary[T]` | (none) | Array[Byte], InputStream, File |
| `SDate[T]` | (none) | LocalDate |
| `SDateTime[T]` | (none) | Instant, ZonedDateTime, OffsetDateTime, LocalDateTime |
| `SProduct[T]` | `fields: List[SProductField[T]]` | Case classes, tuples |
| `SCoproduct[T]` | `subtypes: List[Schema[?]]`, `discriminator: Option[SDiscriminator]`, `subtypeSchema: T => Option[SchemaWithValue[?]]` | Sealed traits, enums with payload |
| `SArray[T, E]` | `element: Schema[E]`, `toIterable: T => Iterable[E]` | List, Vector, Set, Array |
| `SOption[T, E]` | `element: Schema[E]`, `toOption: T => Option[E]` | Option types |
| `SOpenProduct[T, V]` | `fields: List[SProductField[T]]`, `valueSchema: Schema[V]`, `mapFieldValues: T => Map[String, V]` | Map types |
| `SRef[T]` | `name: SName` | Recursive type references (lazy) |

### SProductField

```scala
trait SProductField[T] {
  type FieldType
  def name: FieldName
  def schema: Schema[FieldType]
  def get: T => Option[FieldType]
}
```

- `equals` compares `name` and `schema` (not `get`)
- Factory: `SProductField[T, F](name: FieldName, schema: Schema[F], get: T => Option[F])`

### SProduct Methods

| Method | Description |
|--------|-------------|
| `required` | `List[FieldName]` of non-optional fields |
| `fieldsWithValidation` | Fields where `schema.hasValidation` is true |
| `SProduct.empty[T]` | Empty product with no fields |

### SCoproduct Methods

| Method | Signature | Description |
|--------|-----------|-------------|
| `addDiscriminatorField` | `[D](name: FieldName, discriminatorSchema: Schema[D] = Schema.string, discriminatorMapping: Map[String, SRef[?]] = Map.empty)` | Adds discriminator field to all child product schemas, sets discriminator on coproduct, and attaches `encodedDiscriminatorValue` attribute to each child |

`addDiscriminatorField` behavior:
1. Only adds the field to child schemas if not already present (idempotent)
2. Sets `SDiscriminator(name, mapping)` on the coproduct
3. Each child schema gets an `encodedDiscriminatorValue` attribute via reverse-lookup in the mapping
4. The discriminator field schema uses `Validator.Enumeration` with a single possible value

### SDiscriminator

```scala
case class SDiscriminator(
  name: FieldName,
  mapping: Map[String, SRef[?]]
)
```

### SchemaWithValue

```scala
case class SchemaWithValue[T](schema: Schema[T], value: T)
```

---

## SName

```scala
case class SName(
  fullName: String,
  typeParameterShortNames: List[String] = Nil
) {
  def show: String = ...  // "fullName" or "fullName<param1,param2>"
}

object SName {
  val Unit: SName = SName(fullName = "Unit")
}
```

- `fullName`: Fully qualified class name (e.g., `"sttp.tapir.tests.Entity"`)
- `typeParameterShortNames`: List of fully qualified names of type arguments, flattened recursively
- `@encodedName` annotation overrides `fullName` and clears type parameter names

### SName Construction During Derivation

```scala
def typeNameToSchemaName(typeName: TypeInfo, annotations: Seq[Any]): SName = {
  def allTypeArguments(tn: TypeInfo): Seq[TypeInfo] =
    tn.typeParams.toList.flatMap(tn2 => tn2 +: allTypeArguments(tn2))
  annotations.collectFirst { case ann: Schema.annotations.encodedName => ann.name } match {
    case Some(altName) => SName(altName, Nil)
    case None => SName(typeName.full, allTypeArguments(typeName).map(_.full).toList)
  }
}
```

Type parameter names are **flattened recursively**: for `Wrapper[Map[String, Int]]`, the type parameters include `Map`, `String`, and `Int`.

---

## FieldName

```scala
case class FieldName(name: String, encodedName: String)

object FieldName {
  def apply(name: String): FieldName = FieldName(name, name) // identity encoding
}
```

- `name`: Original Scala field name
- `encodedName`: Name used in serialized/documented form (may differ due to `@encodedName` or `Configuration.toEncodedName`)

---

## Annotations

All annotations are in `Schema.annotations`:

| Annotation | Class | Parameters | Effect on Schema |
|-----------|-------|-----------|-----------------|
| `@description` | `class description(val text: String)` | `text: String` | Sets `schema.description` |
| `@title` | `class title(val name: String)` | `name: String` | Sets `Title` attribute via `schema.title()` |
| `@format` | `class format(val format: String)` | `format: String` | Sets `schema.format` |
| `@deprecated` | `class deprecated` | (none) | Sets `schema.deprecated = true` |
| `@hidden` | `class hidden` | (none) | Sets `schema.hidden = true` |
| `@encodedExample` | `class encodedExample(val example: Any)` | `example: Any` | Sets `schema.encodedExample` |
| `@encodedName` | `class encodedName(val name: String)` | `name: String` | Overrides field encoding name; on types, overrides `SName.fullName` |
| `@default` | `class default[T](val default: T, val encoded: Option[Any] = None)` | `default: T`, `encoded: Option[Any]` | Sets `schema.default` with optional encoded form |
| `@validate` | `class validate[T](val v: Validator[T])` | `v: Validator[T]` | Adds validator to schema |
| `@validateEach` | `class validateEach[T](val v: Validator[T])` | `v: Validator[T]` | Adds validator to collection elements via `modifyUnsafe(ModifyCollectionElements)` |
| `@customise` | `class customise(val f: Schema[?] => Schema[?])` | `f: Schema[?] => Schema[?]` | Applies arbitrary transformation function (unsafe cast back) |

### How Annotations are Applied (enrichSchema)

In `SchemaMagnoliaDerivation`, annotations are folded over the schema sequentially:

```scala
annotations.foldLeft(schema) {
  case (schema, ann: Schema.annotations.description) => schema.description(ann.text)
  case (schema, ann: Schema.annotations.encodedExample) => schema.encodedExample(ann.example)
  case (schema, ann: Schema.annotations.default[X @unchecked]) => schema.default(ann.default, ann.encoded)
  case (schema, ann: Schema.annotations.validate[X @unchecked]) => schema.validate(ann.v)
  case (schema, ann: Schema.annotations.validateEach[X @unchecked]) =>
    schema.modifyUnsafe(Schema.ModifyCollectionElements)(_.validate(ann.v))
  case (schema, ann: Schema.annotations.format) => schema.format(ann.format)
  case (schema, ann: Schema.annotations.title) => schema.title(ann.name)
  case (schema, _: Schema.annotations.deprecated) => schema.deprecated(true)
  case (schema, ann: Schema.annotations.customise) => ann.f(schema).asInstanceOf[Schema[X]]
  case (schema, _) => schema  // ignore unknown annotations
}
```

### SchemaAnnotations (Compile-Time Macro Derivation)

For semi-automatic derivation, `SchemaAnnotations[T]` is derived via macro:

```scala
final case class SchemaAnnotations[T](
  description: Option[String] = None,
  encodedExample: Option[Any] = None,
  default: Option[(T, Option[Any])] = None,
  format: Option[String] = None,
  deprecated: Option[Boolean] = None,
  hidden: Option[Boolean] = None,
  encodedName: Option[String] = None,
  validate: List[Validator[T]] = Nil,
  validateEach: List[Validator[?]] = Nil
)
```

The `enrich(s: Schema[T])` method applies these in order:
1. description
2. encodedExample
3. default (with encoded form)
4. format
5. deprecated
6. hidden
7. encodedName (to SName)
8. validate (folded)
9. validateEach (via modifyUnsafe)

Note: `@title` and `@customise` are **not** handled by `SchemaAnnotations`; they are only handled in the Magnolia derivation path via `enrichSchema`.

### Annotation Application Scope

- **Type-level annotations**: Applied to the entire derived schema (sealed trait or case class level)
- **Field-level annotations**: Applied to individual `SProductField` schemas within products
- **Inherited annotations**: Merged with direct annotations; field annotations from parent traits propagate to subtypes
- **Annotation merge**: `mergeAnnotations` filters out duplicates by class type

---

## Configuration

```scala
final case class Configuration(
  toEncodedName: String => String,
  discriminator: Option[String],
  toDiscriminatorValue: SName => String
)
```

### Factory/Builder Methods

| Method | Effect |
|--------|--------|
| `Configuration.default` | Identity name encoding, no discriminator, short subtype names |
| `.withSnakeCaseMemberNames` | `someFieldName` -> `some_field_name` |
| `.withKebabCaseMemberNames` | `someFieldName` -> `some-field-name` |
| `.withScreamingSnakeCaseMemberNames` | `someFieldName` -> `SOME_FIELD_NAME` |
| `.withDiscriminator(d: String)` | Sets discriminator field name |
| `.withSnakeCaseDiscriminatorValues` | Subtype names in snake_case |
| `.withKebabCaseDiscriminatorValues` | Subtype names in kebab-case |
| `.withScreamingSnakeCaseDiscriminatorValues` | Subtype names in SCREAMING_SNAKE_CASE |
| `.withFullDiscriminatorValues` | Uses fully qualified class names |
| `.withFullSnakeCaseDiscriminatorValues` | Full path + snake_case |
| `.withFullKebabCaseDiscriminatorValues` | Full path + kebab-case |

### How Configuration Affects Derivation

1. **Field names**: `toEncodedName(fieldLabel)` is called for each product field to produce `FieldName(original, encoded)`, unless overridden by `@encodedName`
2. **Discriminator**: When `discriminator` is `Some("field_name")`:
   - A string field is added to all child product schemas
   - `toDiscriminatorValue(childSchemaName)` maps each subtype to its discriminator value
   - `SDiscriminator` is set on the `SCoproduct`
3. **Priority**: `@encodedName` annotation takes precedence over `Configuration.toEncodedName`

---

## Schema Derivation Features

### Feature: Automatic Derivation (import-based)

**Trigger**: `import sttp.tapir.generic.auto._` (or extending `SchemaDerivation` trait)

**Mechanism**: Provides a low-priority implicit `schemaForCaseClass[T]` that returns `Derived[Schema[T]]`:
```scala
// Scala 3
trait SchemaDerivation extends SchemaMagnoliaDerivation:
  inline implicit def schemaForCaseClass[T](implicit m: Mirror.Of[T], cfg: Configuration): Derived[Schema[T]] =
    Derived(derived[T])
```

**Derived wrapper**: `case class Derived[T](value: T) extends AnyVal` (zero-cost wrapper in `sttp.tapir.generic`)

**Priority**: User-provided `implicit Schema[T]` always takes precedence over the auto-derived `Derived[Schema[T]]` due to implicit priority.

### Feature: Semi-Automatic Derivation

**Trigger**: `Schema.derived[T]`

**Behavior**:
- Non-recursive: only derives the specified type T, not its children
- Children must have implicit schemas already in scope
- Easier to debug derivation failures

### Feature: Product Schema Derivation (Case Classes)

The Magnolia-based derivation creates `SProduct[T]` for case classes:

```scala
def productSchemaType[T](ctx: CaseClass[Schema, T]): SProduct[T] =
  SProduct(
    ctx.params.map { p =>
      val annotations = mergeAnnotations(p.annotations, p.inheritedAnnotations)
      val pSchema = enrichSchema(p.typeclass, annotations)
      val encodedName = getEncodedName(annotations)
        .getOrElse(genericDerivationConfig.toEncodedName(p.label))
      SProductField[T, p.PType](
        FieldName(p.label, encodedName), pSchema, t => Some(p.deref(t)))
    }.toList
  )
```

**Test cases**:
- Simple case class with primitive fields
- Nested case classes (B containing A)
- Case classes with collections (List, Option)
- Case classes with Map fields

### Feature: Coproduct Schema Derivation (Sealed Traits)

Creates `SCoproduct[T]` for sealed traits/abstract classes:

1. Collects subtype schemas into `subtypes: List[Schema[?]]`
2. Creates `subtypeSchema` function that maps runtime value to its schema
3. Optionally adds discriminator field based on `Configuration`

**Test cases**:
- Entity (Person | Organization | UnknownEntity case object)
- Pet (Cat | Dog | Hamster) with nested hierarchy (Rodent -> Hamster)
- Discriminator with various naming conventions

### Feature: Value Class Handling

When `ctx.isValueClass` is true, derivation unwraps to the inner type's schema:

```scala
if (ctx.isValueClass) {
  require(ctx.params.nonEmpty, s"Cannot derive schema for generic value class: ...")
  val valueSchema = ctx.params.head.typeclass
  Schema[T](schemaType = valueSchema.schemaType.as[T], format = valueSchema.format)
}
```

**Corner case**: Generic value classes fail with an error message.

### Feature: Discriminator via Configuration

When `Configuration.discriminator` is `Some("type")`:

1. Each child schema gets a string field named by the discriminator
2. The discriminator field value is determined by `Configuration.toDiscriminatorValue(childSName)`
3. `SDiscriminator` metadata is attached to the `SCoproduct`
4. Each child schema gets `encodedDiscriminatorValue` attribute

**Test cases**:
- Default short names: "Person", "Organization"
- Snake case: "person", "organization", "unknown_entity"
- Kebab case: "person", "organization", "unknown-entity"
- Screaming snake case: "PERSON", "ORGANIZATION", "UNKNOWN_ENTITY"
- Full qualified names: "sttp.tapir.tests.Person"
- Full + snake/kebab transformations

### Feature: oneOfUsingField (Macro-Based Discriminator)

```scala
Schema.oneOfUsingField[Entity, String](_.kind, _.toString)(
  "user" -> Schema[User],
  "org"  -> Schema[Organization]
)
```

- Uses existing trait method as discriminator extractor
- Builds `SCoproduct` with `SDiscriminator` and field mapping
- Adds discriminator field to child schemas automatically
- Works with both string and enum extractors

**Test cases**:
- String-based trait method discriminator
- Enum-based trait method discriminator
- 6 different lambda syntax variations produce identical results
- Generic type parameter extraction in WrapperT[String, Int, String]

### Feature: oneOfWrapped (Wrapper Object Discrimination)

```scala
Schema.oneOfWrapped[Entity]
```

- Wraps each sealed trait child in a single-field product
- Field name = subtype name (as configured)
- No explicit discriminator; discrimination by wrapper field name
- Uses `Schema.wrapWithSingleFieldProduct`

### Feature: Enumeration Schemas

**For sealed object families / Scala 3 parameterless enums:**
```scala
given Schema[MyEnum] = Schema.derivedEnumeration[MyEnum].defaultStringBased
```

Creates `SString` schema with `Validator.Enumeration` listing all possible values.

**For scala.Enumeration:**
```scala
// Default string-based
implicit val schema: Schema[Countries.Value] = Schema.derivedEnumerationValue[Countries.Value]

// Custom integer-based
Schema.derivedEnumerationValueCustomise[Countries.Value](
  encode = Some(_.id),
  schemaType = SchemaType.SInteger()
)
```

**Test cases**:
- Letters sealed trait (A, B, C case objects) -> SString + Validator.Enumeration
- Countries scala.Enumeration with @description, @default, @encodedName
- Custom integer encoding for Enumeration

---

## Recursive Type Handling

### Mechanism: ThreadLocal Cache with SRef

```scala
private[auto] val deriveCache: ThreadLocal[mutable.Set[String]] =
  new ThreadLocal[mutable.Set[String]]
```

**Algorithm:**
1. Before deriving type T, check if `fullName` is already in the cache
2. If found: return `Schema[T](SRef(SName(fullName, typeParams)))` (lazy reference)
3. If not found: add to cache, derive normally, remove from cache after derivation

**SRef semantics**: `SRef[T](name: SName)` is a placeholder that references another schema by name. Consumers (e.g., OpenAPI generators) resolve these references when building the final output.

### Test Cases for Recursive Types

| Type | Structure | Notes |
|------|-----------|-------|
| `F` | `f1: List[F], f2: Int` | Direct recursive list |
| `IOpt` | `i1: Option[IOpt], i2: Int` | Recursive through Option |
| `IList` | `i1: List[IList], i2: Int` | Recursive through List |
| `JOpt` | `data: Option[IOpt]` | Indirect recursive through wrapper |
| `JList` | `data: List[IList]` | Indirect recursive through wrapper |
| `Node` (sealed) | `Edge(id, source: Node)`, `SimpleNode(id)` | Recursive sealed trait |
| `RecursiveName` | `name: String, subNames: Option[Vector[RecursiveName]]` | Tree structure |

### Known Issues

1. **Scala 3 `given` keyword deadlock** (Issue #2749): Using `given` for recursive type schemas causes runtime deadlock. Must use `implicit def` instead:
   ```scala
   // WRONG - causes deadlock
   given Schema[RecursiveType] = Schema.derived

   // CORRECT
   implicit def recSchema: Schema[RecursiveType] = Schema.derived
   ```

2. **Scala 2 `implicit val` issue** (Issue #945): Recursive ADT schemas cannot be `implicit val` for auto-derivation. Must use `implicit lazy val` or `implicit def`.

---

## Scala 3 Specific Features

### Union Types

```scala
Schema.derivedUnion[String | Int]
```

- Decomposes `OrType` recursively into component types
- Generates `SCoproduct` with component schemas as subtypes
- Name format: `"String_or_Int"` (using `_or_` separator)
- Runtime `subtypeSchema` uses `isInstanceOf` for dispatch

**Test cases**:
- `String | Int` basic union
- Named type alias `type StringOrInt = String | Int`
- `List[String] | List[Int]` (same constructor, type erasure - no runtime dispatch)
- `List[String] | Vector[Int]` (different constructors, runtime dispatch works)
- `String | Int | Boolean` (3-component union)

### String-Based Union Enumeration

```scala
Schema.derivedStringBasedUnionEnumeration["a" | "b"]
```

- For literal type unions
- Produces `SString` with `Validator.Enumeration` containing the literal values
- Works with nested unions: `type AorB = "a" | "b"`, then `AorB | "c"`

### Scala 3 Enums

```scala
enum Fruit:
  case Apple, Banana

given Schema[Fruit] = Schema.derivedEnumeration.defaultStringBased
```

- Parameterless enum cases: treated as enumeration, get `SString` + `Validator.Enumeration`
- Enum cases with parameters: treated as sealed hierarchy, get `SCoproduct`
- `Schema.oneOfWrapped[Fruit]` also works for enum with payload

### Generic Classes with Type Parameters

```scala
// Problem: semi-auto derivation loses type parameter in name
Schema.derived[Paginated[String]]
// Produces SName("Paginated") without type parameter info

// Solution 1: derivedWithTypeParameter
inline given [T: Schema]: Schema[Paginated[T]] =
  Schema.derivedWithTypeParameter[Paginated, T]

// Solution 2: renameWithTypeParameter
Schema.derived[Paginated[String]].renameWithTypeParameter[String]
```

**Test case**: `Paginated[Values[String]]` should have SName with both `Values` and `String` type parameters.

---

## Schema Modification API

### modify (Macro-Based Path Access)

```scala
// Requires Derived[Schema[T]] in scope
implicitly[Derived[Schema[DevTeam]]]
  .value.modify(_.p1.age)(_.description("test").default(34))
```

**Path elements**:
- Field access: `_.field1.field2`
- Collection traversal: `_.field.each` (for List, Vector, Set, Array, Option)
- Either traversal: `_.field.eachLeft` / `_.field.eachRight`

### modifyUnsafe (String-Based Path Access)

```scala
schema.modifyUnsafe[String]("field1", "field2")(_.description("test"))
```

- Uses string field names instead of macro paths
- `Schema.ModifyCollectionElements` special string for traversing collection elements
- Works at any nesting depth

### How Modification Works Internally

`modifyAtPath` recurses through:
1. **SProduct**: finds matching field by name, modifies its schema recursively
2. **SArray**: if path is `ModifyCollectionElements`, modifies element schema
3. **SOption**: if path is `ModifyCollectionElements`, modifies element schema
4. **SOpenProduct**: if path is `ModifyCollectionElements`, modifies value schema; otherwise modifies named field
5. Continues recursing until path is empty, then applies the modification function

### Test Cases for Modification

| Test | Path | Target |
|------|------|--------|
| Modify basic string schema | `modifyUnsafe()` | Root schema |
| Modify product field | `"f2"` | Field within product |
| Modify nested product field | `"f4", "f2"` | Nested field |
| Modify array elements | `"f5", ModifyCollectionElements` | Array element schema |
| Modify array itself | `"f5"` | Array schema |
| Modify optional field property | `"f4", "f2"` where f4 is Option | Through option |
| Modify map value | `"v", ModifyCollectionElements` | SOpenProduct value |
| Modify open product | `"v"` | SOpenProduct itself |
| Chained modifications | Multiple modify calls | Both p1 and p2 formats |

---

## Validators

### Validator Hierarchy

```scala
sealed trait Validator[T] {
  def apply(t: T): List[ValidationError[?]]
  def contramap[TT](g: TT => T): Validator[TT]
  def and(other: Validator[T]): Validator[T]  // All
  def or(other: Validator[T]): Validator[T]   // Any
  def show: Option[String]
}
```

### Primitive Validators

| Validator | Parameters | Description |
|-----------|-----------|-------------|
| `Min[T: Numeric]` | `value: T, exclusive: Boolean` | Minimum value constraint |
| `Max[T: Numeric]` | `value: T, exclusive: Boolean` | Maximum value constraint |
| `Pattern[T <: String]` | `value: String` | Regex pattern matching |
| `MinLength[T <: String]` | `value: Int, countCodePoints: Boolean` | Minimum string length |
| `MaxLength[T <: String]` | `value: Int, countCodePoints: Boolean` | Maximum string length |
| `MinSize[T, C[_] <: Iterable[_]]` | `value: Int` | Minimum collection size |
| `MaxSize[T, C[_] <: Iterable[_]]` | `value: Int` | Maximum collection size |
| `Custom[T]` | `validationLogic: T => ValidationResult, showMessage: Option[String]` | Custom logic |
| `Enumeration[T]` | `possibleValues: List[T], encode: Option[EncodeToRaw[T]], name: Option[SName]` | Enum constraint |

### Composite Validators

| Validator | Parameters | Behavior |
|-----------|-----------|----------|
| `All[T]` | `validators: Seq[Validator[T]]` | All must pass (AND) |
| `Any[T]` | `validators: Seq[Validator[T]]` | At least one must pass (OR) |
| `Mapped[TT, T]` | `wrapped: Validator[T], g: TT => T` | Contravariant mapping |

### Factory Methods

```scala
Validator.pass[T]                         // always valid
Validator.reject[T]                       // always invalid
Validator.min[T: Numeric](value, exclusive = false)
Validator.max[T: Numeric](value, exclusive = false)
Validator.positive[T: Numeric]
Validator.positiveOrZero[T: Numeric]
Validator.negative[T: Numeric]
Validator.inRange[T: Numeric](min, max, minExclusive, maxExclusive)
Validator.pattern[T <: String](value)
Validator.minLength[T <: String](value, countCodePoints = false)
Validator.maxLength[T <: String](value, countCodePoints = false)
Validator.fixedLength[T <: String](value, countCodePoints = false)
Validator.nonEmptyString[T <: String]
Validator.minSize[T, C[_]](value)
Validator.maxSize[T, C[_]](value)
Validator.fixedSize[T, C[_]](value)
Validator.nonEmpty[T, C[_]]
Validator.enumeration[T](possibleValues, encode?, name?)
Validator.custom[T](validationLogic, showMessage?)
Validator.all[T](validators*)
Validator.any[T](validators*)
```

### Validator Application (Schema.applyValidation)

- **Product**: Applies field validators by extracting field values via `get`
- **Coproduct**: Finds matching subtype, applies its validators
- **Option**: `None` passes; `Some(v)` validates `v`
- **Collection/Array**: Each element validated individually; `Validator.pass` is optimized (skips iteration)
- **OpenProduct (Map)**: Each value validated
- **Recursive**: Validators propagate through `SRef` resolution
- **Either**: Left and right branches validated independently

### Test Cases for Validation

| Test | Scenario | Key Assertion |
|------|----------|---------------|
| validate product | Person with pattern + min | Multiple errors accumulate |
| validate option | Option[Int] with min | None passes, Some validated |
| validate iterable | List[Int] with min | Empty passes, elements checked |
| validate array | Array[Int] with min | Same as iterable |
| validate openProduct | Map[String, Int] with min | Values validated |
| validate recursive | RecursiveName with nested validators | Propagates through nesting |
| validate either | Either[Int, String] | Branches validated independently |
| validate oneOf | Discriminated sealed trait | Custom validators on members |
| skip pass validator | 1M element Array with Validator.pass | Completes in <1s (optimized) |

---

## Attribute System

```scala
case class AttributeKey[T](typeName: String)
case class AttributeMap(private val storage: Map[String, Any]) {
  def get[T](k: AttributeKey[T]): Option[T]
  def put[T](k: AttributeKey[T], v: T): AttributeMap
}
```

### Built-in Attributes

| Attribute | Key | Type | Description |
|-----------|-----|------|-------------|
| `Title` | `Schema.Title.Attribute` | `Title(value: String)` | JSON Schema title |
| `EncodedDiscriminatorValue` | `Schema.EncodedDiscriminatorValue.Attribute` | `EncodedDiscriminatorValue(v: String)` | Discriminator value for coproduct children |

---

## Edge Cases Tested

### 1. Recursive via Option
Type `IOpt(i1: Option[IOpt], i2: Int)` - recursive reference wrapped in Option produces `SRef`.

### 2. Recursive via List
Type `IList(i1: List[IList], i2: Int)` - recursive reference in collection produces `SRef`.

### 3. Recursive Sealed Trait
Type `Node` (sealed) with `Edge(id, source: Node)` - sealed hierarchy with recursive reference.

### 4. Nested Sealed Hierarchy
`Pet` -> `Rodent` (sealed) -> `Hamster` - multi-level sealed trait hierarchy.

### 5. Case Object in Sealed Trait
`UnknownEntity` case object extends `Entity` - gets `SProduct` with no fields.

### 6. Generic Type Names for Either
`Either[Int, String]` - SName includes type parameters; primitives (Int, String) may be excluded from short names depending on context.

### 7. Map Schema Naming
`Map[String, D]` - SName is `"Map"` with type parameter `"D"`.
`Map[String, H[D]]` - SName includes nested type parameters.

### 8. Non-String Map Keys
`Map[K, V]` where K is not String - still produces `SOpenProduct` but with custom key serialization.

### 9. @encodedName Does Not Propagate to Subtypes
`@encodedName("Custom")` on sealed trait only renames the trait's SName, not the children's names.

### 10. @encodedName Overrides Configuration
`@encodedName("specialName")` on a field ignores `Configuration.toEncodedName`.

### 11. Annotation Inheritance
Annotations on sealed trait fields (like `@description` on common fields) propagate to all subtypes via `inheritedAnnotations`.

### 12. addDiscriminatorField Idempotency
If a child schema already has the discriminator field, it is not added again.

### 13. Validator.Enumeration with Custom Encoding
Enumeration schema can use `SInteger` instead of `SString` with custom `.id` encoder.

### 14. @validateEach on Collections and Options
`@validateEach(Validator.min(1))` on `List[Int]` validates each element.
`@validateEach(Validator.minLength(1))` on `Option[String]` validates the contained value.

### 15. Union Type with Type Erasure
`List[String] | List[Int]` - runtime dispatch fails due to erasure; `subtypeSchema` returns `None`.

### 16. Union of Literal Types
`"a" | "b" | "c"` - flattened into single `Validator.Enumeration`.

### 17. Format Propagation in Optional Schemas
`Schema[Option[Double]]` inherits `"double"` format from wrapped `Schema[Double]`.

### 18. Generic Value Class Rejection
`isValueClass` with empty `params` (generic value class) throws `require` failure.

---

## Known Issues / Workarounds

### 1. Recursive Type Deadlock on Scala 3 (Issue #2749)
**Problem**: Using `given Schema[Rec] = Schema.derived` causes runtime deadlock.
**Workaround**: Use `implicit def recSchema: Schema[Rec] = Schema.derived[Rec]` instead.

### 2. Recursive ADT Auto-Derivation (Issue #945)
**Problem**: `implicit val` for recursive ADTs cannot be resolved during auto-derivation.
**Workaround**: Use `implicit lazy val` or `implicit def`.

### 3. Generic Class Schema Names (Issue #4549)
**Problem**: Semi-auto `Schema.derived[Generic[String]]` produces SName without type parameter info (just `"Generic"` instead of `"Generic<String>"`).
**Workaround**: Use `Schema.derivedWithTypeParameter[Generic, String]` or `inline given` with `Schema.derived`.

### 4. Parametrized Data Schema Derivation (Issue #1303)
**Problem**: Abstract type parameters in schemas cause issues when schemas are keyed by name.
**Workaround**: Provide explicit schemas for generic instantiations.

### 5. Opaque Types (Issue #1858)
**Problem**: No automatic codec/schema derivation for Scala 3 opaque types (no common bound beyond Any, no standard accessor).
**Workaround**: Define manual schemas.

### 6. Value Class Format Missing (Issue #1069)
**Problem**: Derived schema for value class wrapping Int may lose `"int32"` format.
**Workaround**: The Magnolia derivation now copies `format` from inner type.

### 7. Hidden Annotation on Coproduct Subtypes (Issue #3382)
**Problem**: `@hidden` annotation not propagated to schema in Scala 3 for some cases.
**Workaround**: Apply `.hidden(true)` manually.

---

## Derivation Infrastructure

### Magnolia Integration

Both Scala 2 and 3 use Magnolia for generic programming:
- `join[T](ctx: CaseClass[Schema, T])` for products
- `split[T](ctx: SealedTrait[Schema, T])` for coproducts
- `TypeInfo` provides fully qualified name and type parameters

### Derived Wrapper

```scala
// In sttp.tapir.generic
case class Derived[T](value: T) extends AnyVal
```

Used to give auto-derived schemas lower implicit priority than user-provided schemas. The `SchemaDerivation` trait produces `Derived[Schema[T]]`, and an implicit conversion `Derived[Schema[T]] => Schema[T]` bridges the gap.

### ThreadLocal Cache for Recursion

```scala
private[auto] val deriveCache: ThreadLocal[mutable.Set[String]] =
  new ThreadLocal[mutable.Set[String]] {
    override def initialValue(): mutable.Set[String] = mutable.Set.empty
  }
```

Prevents infinite recursion during derivation by tracking in-progress type names and returning `SRef` for cycles.

---

## Source File Inventory

### Core Types (platform-independent)
| File | Key Types |
|------|-----------|
| `core/src/main/scala/sttp/tapir/Schema.scala` | `Schema[T]`, `SName`, `Title`, `EncodedDiscriminatorValue`, annotations |
| `core/src/main/scala/sttp/tapir/SchemaType.scala` | `SchemaType`, `SProduct`, `SCoproduct`, `SArray`, `SOption`, `SOpenProduct`, `SRef`, `SProductField`, `SDiscriminator`, `SchemaWithValue` |
| `core/src/main/scala/sttp/tapir/SchemaAnnotations.scala` | `SchemaAnnotations[T]` |
| `core/src/main/scala/sttp/tapir/FieldName.scala` | `FieldName` |
| `core/src/main/scala/sttp/tapir/Validator.scala` | `Validator` hierarchy |
| `core/src/main/scala/sttp/tapir/generic/Configuration.scala` | `Configuration` |
| `core/src/main/scala/sttp/tapir/macros/CreateDerivedEnumerationSchema.scala` | Enumeration schema builder |

### Scala 3 Derivation
| File | Purpose |
|------|---------|
| `core/src/main/scala-3/sttp/tapir/generic/auto/SchemaMagnoliaDerivation.scala` | Product/Coproduct derivation via Magnolia |
| `core/src/main/scala-3/sttp/tapir/generic/auto/SchemaDerivation.scala` | Auto-derivation trait |
| `core/src/main/scala-3/sttp/tapir/macros/SchemaMacros.scala` | `oneOfUsingField`, `oneOfWrapped`, `modify`, `derivedUnion`, `derivedWithTypeParameter` |
| `core/src/main/scala-3/sttp/tapir/macros/union_derivation.scala` | Union type decomposition |
| `core/src/main/scala-3/sttp/tapir/internal/SchemaAnnotationsMacro.scala` | Compile-time annotation extraction |
| `core/src/main/scala-3/sttp/tapir/internal/CaseClass.scala` | Case class field extraction |
| `core/src/main/scala-3/sttp/tapir/internal/SNameMacros.scala` | Type name/parameter extraction |

### Scala 2 Derivation
| File | Purpose |
|------|---------|
| `core/src/main/scala-2/sttp/tapir/generic/auto/SchemaMagnoliaDerivation.scala` | Product/Coproduct derivation via Magnolia |
| `core/src/main/scala-2/sttp/tapir/generic/auto/SchemaDerivation.scala` | Auto-derivation trait |
| `core/src/main/scala-2/sttp/tapir/internal/SchemaAnnotationsMacro.scala` | Compile-time annotation extraction |
| `core/src/main/scala-2/sttp/tapir/internal/OneOfMacro.scala` | `oneOfUsingField`, `oneOfWrapped` |

### Test Files
| File | Focus |
|------|-------|
| `core/src/test/scala/sttp/tapir/SchemaTest.scala` | Schema modification, naming, discriminator field generation |
| `core/src/test/scala/sttp/tapir/SchemaMacroTest.scala` | Macro-based derivation, annotations, discriminators, enums |
| `core/src/test/scala/sttp/tapir/SchemaMacroTestData.scala` | Test fixture types (Person, Entity, Pet, etc.) |
| `core/src/test/scala/sttp/tapir/SchemaAnnotationsTest.scala` | SchemaAnnotations.enrich() |
| `core/src/test/scala/sttp/tapir/SchemaAnnotationsTestData.scala` | MyString annotated fixture |
| `core/src/test/scala/sttp/tapir/SchemaApplyValidationTest.scala` | Validator application |
| `core/src/test/scala/sttp/tapir/SchemaApplyValidationTestData.scala` | Validation fixture types |
| `core/src/test/scala/sttp/tapir/generic/SchemaGenericAutoTest.scala` | Auto-derivation end-to-end tests |
| `core/src/test/scala-3/sttp/tapir/SchemaMacroScala3Test.scala` | Scala 3 enums, unions, generic derivation |
