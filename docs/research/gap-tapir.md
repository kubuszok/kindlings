# Tapir Schema Gap Analysis: Upstream vs Kindlings

## Summary

### Annotations supported: 9/11
- **Implemented**: `@description`, `@title`, `@format`, `@deprecated`, `@hidden`, `@encodedName`, `@validate`, `@validateEach`, `@customise`
- **Not implemented (compile-time)**: `@default`, `@encodedExample`
- **Note**: All 11 annotations are handled at runtime via `TapirSchemaUtils.enrichSchema` (they work on fields/types that have annotations). The gap is that `@default` and `@encodedExample` are not tested. They should work at runtime since `enrichSchema` handles them, but there are no test cases verifying this.

### Schema types supported: 8/12
- **Implemented**: `SProduct`, `SCoproduct`, `SString` (enum), `SArray`, `SOption`, `SOpenProduct`, `SRef`, primitives (via summoned implicits)
- **Not directly produced by derivation**: `SInteger`, `SNumber`, `SBoolean`, `SBinary`, `SDate`, `SDateTime` -- these come from Tapir's built-in `Schema` implicits which Kindlings summons rather than derives

### Derivation features: 5/12 upstream features
- **Implemented**: Product (case class), Coproduct (sealed trait), Singleton (case object), Recursive types, Generic type parameter names
- **Not implemented**: Value class unwrapping, `oneOfUsingField`, `oneOfWrapped`, `derivedEnumeration`/`derivedEnumerationValue`, `derivedUnion` (Scala 3 unions), `derivedStringBasedUnionEnumeration`, `derivedWithTypeParameter`, `Schema.modify` macro, `Schema.modifyUnsafe` (on derived), `Configuration` (uses JSON library configs instead)

### Test coverage: 34 test cases total in Kindlings vs ~50+ upstream test scenarios

---

## Annotation-by-Annotation Comparison

### @description
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On type | Sets `schema.description` | Tested: `AnnotatedPerson` | **Implemented + tested** |
| On field | Sets field schema description | Tested: `AnnotatedPerson.name` | **Implemented + tested** |
| Inherited from sealed trait fields | Propagates via `inheritedAnnotations` | Not tested | **Gap: no inheritance test** |

- Upstream test: `SchemaMacroTestData.Person` with `@description` on type and fields
- Kindlings test: `KindlingsSchemaSpec` lines 121-143, `examples.scala` line 19

### @title
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On type | Sets `Title` attribute | Tested: `AnnotatedPerson` | **Implemented + tested** |
| On field | Not typically used | Not tested | N/A |
| **Note** | Only in Magnolia path (not `SchemaAnnotations`) | Handled in `enrichSchema` runtime | **Works differently** |

- Upstream: Only handled in `enrichSchema` (Magnolia), NOT in `SchemaAnnotations.enrich()`
- Kindlings: Handled at runtime in `TapirSchemaUtils.enrichSchema` line 19
- Kindlings test: `KindlingsSchemaSpec` lines 127-131

### @format
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field | Sets field `schema.format` | Tested: `AnnotatedPerson.age` | **Implemented + tested** |
| On type | Sets type schema format | Not tested | **Gap: no type-level test** |

- Kindlings test: `KindlingsSchemaSpec` lines 145-155

### @deprecated
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On type | Sets `schema.deprecated = true` | Tested: `DeprecatedType` | **Implemented + tested** |
| On field | Not commonly used | Not tested | N/A |
| On coproduct subtypes (Issue #3382) | Known issue in Scala 3 | Not tested | **Gap: no subtype test** |

- Kindlings test: `KindlingsSchemaSpec` lines 169-172

### @hidden
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field | Sets field `schema.hidden = true` | Tested: `WithHiddenField.secret` | **Implemented + tested** |
| On type | Sets type schema hidden | Not tested | **Gap** |
| On coproduct subtypes | Known issue in Scala 3 upstream | Not tested | **Gap** |

- Kindlings test: `KindlingsSchemaSpec` lines 174-184
- Upstream issue: #3382 - `@hidden` on coproduct subtypes may not propagate in Scala 3

### @encodedName
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field | Overrides JSON config name | Tested: `WithEncodedName.userName` | **Implemented + tested** |
| On type | Overrides `SName.fullName`, clears type params | Not tested | **Gap: no type-level test** |
| Priority over Configuration | Annotation wins over config transform | Tested implicitly (field level) | **Partially tested** |
| Does NOT propagate to subtypes | Annotation on sealed trait only renames trait | Not tested | **Gap** |

- Implementation: `TapirSchemaUtils.productFieldWithAnnotations` lines 37-41
- Kindlings test: `KindlingsSchemaSpec` lines 157-167
- Upstream edge case 9 (`@encodedName` does not propagate to subtypes): **Not tested**
- Upstream edge case 10 (`@encodedName` overrides Configuration): **Not tested at type level**

### @default
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field | Sets `schema.default(value, encoded)` | Runtime handler exists in `enrichSchema` line 14 | **Implemented but NOT tested** |
| Test case | Upstream tests default annotation with encoded form | No test case | **Gap: no test** |

### @encodedExample
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field/type | Sets `schema.encodedExample` | Runtime handler exists in `enrichSchema` line 13 | **Implemented but NOT tested** |
| Test case | Upstream tests with example values | No test case | **Gap: no test** |

### @validate
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field | Adds validator to field schema | Tested: `WithValidation.age` | **Implemented + tested** |
| On type | Adds validator to type schema | Not tested | **Gap** |
| Multiple validators | Folded sequentially | Not tested | **Gap** |

- Kindlings test: `KindlingsSchemaSpec` lines 186-196

### @validateEach
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On collection field | Validates each element via `modifyUnsafe(ModifyCollectionElements)` | Runtime handler exists in `enrichSchema` lines 16-17 | **Implemented but NOT tested** |
| On Option field | Validates the contained value | Not tested | **Gap** |
| Test case | Upstream tests `@validateEach(Validator.min(1))` on `List[Int]` | No test case | **Gap: no test** |

### @customise
| Aspect | Upstream | Kindlings | Status |
|--------|----------|-----------|--------|
| On field/type | Applies arbitrary `Schema[?] => Schema[?]` function | Runtime handler exists in `enrichSchema` line 22 | **Implemented but NOT tested** |
| Test case | Only in Magnolia path upstream | No test case | **Gap: no test** |

---

## Schema Type Comparison

| Schema Type | Upstream | Kindlings | Status |
|-------------|----------|-----------|--------|
| `SString` | Strings, string-based enums | Used for `stringEnumSchema` (case object sealed traits with `enumAsStrings`) | **Implemented** |
| `SInteger` | Byte, Short, Int, Long | Summoned from Tapir built-in implicits, not derived | **Delegated to Tapir** |
| `SNumber` | Float, Double, BigDecimal | Summoned from Tapir built-in implicits | **Delegated to Tapir** |
| `SBoolean` | Boolean | Summoned from Tapir built-in implicits | **Delegated to Tapir** |
| `SBinary` | Array[Byte], InputStream, File | Summoned from Tapir built-in implicits | **Delegated to Tapir** |
| `SDate` | LocalDate | Summoned from Tapir built-in implicits | **Delegated to Tapir** |
| `SDateTime` | Instant, ZonedDateTime, etc. | Summoned from Tapir built-in implicits | **Delegated to Tapir** |
| `SProduct` | Case classes, tuples | Derived for case classes and singletons (case objects) | **Implemented** |
| `SCoproduct` | Sealed traits, enums | Derived for sealed traits and Scala 3 enums with payload cases | **Implemented** |
| `SArray` | List, Vector, Set, Array | Derived via `collectionSchema` and `mapAsArraySchema` | **Implemented** |
| `SOption` | Option types | Derived via `optionSchema` | **Implemented** |
| `SOpenProduct` | Map types | Derived via `mapSchema` (string keys only) | **Partially implemented** |
| `SRef` | Recursive types | Derived via `refSchema` when recursion detected | **Implemented** |

### Gaps in Schema Types

1. **Non-String Map keys**: Upstream supports `Map[K, V]` where K is not String (produces `SOpenProduct` with custom key serialization). Kindlings explicitly fails with error: "Cannot derive tapir Schema for Map with non-String key type" (`SchemaMacrosImpl.scala` line 353). **Gap: non-string map keys are rejected.**

2. **Tuple schemas**: Upstream derives `SProduct` for tuples. Kindlings does not test or explicitly handle tuples (they would need to be parseable as `CaseClass` by Hearth). **Gap: untested.**

---

## Derivation Feature Comparison

### Product Schema Derivation (Case Classes)

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Simple case class | Tested | Tested: `SimplePerson` | **Implemented + tested** |
| Nested case classes | Tested | Tested: `Nested` | **Implemented + tested** |
| Case class with Option fields | Tested | Tested: `WithOptional` | **Implemented + tested** |
| Case class with collection fields | Tested | Tested: `WithCollections` | **Implemented + tested** |
| Case class with Map fields | Tested | Tested: `WithMap` | **Implemented + tested** |
| Field name encoding | Via `Configuration.toEncodedName` | Via `JsonSchemaConfig.resolveFieldName` | **Implemented + tested** |
| SName with type parameters | Flattened recursively | Via `runtimePlainPrint` + `parseSName` | **Implemented + tested** |
| Value class unwrapping | Unwraps to inner type's schema + format | Not implemented | **Gap: not implemented** |
| Generic value class rejection | Throws `require` failure | Not applicable (no value class support) | **Gap** |
| Transient fields | N/A (not in upstream tapir) | Supported via `JsonSchemaConfig.isTransientField` | **Kindlings extension** |

#### Kindlings-only Product Features (not in upstream Tapir derivation)
- `fieldsWithDefaultsAreOptional`: marks fields with Scala defaults as `isOptional = true` (tested)
- `emptyFieldsAreOptional`: marks collection/map fields as optional (config-driven, not explicitly tested)
- `numericFieldsAsStrings`: adds `"string"` format to numeric fields (tested for jsoniter `isStringified`)
- `mapAsArray`: encodes maps as arrays of key-value objects (tested)
- `@transientField` support from circe/jsoniter annotations (tested)

### Coproduct Schema Derivation (Sealed Traits)

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Basic sealed trait | Tested (Entity, Pet) | Tested: `Shape` | **Implemented + tested** |
| Subtypes list | Verified subtype count | Tested: `Shape` has 2 subtypes | **Implemented + tested** |
| Case object in sealed trait | Gets `SProduct` with no fields | Derived via singleton path | **Implemented** (tested in `Color` enum tests) |
| Nested sealed hierarchy | Pet -> Rodent -> Hamster | Not tested | **Gap: no nested hierarchy test** |
| `subtypeSchema` runtime function | Maps value to schema via Magnolia dispatch | Maps via `getClass.getName` comparison | **Implemented differently** |
| Discriminator via Configuration | `Configuration.discriminator` | Via `JsonSchemaConfig.discriminatorFieldName` | **Implemented + tested** |
| Discriminator field added to children | Added to each child `SProduct` | NOT added to children, only to `SDiscriminator` mapping | **Partial gap: children don't get discriminator field** |
| `encodedDiscriminatorValue` attribute on children | Set via `addDiscriminatorField` | Not set | **Gap: no per-child discriminator attribute** |
| Discriminator naming transforms | snake_case, kebab-case, SCREAMING_SNAKE, full qualified, etc. | Via `JsonSchemaConfig.resolveConstructorName` | **Delegated to JSON config** |

#### Discriminator Implementation Differences

Upstream Tapir uses `SCoproduct.addDiscriminatorField` which:
1. Adds a string field to each child product schema
2. Sets `SDiscriminator(name, mapping)` on the coproduct
3. Sets `encodedDiscriminatorValue` attribute on each child

Kindlings' `TapirSchemaUtils.coproductSchema` (lines 79-107):
1. Creates `SDiscriminator(name, mapping)` on the coproduct -- **done**
2. Does NOT add discriminator field to child schemas -- **gap**
3. Does NOT set `encodedDiscriminatorValue` on children -- **gap**

This means OpenAPI generators consuming Kindlings schemas may miss discriminator information on child types.

### Tested Discriminator Naming Conventions

| Convention | Upstream Tested | Kindlings Tested | Status |
|-----------|----------------|------------------|--------|
| Default short names | Yes | Implicit via JSON config | **Delegated** |
| snake_case | Yes | Tested: `CirceDiscriminatorDerivation` with `_.toLowerCase` | **Partial** |
| kebab-case | Yes | Not tested | **Gap** |
| SCREAMING_SNAKE_CASE | Yes | Not tested | **Gap** |
| Full qualified names | Yes | Not tested | **Gap** |
| Full + snake/kebab | Yes | Not tested | **Gap** |

### Value Class Handling

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Value class unwrapping | Unwraps to inner type schema | **Not implemented** | **Gap** |
| Format propagation | Copies format from inner type | N/A | **Gap** |
| Generic value class rejection | Throws require failure | N/A | **Gap** |

### oneOfUsingField (Macro-Based Discriminator)

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| String-based field discriminator | Fully implemented + tested | **Not implemented** | **Gap** |
| Enum-based field discriminator | Implemented + tested | **Not implemented** | **Gap** |
| Lambda syntax variations | 6 variations tested | **Not implemented** | **Gap** |
| Generic type parameter extraction | Tested | **Not implemented** | **Gap** |

### oneOfWrapped (Wrapper Object Discrimination)

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Single-field wrapper wrapping | Implemented + tested | **Not implemented** | **Gap** |

### Enumeration Schemas

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Sealed object families as SString | `derivedEnumeration.defaultStringBased` | `stringEnumSchema` when `enumAsStrings=true` | **Partially equivalent** |
| `derivedEnumeration` builder API | Full builder with encode/name options | **Not implemented** | **Gap** |
| `derivedEnumerationValue` (scala.Enumeration) | Implemented | **Not implemented** | **Gap** |
| `derivedEnumerationValueCustomise` | Custom integer encoding | **Not implemented** | **Gap** |
| Validator.Enumeration on enum schemas | Lists all possible values | Implemented in `stringEnumSchema` line 119 | **Implemented** |
| Custom enum encoding (SInteger) | Supported | **Not implemented** | **Gap** |

Kindlings' enum handling:
- When `enumAsStrings = true` (from JSON config), all-singleton sealed traits produce `SString` + `Validator.Enumeration`
- When `enumAsStrings = false`, they produce `SCoproduct` with child product schemas
- There is no standalone `derivedEnumeration` API -- it's config-driven

### Recursive Type Handling

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Recursion detection | `ThreadLocal` cache with fullName set | `MLocal[Set[String]]` with `cacheKey` | **Implemented** |
| SRef emission | Returns `SRef(SName)` for cycles | Returns `refSchema` (SRef) | **Implemented** |
| Direct recursive list | `F(f1: List[F])` | Tested: `RecursiveTree` | **Implemented + tested** |
| Recursive through Option | `IOpt(i1: Option[IOpt])` | Not tested | **Gap: no Option recursion test** |
| Indirect recursive through wrapper | `JOpt(data: Option[IOpt])` | Not tested | **Gap** |
| Recursive sealed trait | `Node -> Edge(source: Node)` | Not tested | **Gap: no recursive sealed trait test** |
| Tree structure | `RecursiveName(subNames: Option[Vector[RecursiveName]])` | Not tested | **Gap** |

Kindlings test: `KindlingsSchemaSpec` lines 234-243 (only tests `RecursiveTree` field names, does not verify SRef is actually used in the children list)

### Generic Type Parameter Resolution

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Parameterized type SName | Includes type parameter short names | Implemented via `runtimePlainPrint` | **Implemented + tested** |
| Multi-param types | Both type params in SName | Tested: `Pair[SimplePerson, Nested]` | **Implemented + tested** |
| Abstract type parameter at call site | Needs `derivedWithTypeParameter` | Handled via `runtimePlainPrint` (runtime resolution) | **Implemented differently + tested** |
| `derivedWithTypeParameter` API | Explicit macro for named type params | **Not implemented** (not needed due to `runtimePlainPrint`) | **Different approach** |
| Nested type params flattened | `Wrapper[Map[String, Int]]` -> `Map`, `String`, `Int` | Not tested | **Gap: no nested param test** |

Kindlings tests: `KindlingsSchemaSpec` lines 48-94 and `GenericDerivation` object

---

## Configuration Comparison

### Upstream Tapir Configuration vs Kindlings Approach

Upstream Tapir has its own `Configuration` class:
```scala
case class Configuration(
  toEncodedName: String => String,
  discriminator: Option[String],
  toDiscriminatorValue: SName => String
)
```

Kindlings does NOT use Tapir's `Configuration`. Instead, it reads configuration from the JSON library in implicit scope (circe `Configuration` or jsoniter `JsoniterConfig`) via the `JsonSchemaConfig` abstraction. This is a **deliberate design difference** -- the schema matches the JSON encoding.

| Tapir Configuration Feature | Kindlings Equivalent | Status |
|-----------------------------|---------------------|--------|
| `toEncodedName` | `JsonSchemaConfig.resolveFieldName` | **Equivalent** |
| `discriminator` | `JsonSchemaConfig.discriminatorFieldName` | **Equivalent** |
| `toDiscriminatorValue` | `JsonSchemaConfig.resolveConstructorName` | **Equivalent** |
| `withSnakeCaseMemberNames` | Via circe `.withSnakeCaseMemberNames` or jsoniter `.withSnakeCaseFieldNames` | **Delegated** |
| `withKebabCaseMemberNames` | Via circe `.withKebabCaseMemberNames` or jsoniter equivalent | **Delegated** |
| `withScreamingSnakeCaseMemberNames` | Via circe `.withScreamingSnakeCaseMemberNames` or equivalent | **Delegated** |
| `withSnakeCaseDiscriminatorValues` | Via JSON config constructor name transform | **Delegated** |
| `withKebabCaseDiscriminatorValues` | Via JSON config | **Delegated** |
| `withScreamingSnakeCaseDiscriminatorValues` | Via JSON config | **Delegated** |
| `withFullDiscriminatorValues` | Via JSON config | **Delegated** |

---

## Validator Comparison

Validators are a Tapir core feature, not part of derivation. Kindlings handles validators via:

1. **`@validate` annotation**: Applied at runtime via `enrichSchema` -- **Implemented + tested**
2. **`@validateEach` annotation**: Applied at runtime via `enrichSchema` + `modifyUnsafe(ModifyCollectionElements)` -- **Implemented, NOT tested**
3. **`Validator.Enumeration` on string enums**: Created in `stringEnumSchema` -- **Implemented + tested**
4. **Discriminator field validator**: NOT created (upstream sets single-value `Validator.Enumeration` on discriminator field) -- **Gap**

| Validator Feature | Upstream Derivation | Kindlings | Status |
|-------------------|-------------------|-----------|--------|
| `@validate` on field | Applied during derivation | Applied at runtime via `enrichSchema` | **Implemented** |
| `@validateEach` on collection field | Applied via `modifyUnsafe` | Applied via `enrichSchema` | **Implemented but untested** |
| `Validator.Enumeration` on enum schemas | Created for both sealed objects and `scala.Enumeration` | Created for sealed object enum schemas | **Partial** |
| Discriminator field `Validator.Enumeration` | Single-value enum validator on disc field | Not implemented (disc field not added to children) | **Gap** |
| Validator propagation through recursive types | Tested | Not tested | **Gap** |

---

## Scala 3 Feature Comparison

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| Union types (`String \| Int`) | `Schema.derivedUnion` produces `SCoproduct` | **Not implemented** | **Gap** |
| String-based union enumeration (`"a" \| "b"`) | `derivedStringBasedUnionEnumeration` | **Not implemented** | **Gap** |
| Scala 3 enums (parameterless) | Treated as enumeration | Handled as all-singleton sealed trait | **Implemented** (assuming Hearth `Enum.parse` works for Scala 3 enums) |
| Scala 3 enums (with parameters) | Treated as sealed hierarchy | Handled as sealed trait with non-singleton children | **Implemented** (assuming Hearth works) |
| `inline given` derivation | `inline given [T: Schema]: Schema[Paginated[T]] = Schema.derived` | `inline given derived[A]: KindlingsSchema[A]` (Scala 3) | **Different API** |

### Scala 3 Union Type Test Cases Missing

All upstream union type tests are missing from Kindlings:
- `String | Int` basic union
- Named type alias union
- `List[String] | List[Int]` (erasure issue)
- `List[String] | Vector[Int]` (works)
- `String | Int | Boolean` (3-component)
- Literal type unions (`"a" | "b"`)

---

## Schema Modification API Comparison

| Feature | Upstream | Kindlings | Status |
|---------|----------|-----------|--------|
| `Schema.modify(_.field)` macro | Implemented for paths, collections, either | **Not implemented** | **Gap** |
| `Schema.modifyUnsafe("field")` | Works on derived schemas | Available via Tapir core (not derivation-specific) | **N/A (core Tapir feature)** |
| `.each` collection traversal | Macro support | **Not implemented** | **Gap** |
| `.eachLeft`/`.eachRight` traversal | Macro support | **Not implemented** | **Gap** |

---

## Missing Test Cases

### Tests present in upstream but missing in Kindlings

#### Case Class Derivation
1. **Value class schema** -- derivation should unwrap to inner type
2. **Case class with nested Map** -- `Map[String, H[D]]` SName with nested type params
3. **Case class with Either field** -- `Either[A, B]` schema derivation
4. **Tuple derivation** -- `(A, B)` as SProduct

#### Sealed Trait Derivation
5. **Nested sealed hierarchy** -- e.g., `Pet -> Rodent -> Hamster` multi-level
6. **Case object in sealed trait produces empty SProduct** -- verify SProduct with 0 fields
7. **Recursive sealed trait** -- `Node -> Edge(source: Node)` with SRef
8. **Discriminator with addDiscriminatorField behavior** -- discriminator field added to children
9. **Multiple discriminator naming conventions** -- kebab, screaming snake, full qualified, etc.

#### Annotations
10. **`@default` on field** -- verify schema.default is set
11. **`@encodedExample` on field/type** -- verify schema.encodedExample is set
12. **`@encodedName` on type** -- verify SName.fullName is overridden, type params cleared
13. **`@encodedName` does NOT propagate to subtypes** -- edge case #9
14. **`@validateEach` on collection field** -- verify element validation
15. **`@validateEach` on Option field** -- validate contained value
16. **`@customise` on field/type** -- verify arbitrary transformation
17. **`@hidden` on type** -- verify schema.hidden at type level
18. **Multiple `@validate` annotations** -- verify they compose (AND)
19. **Annotation inheritance in sealed traits** -- annotations on trait fields propagate to subtypes

#### Recursive Types
20. **Recursive through Option** -- `IOpt(Option[IOpt])`
21. **Indirect recursive through wrapper** -- `JOpt(data: Option[IOpt])`
22. **Tree structure** -- `RecursiveName(subNames: Option[Vector[RecursiveName]])`
23. **Verify SRef is actually emitted** -- current test only checks field names, not schema type of recursive field

#### Enumeration
24. **`derivedEnumeration.defaultStringBased`** -- standalone API (not config-driven)
25. **`scala.Enumeration` value derivation** -- `derivedEnumerationValue`
26. **Custom integer-encoded enumeration** -- `SInteger` instead of `SString`
27. **Enum with @description, @default, @encodedName** -- upstream tests this on `Countries`

#### Schema Naming
28. **Nested type parameter flattening** -- `Wrapper[Map[String, Int]]` -> params include `Map`, `String`, `Int`
29. **Primitive types excluded from SName short names** -- edge case for `Either[Int, String]`
30. **Map schema naming** -- `Map[String, D]` SName is `"Map"` with type param `"D"`

#### Scala 3 Specific
31. **Union type derivation** -- `String | Int`
32. **Literal type union enumeration** -- `"a" | "b"`
33. **Scala 3 enum with parameters** -- treated as sealed hierarchy
34. **`given` vs `implicit def` for recursive types** -- deadlock issue #2749

---

## Missing Corner Cases

### From upstream edge cases (numbered per upstream research doc)

| # | Edge Case | Upstream Tests | Kindlings Tests | Status |
|---|-----------|---------------|-----------------|--------|
| 1 | Recursive via Option | `IOpt` type | None | **Missing** |
| 2 | Recursive via List | `IList` type | `RecursiveTree` (partial) | **Partial -- does not verify SRef** |
| 3 | Recursive sealed trait | `Node`/`Edge` | None | **Missing** |
| 4 | Nested sealed hierarchy | `Pet`/`Rodent`/`Hamster` | None | **Missing** |
| 5 | Case object in sealed trait | `UnknownEntity` | `Color` (Red/Green/Blue as enum, not as coproduct member) | **Partial** |
| 6 | Generic type names for Either | `Either[Int, String]` SName | None | **Missing** |
| 7 | Map schema naming | `Map[String, D]` SName | None | **Missing** |
| 8 | Non-String map keys | `Map[K, V]` where K != String | Explicitly rejected with error | **Intentional gap** |
| 9 | @encodedName does NOT propagate to subtypes | Verified in upstream | None | **Missing** |
| 10 | @encodedName overrides Configuration | Priority tested | Not at type level | **Missing** |
| 11 | Annotation inheritance | Sealed trait field annotations propagate | None | **Missing** |
| 12 | addDiscriminatorField idempotency | Child already has disc field | Not applicable (disc field not added to children) | **Missing (different architecture)** |
| 13 | Validator.Enumeration with custom encoding | `SInteger` enum | None | **Missing** |
| 14 | @validateEach on collections/options | `List[Int]`, `Option[String]` | None | **Missing** |
| 15 | Union type with type erasure | `List[String] \| List[Int]` returns None | Not applicable (no union support) | **Missing** |
| 16 | Union of literal types | `"a" \| "b" \| "c"` | Not applicable | **Missing** |
| 17 | Format propagation in optional schemas | `Schema[Option[Double]]` inherits `"double"` | None | **Missing** |
| 18 | Generic value class rejection | isValueClass with empty params | None | **Missing** |

### Kindlings-Specific Edge Cases Not Tested

1. **PreferSchemaConfig with no configs**: Error message when no JSON config exists (tested implicitly by requiring config, but no negative test)
2. **PreferSchemaConfig with >2 configs**: Multiple configs without preference (no test)
3. **Empty sealed trait**: Sealed trait with no subtypes (no test)
4. **Deeply nested generics**: `Box[Box[Box[Int]]]` SName resolution (no test)
5. **Recursive type with Map field**: `case class R(m: Map[String, R])` (no test)
6. **Mutual recursion**: `case class A(b: B); case class B(a: A)` (no test)
7. **SRef resolution by consumers**: Verify that SRef names match the emitted SProduct/SCoproduct names so consumers can resolve them (no test)

---

## Implementation Architecture Differences

### Upstream: Magnolia-based derivation
- Uses Magnolia's `join`/`split` for generic programming
- `ThreadLocal` cache for recursion detection
- Tapir's own `Configuration` class for naming

### Kindlings: Hearth-based macro derivation
- Uses Hearth's `CaseClass.parse`, `Enum.parse`, `SingletonValue.parse`
- `MLocal[Set[String]]` for recursion detection (macro-level, not thread-level)
- `ValDefsCache` for caching derived schemas within a single macro expansion
- `JsonSchemaConfig` abstraction bridges to actual JSON library configs
- Runtime annotation application via `TapirSchemaUtils.enrichSchema`
- `runtimePlainPrint` for type parameter name resolution (handles abstract type params at runtime)

### Key Architectural Gap: Discriminator Field on Children

The most significant behavioral difference is that upstream Tapir's `addDiscriminatorField` adds the discriminator field to each child schema's `SProduct`, while Kindlings only sets the `SDiscriminator` on the `SCoproduct`. This means:
- **Upstream**: Child schemas have the discriminator as a real field with `Validator.Enumeration` and `encodedDiscriminatorValue` attribute
- **Kindlings**: Child schemas are unchanged; only the parent coproduct has discriminator metadata

This could cause issues with OpenAPI generators that expect discriminator information on child schemas.

---

## Priority Recommendations

### High Priority (Behavioral Correctness)
1. **Add discriminator field to child schemas** -- implement `addDiscriminatorField`-equivalent behavior in `coproductSchema`
2. **Test `@default` annotation** -- already implemented in `enrichSchema`, just needs tests
3. **Test `@encodedExample` annotation** -- same as above
4. **Test recursive type SRef emission** -- current test does not verify SRef is produced
5. **Test `@validateEach` on collections** -- runtime handler exists, needs test

### Medium Priority (Feature Parity)
6. **Value class unwrapping** -- common pattern in Scala codebases
7. **Test nested sealed hierarchy** -- multi-level inheritance
8. **Test recursive sealed trait** -- `Node -> Edge(source: Node)`
9. **Test `@encodedName` on type** -- type-level name override
10. **Test annotation inheritance** -- sealed trait field annotations

### Low Priority (Niche Features)
11. **`oneOfUsingField` API** -- useful but can be done manually
12. **`oneOfWrapped` API** -- niche use case
13. **Union type derivation** -- Scala 3 only
14. **`derivedEnumeration` standalone API** -- enum config covers most cases
15. **`scala.Enumeration` support** -- legacy API
16. **Non-String map keys** -- intentionally unsupported (can be documented)

### Not Applicable / Different By Design
- **Tapir `Configuration`**: Kindlings intentionally uses JSON library configs instead
- **`Derived[Schema[T]]` wrapper**: Kindlings uses `KindlingsSchema[A]` wrapper instead
- **`SchemaAnnotations[T]` compile-time**: Kindlings applies annotations at runtime via `enrichSchema`
- **`derivedWithTypeParameter`**: Kindlings handles this via `runtimePlainPrint` instead
