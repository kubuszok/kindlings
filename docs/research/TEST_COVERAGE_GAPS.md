# Test Coverage Gaps: Kindlings vs Upstream Libraries

Generated 2026-04-24 by auditing upstream library test suites against kindlings tests.

**Purpose:** Identify test scenarios present in upstream libraries but missing from kindlings,
to prevent behavioral divergences that could affect users migrating from the upstream library.

**Methodology:** Cloned upstream repos, read all test files, compared against kindlings test suites.
Shapeless-specific tests are excluded (not portable). Stream/IO/server tests are excluded (not reimplemented).

---

## avro-derivation (vs avro4s)

**Upstream:** `sksamuel/avro4s` — 4.x (Scala 2.13), 5.x/master (Scala 3)
**Kindlings tests:** 97 schema + 52 encoder + 50 decoder + 40 round-trip + 35 Scala 3

### High-Priority Gaps

| Gap | avro4s Test | What It Tests | Risk |
|-----|------------|---------------|------|
| Schema evolution (missing fields) | `SchemaEvolutionTest.scala` | Decoding with missing fields using defaults/Option | Data loss on schema upgrade |
| Field reordering on decode | `ReorderFieldsDecoderTest.scala` | Decoding when source fields are in different order | Fails on schema evolution |
| Complex default values | `DefaultValueSchemaTest.scala` | Defaults for Maps, Sets, Seqs, nested ADTs | Schema compatibility issues |
| Seq[Byte]/List[Byte] as BYTES | `ByteArraySchemaTest.scala` | Byte sequences treated as BYTES not ARRAY[INT] | Different schema for byte collections |
| Map[String, Option[T]] | `MapSchemaTest.scala` | MAP with nullable value type | Wrong schema for optional map values |

### Medium-Priority Gaps

| Gap | avro4s Test | What It Tests |
|-----|------------|---------------|
| Top-level collections | `ArraySchemaTest.scala` | Array/List/Vector/Set as root type |
| Enum schema compatibility | `EnumSchemaCompatibilityTest.scala` | Schema evolution with new enum symbols |
| Custom SchemaFor override | `SchemaForTypeclassOverrideTest.scala` | User-provided SchemaFor taking priority |
| Field mapper transforms | `FieldMapperFieldTest.scala` | Custom field name mapping functions |
| Recursive with Either/coproduct | `RecursiveSchemaTest.scala` | Recursive types combined with Either |

---

## cats-derivation (vs kittens)

**Upstream:** `typelevel/kittens`
**Kindlings tests:** ~100 tests in CatsDerivationSpec

### High-Priority Gaps

| Gap | What It Tests | Risk |
|-----|---------------|------|
| PartialOrder derivation | Incomplete orderings for partially ordered types | Missing type class entirely |
| Property-based law verification | cats-laws discipline checks (transitivity, consistency, etc.) | Derived instances may violate algebraic laws |
| Complex recursive structures | Tree, IList, nested enums with ScalaCheck properties | Derivation may fail on complex shapes |

### Medium-Priority Gaps

| Gap | What It Tests |
|-----|---------------|
| Band (idempotent semigroup) | combine(x,x) == x law |
| Group / CommutativeGroup | Invertible monoids |
| Semilattice / BoundedSemilattice | Lattice algebraic properties |
| Bifunctor / Bitraverse / Bifoldable | 2-argument type constructor derivation |
| ShowPretty | Formatted multi-line pretty-printing |
| Respecting existing instances | User-provided instances take priority over derived |

---

## circe-derivation (vs circe)

**Upstream:** `circe/circe` (generic, generic-extras, configured derives)
**Kindlings tests:** 83 encoder + 128 decoder + 40 round-trip + Scala 3 spec

### High-Priority Gaps

| Gap | circe Test | What It Tests | Risk |
|-----|-----------|---------------|------|
| Null discriminator rejection | `ConfiguredDerivesSuite.scala` | Decode fails when discriminator is null | Silent wrong-type dispatch |
| Option null vs missing matrix | `ConfiguredDerivesSuite.scala` | Exhaustive Option/default interaction | Wrong default applied on null vs absent |
| Strict mode + error accumulation | `ConfiguredDerivesSuite.scala` | Fail-fast vs accumulation under strict | Incomplete error reporting |
| Recursive types + discriminator | `ConfiguredDerivesSuite.scala` | Tree with discriminator on sealed trait | Fails on complex recursive ADTs |

### Medium-Priority Gaps

| Gap | circe Test | What It Tests |
|-----|-----------|---------------|
| Large products (33+ fields) | `DerivesSuite.scala` | Macro scalability with 33-field case class |
| Large enums (33+ variants) | `DerivesSuite.scala` | Sealed trait with 33 case classes |
| Multi-level hierarchy field collision | `ConfiguredDerivesSuite.scala` | Sibling field name matching type name |
| ConfiguredEnumCodec validation | `ConfiguredEnumDerivesSuites.scala` | Compile error for non-singleton enum cases |
| Generics + defaults combined | `ConfiguredDerivesSuite.scala` | Generic case class with default values |

---

## jsoniter-derivation (vs jsoniter-scala)

**Upstream:** `plokhotnyuk/jsoniter-scala`
**Kindlings tests:** 180 codec + 18 inline + 11 JVM-specific

### High-Priority Gaps

| Gap | jsoniter-scala Test | What It Tests | Risk |
|-----|-----------|---------------|------|
| Scala 3 opaque types | `JsonCodecMakerNewTypeSpec.scala` | Opaque type codec derivation | Missing Scala 3 feature |
| Scala 3 union types | `JsonCodecMakerNewTypeSpec.scala` | `A \| B` union type codecs | Missing Scala 3 feature |
| Scala 3 literal types | `JsonCodecMakerLiteralTypesSpec.scala` | Literal type constraints | Missing Scala 3 feature |
| Comprehensive Scala 3 enums | `JsonCodecMakerNewEnumSpec.scala` | Recursive enums, enum ADTs, F-bounded enums | ~20 untested scenarios |
| Array[T] support | `JsonCodecMakerSpec.scala` | Array[Array[Int]], Array[BigInt], etc. | Wrong codec for arrays |
| Discriminator position | `JsonCodecMakerSpec.scala` | `requireDiscriminatorFirst` flag | Different parse behavior |

### Medium-Priority Gaps

| Gap | What It Tests |
|-----|---------------|
| Advanced collection types | HashMap, TreeMap, BitSet, ArrayBuffer, etc. (~25 types) |
| Iterator[T] codecs | Streaming iterator support |
| UUID type codec | Parsing/error handling for UUIDs |
| GADT support | Generalized ADTs with type refinements |
| Nested sealed trait hierarchies | Intermediate non-sealed traits |
| Error message formatting | Hex dumps, offset info in parse errors |
| Scala 3 `derives` keyword | `derives JsonValueCodec` syntax |

---

## pureconfig-derivation (vs pureconfig)

**Upstream:** `pureconfig/pureconfig` (generic-scala3 module)
**Kindlings tests:** 24 reader + 10 writer + 6 convert + 3 JVM-specific

### High-Priority Gaps

| Gap | pureconfig Test | What It Tests | Risk |
|-----|-----------|---------------|------|
| Tuple field reading | `ProductReaderDerivationSuite.scala` | Case class with tuple field from list | Missing type support |
| Complex defaults (FiniteDuration, Some) | `ProductConvertDerivationSuite.scala` | Non-primitive default values | Wrong defaults for complex types |
| Lazy default evaluation | `ProductConvertDerivationSuite.scala` | Defaults not evaluated if field present | Side effects from unevaluated defaults |
| Value classes | `ValueClassSuite.scala` | AnyVal wrapper derivation | Missing type support |
| Recursive product types | `ProductReaderDerivationSuite.scala` | Tree-like self-referential case classes | Macro infinite loop |

### Medium-Priority Gaps

| Gap | What It Tests |
|-----|---------------|
| ReadsMissingKeys / WritesMissingKeys | Custom reader/writer interface for sparse configs |
| Option field omission on write | None fields omitted from output |
| CoproductHint field name conflicts | Discriminator field collides with subtype field |
| FirstSuccessCoproductHint | Try-each-subtype strategy (alternative to discriminator) |
| Failure accumulation (strict mode) | Multiple errors reported at once |
| Misconfigured hint detection | Suggests similar keys on key-not-found |
| Enumeration error messages | Detailed errors for unknown enum values |

---

## scalacheck-derivation (vs scalacheck-shapeless)

**Upstream:** `alexarchambault/scalacheck-shapeless`
**Kindlings tests:** 15 Arbitrary tests

### High-Priority Gaps

| Gap | What It Tests | Risk |
|-----|---------------|------|
| Shrink derivation | Minimizing counter-examples on test failure | Core PBT feature entirely missing |
| Cogen derivation | Function generators for Prop.forAll | Cannot generate random functions |
| Property-based validation | Generators used in actual Prop.forAll | Generators may produce invalid data |
| Recursive ADT size control | Size parameter governs tree depth | Infinite structures / stack overflow |

### Medium-Priority Gaps

| Gap | What It Tests |
|-----|---------------|
| Singleton type generators | Witness literal type Arbitrary |
| Generator comparison tests | Precise generator equality verification |
| Empty/unit type generators | Edge case: Empty.type, EmptyCC |

---

## tapir-schema-derivation (vs tapir)

**Upstream:** `softwaremill/tapir`
**Kindlings tests:** 44 schema tests

### High-Priority Gaps

| Gap | tapir Test | What It Tests | Risk |
|-----|-----------|---------------|------|
| Schema modification API | `SchemaMacroTest.scala` | `.modify()` DSL for post-derivation customization | Missing user-facing API |
| Non-string map keys | `SchemaGenericAutoTest.scala` | SOpenProduct with custom key serialization | Wrong schema for non-String maps |
| Discriminator via trait method | `SchemaMacroTest.scala` | `oneOfUsingField` with method-based discriminator | Different ADT schema generation |
| Value class schemas | `SchemaMacroTest2.scala` | AnyVal wrapper unwrapping | Missing type support |

### Medium-Priority Gaps

| Gap | What It Tests |
|-----|---------------|
| Naming transformations (snake, kebab, SCREAMING) | Global field name config |
| Discriminator name transforms | Subtype name transformation variants |
| Scala 3 union type schemas | `A \| B` union derivation |
| Collection element validators | `@validateEach` on List/Option |
| Schema serialization | ObjectOutputStream round-trip |
| Subtypes inherit parent annotations | Sealed trait annotation propagation |

---

## Cross-Cutting Themes

These gaps appear across multiple modules:

1. **Scala 3 features** (opaque types, union types, literal types, `derives` keyword) — gaps in jsoniter, tapir
2. **Value class / AnyVal support** — gaps in pureconfig, tapir
3. **Property-based / law verification testing** — gaps in cats, scalacheck
4. **Schema evolution / field reordering** — gaps in avro, pureconfig
5. **Large type stress tests** (33+ fields/variants) — gaps in circe, jsoniter
6. **Error message quality** — gaps in circe, pureconfig, jsoniter
7. **Complex default value handling** — gaps in avro, pureconfig, circe
