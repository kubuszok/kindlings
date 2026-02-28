# Research Summary: Upstream Library Gap Analysis

> Generated 2026-02-28 from fresh upstream research (no prior assumptions).
> Detailed per-module analysis: `gap-circe.md`, `gap-jsoniter.md`, `gap-tapir.md`, `gap-avro.md`, `gap-yaml.md`
> Upstream feature catalogs: `circe-upstream.md`, `jsoniter-upstream.md`, `tapir-upstream.md`, `avro4s-upstream.md`

---

## Cross-Module Scorecard

| Module | Features | Tests | Critical Gaps |
|--------|----------|-------|---------------|
| **circe** | 12/17 (71%) | Good core, 27 missing scenarios | Case transform algorithm, Option+default matrix |
| **jsoniter** | 23/31 config fields (74%) | Good core, weak DoS/edge | Default differences (security), dead config field |
| **tapir** | 9/11 annotations, 5/12 derivation | 34 tests vs ~50 upstream | Discriminator child metadata, value class unwrap |
| **avro** | 12/13 annotations | Weakest test coverage | Recursive types untested, no schema evolution tests |
| **yaml** | Custom (exceeds upstream) | 124 tests, comprehensive | Mixed sealed hierarchies, unknown field handling |

---

## Priority 1 — Correctness Bugs / Security Issues

### P1.1: Jsoniter DoS protection defaults differ from upstream
- **`mapMaxInsertNumber`**: Kindlings `Int.MaxValue`, upstream `1024`
- **`setMaxInsertNumber`**: Kindlings `Int.MaxValue`, upstream `1024`
- **`checkFieldDuplication`**: Kindlings `false`, upstream `true`
- **Risk**: Users migrating from jsoniter-scala get weaker DoS protection by default
- **File**: `gap-jsoniter.md` § Summary

### P1.2: Jsoniter `requireDiscriminatorFirst` is dead code
- Config field exists in `JsoniterConfig` but is **never read** in macro implementation
- Discriminator is always required first regardless of this setting
- **Action**: Either wire it up or remove it to avoid confusing users
- **File**: `gap-jsoniter.md` § Field 22

### P1.3: Jsoniter `circeLikeObjectEncoding` behavioral difference
- Upstream encodes case objects as `{"Name":{}}` (wrapper with empty object)
- Kindlings encodes as bare `"Name"` (just the string)
- **Risk**: Incompatible JSON output for users expecting upstream behavior
- **File**: `gap-jsoniter.md`

### P1.4: Circe case transformation algorithm difference
- Upstream uses regex: `HTMLParser` → `html_parser`
- Kindlings uses char-by-char: `HTMLParser` → `h_t_m_l_parser`
- **Risk**: Users migrating from circe get different field names for types with consecutive capitals
- **File**: `gap-circe.md` § Implementation Differences #1

### P1.5: Jsoniter transient* defaults differ from upstream
- `transientDefault`, `transientEmpty`, `transientNone` all default `false` in Kindlings vs `true` upstream
- **Impact**: Different encoding behavior for users migrating from jsoniter-scala
- **File**: `gap-jsoniter.md` § Summary

---

## Priority 2 — Missing Tests (Correctness Verification)

### P2.1: Avro recursive types untested
- `RecursiveTree` defined in examples but zero test cases
- Self-recursive, mutual, via-List, via-Option patterns all untested
- **File**: `gap-avro.md` § Recursive types

### P2.2: Avro schema evolution untested
- No tests verify that schemas with `@avroDefault` enable backward-compatible reads
- No tests for union evolution with `@avroEnumDefault`
- **File**: `gap-avro.md` § Schema Evolution

### P2.3: Circe Option+default behavior matrix incomplete
- Upstream tests 6 scenarios: null/missing/wrong-type × with-default/without-default
- Kindlings tests basic cases but misses null-overrides-default for Option[T]
- **File**: `gap-circe.md` § Feature: Default Values

### P2.4: Tapir `@default` and `@encodedExample` annotations untested
- Runtime handler exists in `enrichSchema` but no test verifies they work
- **File**: `gap-tapir.md` § Annotations

### P2.5: Tapir recursive type `SRef` emission unverified
- `RecursiveTree` test only checks field names, not that `SRef` is actually emitted
- **File**: `gap-tapir.md` § Missing Test Cases

### P2.6: Tapir discriminator child metadata missing
- Upstream adds discriminator field to each child `SProduct` with single-value validator
- Kindlings only creates `SDiscriminator` on parent `SCoproduct`
- **Impact**: OpenAPI generators may produce incorrect schemas
- **File**: `gap-tapir.md` § Biggest behavioral gap

### P2.7: YAML mixed sealed hierarchies untested
- No test with both case objects and case classes as children of same sealed trait
- No multi-level sealed trait hierarchy test
- **File**: `gap-yaml.md` § Notable Gaps

### P2.8: YAML unknown field handling untested
- No test for what happens when YAML input contains fields not in the case class
- **File**: `gap-yaml.md` § Notable Gaps

---

## Priority 3 — Missing Features (Nice to Have)

### P3.1: Circe missing Configuration builder methods
- Missing: `withPascalCaseConstructorNames`, `withScreamingSnakeCaseConstructorNames`, `withoutDefaults`, `withoutDiscriminator`, `withoutStrictDecoding`
- **File**: `gap-circe.md` § Implementation Differences #6

### P3.2: Circe missing `incomplete` and `patch` decoders
- `DerivationHelper.incomplete` for partial constructor application
- `DerivationHelper.patch` for `Decoder[A => A]` partial updates
- **File**: `gap-circe.md` § Summary

### P3.3: Jsoniter missing type support
- `java.util.UUID`, 8 of 16 `java.time.*` types, `BitSet`, boxed Java primitives, `Either`, `Unit`
- **File**: `gap-jsoniter.md` § Type Support Gaps

### P3.4: Jsoniter missing config fields (8)
- `bigIntDigitsLimit`, `bitSetValueLimit`, `allowRecursiveTypes`, `skipNestedOptionValues`, `scalaTransientSupport`, `inlineOneValueClasses`, `alwaysEmitDiscriminator`, `transientNull`
- **File**: `gap-jsoniter.md` § Summary

### P3.5: Tapir missing derivation features
- Value class unwrapping, `oneOfUsingField`, `derivedEnumeration`, `Schema.modify`
- Many by design (Kindlings uses JSON library configs instead of Tapir's `Configuration`)
- **File**: `gap-tapir.md` § Summary

### P3.6: Avro missing annotations
- `@AvroUnionPosition` (critical for schema evolution)
- `@AvroScalePrecision` (per-field BigDecimal precision)
- **File**: `gap-avro.md` § Key Gaps

### P3.7: Avro missing types
- `OffsetDateTime`, `java.sql.Date`, `java.sql.Timestamp`
- Timestamp precision variants (micros, nanos — only millis supported)
- Byte collection special-casing (`List[Byte]` → bytes)
- **File**: `gap-avro.md` § Key Gaps

### P3.8: Avro missing functional combinators
- No `Codec[T]` combined typeclass
- No `contramap`, `map`, `forType` on encoders/decoders
- No schema merge
- **File**: `gap-avro.md` § Summary

---

## Priority 4 — Documentation / API Polish

### P4.1: Document default value differences
- Jsoniter config defaults differ from upstream in 7 fields
- Users migrating need clear migration guide
- **File**: `gap-jsoniter.md` § Summary

### P4.2: Document case transformation algorithm
- Circe consecutive-capital behavior differs from upstream
- Should be documented or fixed
- **File**: `gap-circe.md` § Implementation Differences #1

### P4.3: Tapir `@customise` annotation undocumented
- Implemented and wired up but no documentation or tests
- **File**: `gap-tapir.md` § @customise

### P4.4: Avro generic type name handling
- Kindlings always erases type parameters from schema names
- Could cause collisions with `Box[Int]` vs `Box[String]`
- Should be documented as known limitation
- **File**: `gap-avro.md` § Generic type names

---

## Already Exceeding Upstream

Kindlings adds features that upstream libraries don't have:

| Feature | Module | Notes |
|---------|--------|-------|
| Cross-compiled Scala 2.13 + 3 | All | Upstream libraries are often single-version |
| Named tuples (3.7+) | All | jsoniter-scala also has this, but others don't |
| Union types | circe, yaml | Scala 3 `A \| B` dispatch |
| Literal types | circe, avro | `42`, `"hello"` literal type support |
| IArray | All | Scala 3 immutable arrays |
| Opaque types | All | Transparent derivation through opaque wrappers |
| `enumAsStrings` config flag | circe, yaml | Simpler than upstream's separate derivation methods |
| `@transientField` | circe, jsoniter | Different from upstream but consistently designed across modules |
| Inline encode/decode macros | All | Direct `encode(value)` without creating intermediate typeclass instances |
| Non-string map key derivation | circe | Built-in for Int, Long, etc. |
| `PreferSchemaConfig` | tapir | Type-level JSON library selection for schema derivation |
