# Kindlings: Gaps, Issues & Action Items

Comprehensive gap analysis produced 2025-06-01 by cloning every upstream repository
we replace into `/tmp/`, diffing their test suites, features, and bug trackers against
ours, and analysing our own bug history for patterns.

**Repositories analysed:** avro4s, kittens, circe + circe-generic-extras, jsoniter-scala,
pureconfig, scalacheck-shapeless, scala-yaml, tapir, diffx, cats-tagless.

Legend: **P0** = blocks correctness / parity claim, **P1** = important for users migrating
from upstream, **P2** = nice to have / quality improvement.

---

## 0. API: Remove `derive`, keep only `derived`

**Priority: P1 | All 9 config modules**

The original intent was to expose **only** `derived[A]`. Nine modules currently also
expose `derive[A]`:

- circe (`KindlingsEncoder`, `KindlingsDecoder`, `KindlingsCodecAsObject`)
- jsoniter (`KindlingsJsonValueCodec`, `KindlingsJsonCodec`)
- pureconfig (`KindlingsConfigReader`, `KindlingsConfigWriter`, `KindlingsConfigConvert`)
- tapir (`KindlingsSchema`)
- xml (`KindlingsXmlEncoder`, `KindlingsXmlDecoder`, `KindlingsXmlCodec`)
- yaml (`KindlingsYamlEncoder`, `KindlingsYamlDecoder`, `KindlingsYamlCodec`)
- avro (`AvroEncoder`, `AvroDecoder`, `AvroSchemaFor`)
- sconfig (`ConfigReader`, `ConfigWriter`, `ConfigCodec`)
- ubjson (`UBJsonValueCodec`)

`derive` is redundant because:

1. `KindlingsEncoder[A] extends Encoder[A]` -- subtyping handles every use site expecting
   `Encoder[A]`.
2. Users wanting an explicit `Encoder[Foo]` in a companion write
   `given Encoder[Foo] = KindlingsEncoder.derived[Foo]` -- the upcast happens naturally,
   identically to what `derive` would give.
3. `derives KindlingsEncoder` calls `.derived` and puts a
   `given KindlingsEncoder[Foo]` in scope, found via subtyping when searching for
   `Encoder[Foo]`.
4. The self-reference issue (#119) is actually *worse* with `derive` (returns `Schema[A]`
   matching the val being initialised) and *better* with `derived` (returns
   `KindlingsSchema[A]`, avoiding the recursive implicit lookup).

The four no-config modules (cats-derivation, fast-show-pretty, diff-derivation,
scalacheck-derivation) already have only `derived` and no `derive`, proving the pattern
works.

**Action:** Deprecate `derive` in all modules, then remove. Update migration docs.

---

## 1. Cross-Cutting: Testing Methodology

### 1.1 Law-based property testing (P0)

**Affects: cats-derivation**

Kittens verifies algebraic laws for every type class via cats-laws discipline tests
(ScalaCheck): `EqTests[A].eqv`, `SemigroupTests[A].semigroup`,
`FunctorTests[F].functor[A,B,C]`, etc. Kindlings uses only hand-written unit tests
checking specific values. Bugs like non-associative Semigroup combine or non-identity
Functor map would go undetected.

**Action:** Add cats-laws discipline tests for all 30+ type classes.

### 1.2 Combinatorial testing matrices (P0)

**Affects: all derivation modules**

6 of 10 real bugs (#120, #78, #80, #79, #110, #108) came from features tested in
isolation but never composed. The test suite is structured around individual features,
missing cross-cutting combinations.

Required matrices per module:

**Wrapper x Inner type** -- for every wrapper (`Option`, `List`, `Map`, `Either`, `Set`)
test with every inner type (primitive, derived case class, sealed trait, value class,
generic type).

**Annotation x Type shape** -- for every annotation, test against case classes, sealed
traits of records, sealed traits of case objects / enums, value classes, and generic types.

**Annotation x Codec direction** -- every annotation must have encode + decode + full
round-trip tests. Never test schema-only or encoder-only.

**Name collision tests** -- same simple name in different packages, two instantiations of
same generic in one record, names that collide after case transformation.

### 1.3 Negative compilation tests (P2)

**Affects: cats-derivation, scalacheck-derivation**

Kittens tests that derivation correctly FAILS for invalid types (e.g., `Empty[Rgb]` for
case-object-only sealed traits). Kindlings has no compilation-failure tests.

### 1.4 Upstream bug tracker scan process (P1)

**Affects: all modules**

Issues #92 (avro nested default null) and #91 (avro generic type names) were bugs already
fixed in avro4s that we reimplemented without porting the fix. Before each release, review
recently-closed issues/PRs in upstream libraries. Port regression tests even if kindlings
doesn't currently have the bug.

### 1.5 Codegen quality audits (P2)

**Affects: all modules**

Issue #109 (avro decoder allocations) came from inspecting encoder codegen but not
decoder codegen. For performance-critical modules, inspect generated code (via
`logDerivation`) for unnecessary allocations, anonymous classes vs factories, and cached
lazy vals. Audit both encoder AND decoder paths symmetrically.

---

## 2. avro-derivation

### 2.1 Automatic Scala default value detection (P1)

avro4s automatically detects Scala default values (`String = "foo"`, `Int = 123`, etc.)
and writes them to the schema WITHOUT requiring annotations. Kindlings requires explicit
`@avroDefault("\"foo\"")` with JSON syntax. This is the single biggest avro UX gap.

Includes: defaults for sealed trait case objects/classes, `Option[T] = Some(v)` reordering
union, UUID defaults, Instant defaults, nested defaults-in-defaults.

### 2.2 Schema evolution (P1)

Reading old data with new schema: missing fields filled from Scala defaults,
`@avroAlias` for renamed fields across versions. No schema evolution tests exist.

### 2.3 Package-based automatic namespacing (P1)

avro4s uses the Scala package name as the Avro namespace automatically. Kindlings
requires explicit `@avroNamespace` annotation or `AvroConfig(namespace = ...)`.

Includes: sealed trait subtypes inheriting `@avroNamespace` from parent, empty namespace
`@AvroNamespace("")`, classes inside objects getting object's namespace.

### 2.4 `Seq[Byte]`/`List[Byte]`/`Vector[Byte]` as BYTES (P1)

avro4s treats byte collections as BYTES type (same as `Array[Byte]`). Kindlings would
treat them as ARRAY of INT.

### 2.5 `@avroFixed` on String fields (P2)

Kindlings `@avroFixed` only works on `Array[Byte]`. avro4s supports it on String fields
(encodes/decodes strings to/from fixed byte arrays).

### 2.6 `@avroNamespace` on fields (P2)

avro4s supports field-level `@AvroNamespace` to override the namespace of a nested type.
Kindlings only supports class/trait-level.

### 2.7 Mutually recursive types (P2)

`MutRec1 -> List[MutRec2] -> List[MutRec1]`. Kindlings tests self-recursion and indirect
recursion but not mutual recursion. Also: generic recursive types (`ListTree[+T]`,
`MapTree[+T]`).

### 2.8 Missing temporal types (P2)

`java.sql.Date`, `java.sql.Timestamp`, `OffsetDateTime`. avro4s supports these; kindlings
does not.

### 2.9 Streaming and container format (P2)

avro4s has Data format (Avro container files with embedded schema + codec support) and a
full streaming API (`AvroInputStream`/`AvroOutputStream`). Kindlings has single-value
`toBinary`/`fromBinary`/`toJson`/`fromJson` only.

### 2.10 `@avroProp` with JSON values (P2)

avro4s `@AvroProp("terms", jsonArray)` supports Jackson `JsonNode` values. Kindlings
only supports string key-value pairs.

### 2.11 `None.type` as standalone type (P2)

avro4s produces a NULL schema for `None.type`. Not tested in kindlings.

### 2.12 Parameterized enums with `val` fields (P2)

Scala 3 `enum ParametrizedWithInt(val param: Int)` treated as ENUM by avro4s. Not tested
in kindlings.

---

## 3. cats-derivation

### 3.1 Recursive polymorphic sealed traits for HKT (P0)

`IList[A]`, `Tree[A]`, `EnumK1[A]` (recursive Scala 3 enum) are tested in kittens for
Functor, Foldable, Traverse, Reducible, NonEmptyTraverse, etc. Absent from kindlings.

### 3.2 Missing test types (P1)

Types tested in kittens across 14+ type classes, absent from kindlings:

- `Interleaved[T](i: Int, t: T, l: Long, tt: Vector[T], s: String)` -- invariant + type
  param fields interleaved
- `Singletons[A](value: A, str: "Scala", lng: 42L, dbl: 3.14)` -- singleton literal
  type fields
- `Search[+A](move: A, child: Option[Search[A]], variations: List[Search[A]])` --
  multiple recursive positions
- `CaseClassWOption[A](value: Option[A])` -- Option-wrapped type param in HKT
- `ComplexProduct[T](lbl: String, set: Set[T], fns: Vector[() => T], opt: Eval[Option[T]])` --
  mixed container fields with Eval and function types
- `Bivariant[A](run: A => Boolean, store: A)` -- both covariant and contravariant use of
  same type param
- `GenericAdt[A]` (single-case sealed trait)
- `Recursive(i: Int, is: Option[Recursive])` -- self-recursive case class for algebraic
  type classes (Semigroup, Monoid, etc.)

### 3.3 Composition / type alias types for HKT (P1)

Kittens tests derivation for type aliases kindlings doesn't:

- `OptList[A] = Option[List[A]]` (nested functor composition)
- `AndChar[A] = (A, Char)` / `AndInt[A] = (A, Int)` (tuple-based)
- `NestedPred[A] = A => Boolean => Boolean` (double negation = covariant)
- `OptPred[A] = Option[A => Boolean]` (contravariant inside functor)
- `NelSCons[A] = NonEmptyList[SCons[A]]` (nested Reducible)
- `VecAndNel[A] = (Vector[A], NonEmptyList[A])` (mixed Foldable/Reducible tuple)

### 3.4 Respecting user-provided instances (P1)

Kittens systematically tests that derivation delegates to custom instances: `Box(Bogus(42))`
shows `"Box(content = Blah)"` via user-provided `Show[Bogus]`, `Box[Mul]` combines
multiplicatively via custom `Semigroup[Mul]`, etc. Kindlings tests this only minimally for
Eq.

### 3.5 Automatic derivation mode (P2)

`import auto.show.given` provides fully implicit derivation. Documented gap. Lower
priority because the maintainer deliberately chose "sanely-automatic" as the only mode.

### 3.6 `strict.semiauto` mode (P2)

Requires all transitive instances be manually declared. Kittens has it; kindlings does not.
Related to issue #85.

### 3.7 Serializable tests (P2)

Kittens verifies `SerializableTests.serializable(instance)` for all type classes. Not
tested in kindlings.

### 3.8 Bi-variant types for Bifunctor/Bifoldable/Bitraverse (P1)

`Result[+A, +E]` (multi-case bi-variant enum), `Nested[A, B] = Either[Option[A], Either[A, B]]`.
Not tested in kindlings.

---

## 4. circe-derivation

### 4.1 Patch decoders (P2)

`Decoder[A => A]` for PATCH-style updates where all fields are optional. Derives a
function that takes an existing `A` and overrides only the provided fields. Scala 2 only
in upstream (circe-generic-extras, Shapeless-based). No Scala 3 equivalent exists anywhere.

### 4.2 Incomplete/partial decoders (P2)

`deriveConfiguredFor[Int => Qux[String]].incomplete` -- decodes a partial JSON where some
fields come from a function argument. Scala 2 only in upstream. Niche but used for
dependency injection / partially-applied construction.

### 4.3 Programmatic strict-mode field list (P2)

`ExtrasDecoder.decodeStrict` returns `Either[(DecodingFailure, List[String]), A]` giving
programmatic access to extraneous field names. Kindlings has strict decoding but reports
fields only in the error message string, not as structured data.

### 4.4 Local class derivation test (P2)

circe tests derivation of classes defined inside methods (strict `val` vs `lazy val`
patterns). Not tested in kindlings.

### 4.5 Derivation from non-`io.circe` package (P2)

circe tests derivation from `com.example` package. Not tested in kindlings.

---

## 5. jsoniter-derivation

### 5.1 Missing java.time types (P0)

8 types with no codec: `DayOfWeek`, `Month`, `MonthDay`, `OffsetTime`, `Year`,
`YearMonth`, `ZoneId`, `ZoneOffset`. Each needs value codec + key codec + stringified
support.

### 5.2 Java boxed primitives (P1)

`java.lang.Byte`, `Short`, `Integer`, `Long`, `Boolean`, `Character`, `Double`, `Float`.
All 8 types are unsupported (top-level, in case classes, as map keys, with stringification).

### 5.3 `Either[A, B]` (P1)

Very commonly used type. jsoniter-scala supports via ADT (`Left`/`Right` with
discriminator). Kindlings does not.

### 5.4 `BitSet` support (P1)

`scala.collection.BitSet` / `immutable.BitSet` / `mutable.BitSet` with configurable
`bitSetValueLimit`. No BitSet support at all.

### 5.5 `IntMap`/`LongMap` specialized map types (P1)

`immutable.IntMap[V]`, `mutable.LongMap[V]`, `immutable.LongMap[V]`. None supported.

### 5.6 `requireDiscriminatorFirst` (P1)

Security-relevant config. Prevents CPU abuse by requiring the discriminator field to
appear first in the JSON object (no need to buffer/scan entire object).

### 5.7 Compile-time config conflict validation (P1)

jsoniter-scala rejects contradictory configs at compile time:
- `requireCollectionFields + transientEmpty = true`
- `requireDefaultFields + transientDefault = true`
- `decodingOnly + encodingOnly = true`
- `circeLikeObjectEncoding + discriminatorFieldName != None`

Kindlings does not validate these conflicts.

### 5.8 Missing collection types (P2)

`LinkedHashMap`, `LinkedHashSet`, `ListSet`, `ListMap`, `Queue`, `LazyList`, `Stream`,
`::[A]` (non-empty list enforcing non-empty JSON array), `Iterator[A]` (encoding only),
`AnyRefMap`, `OpenHashMap`, `UnrolledBuffer`, `Buffer`, `ListBuffer`, `ArrayStack`.

### 5.9 Default value differences from upstream (P1)

| Config field | jsoniter-scala | Kindlings | Risk |
|---|---|---|---|
| `transientDefault` | `true` | `false` | Silent behavior difference |
| `checkFieldDuplication` | `true` | `false` | Silent behavior difference |
| `discriminatorFieldName` | `Some("type")` | `None` | Different default encoding |
| `mapMaxInsertNumber` | `1024` | `Int.MaxValue` | DoS protection weaker |
| `setMaxInsertNumber` | `1024` | `Int.MaxValue` | DoS protection weaker |

### 5.10 `javaEnumValueNameMapper` (P2)

Remapping Java enum value names (e.g., `LOW` -> `lo`). No equivalent.

### 5.11 `skipNestedOptionValues` (P2)

`Option[Option[_]]` distinction between `null` and missing.

### 5.12 `alwaysEmitDiscriminator` (P2)

Emit discriminator when deriving codec for a leaf type (not just sealed base).

### 5.13 `inlineOneValueClasses` (P2)

Inline non-AnyVal single-field classes.

### 5.14 `IArray[A]` (Scala 3) (P2)

Immutable arrays. Supported in jsoniter-scala, not in kindlings.

### 5.15 Advanced tuple/named-tuple operations (P2)

Generic tuples >22 (`Tuple.Concat`, `Tuple.Drop`, `Tuple.Zip`, `Tuple.Map`), named tuple
operations (`NamedTuple.Reverse`, `Concat`, `Tail`, `Split`, `Zip`, `Map`).

### 5.16 Additional edge cases (P2)

- Non-case classes with `val`/`var` parameters
- Multiple parameter lists
- Private primary constructors
- Polymorphic default values
- Scala operators in field names
- `Char` as map key
- Non-abstract sealed base classes (`sealed class`)
- Diamond inheritance in ADTs
- Opaque types with upper bounds (`opaque type Meter <: Double`)

---

## 6. tapir-schema-derivation

### 6.1 `@default` annotation (P1)

Tapir supports `@default(value, encoded)` on fields for per-field default enrichment in
schemas. Kindlings does not apply this annotation.

### 6.2 `@encodedExample` annotation (P1)

Tapir supports `@encodedExample(example)` on fields for example value enrichment. Not
applied by kindlings.

### 6.3 `Schema.derivedEnumeration` (P1)

String-based enum schema API with `.defaultStringBased` / custom encoding. Kindlings
handles enums via `enumAsStrings` in config but no `derivedEnumeration` API.

### 6.4 `.modify(_.path)` post-derivation (P2)

Core tapir API: `.modify(_.f1.f2)(_.description(...))`, `.modify(_.f1.each)(fn)`, etc.
The kindlings `KindlingsSchema` type does not expose these. Users must call `.schema`
first then use tapir's native modify.

### 6.5 `Schema.oneOfWrapped` / `Schema.oneOfUsingField` (P2)

Manual union schema builders. Not replicated.

### 6.6 Scala 3 union type schemas (P2)

`Schema.derivedUnion` for `String | Int`, `Schema.derivedStringBasedUnionEnumeration`
for `"a" | "b"`. Not implemented.

### 6.7 Abstract class hierarchies with annotation inheritance (P2)

`sealed abstract class Pet` with `@description` inherited through intermediate traits
(`Rodent extends Pet`). Not tested.

---

## 7. pureconfig-derivation

### 7.1 Value class edge cases (P2)

Private-constructor rejection, double-wrapping (`FooDoubleWrapper`), generic value class
with custom reader override (`GenericValue[A]`). Not tested.

### 7.2 Candidate key suggestions in error messages (P2)

PureConfig includes candidate keys in `KeyNotFound` errors when it suspects a
misconfigured hint (e.g., kebab-case config keys when field expects snake_case). Not
implemented.

### 7.3 Tuple converter support (P2)

PureConfig's `TupleConvertersSuite` tests reading/writing tuples. Not tested in kindlings
pureconfig (yaml-derivation does test tuples).

### 7.4 `EnumCoproductHint` with custom `ConfigFieldMapping` (P2)

`deriveEnumerationReader(ConfigFieldMapping(PascalCase, SnakeCase))`. Not tested.

---

## 8. scalacheck-derivation

### 8.1 Direct recursive sealed trait support (P1)

`sealed trait Tree; case class Node(left: Tree, right: Tree, v: Int) extends Tree; case object Leaf extends Tree`
with size-based termination. Kindlings tests recursion only via bounded collection/Option
fields, not direct sealed trait recursion.

### 8.2 Size distribution verification (P2)

scalacheck-shapeless verifies tree depth grows logarithmically with size parameter.
No equivalent test.

### 8.3 Cogen for recursive types (P2)

`Cogen[Tree]` with recursive tree. Not tested.

### 8.4 ADT shrink behavior (P2)

Shrinking a case class ADT member should yield case objects as alternatives. Minimal
testing in kindlings.

---

## 9. diff-derivation

The diff module is an **independent implementation**, not a diffx port. It uses Myers diff
(vs diffx's index matching), has hierarchical string diff (line/word/char), and
`DiffRenderer`/`snapshot` that diffx lacks. Gaps below only matter if we claim diffx
replacement.

### 9.1 Post-derivation modification API (P1 if claiming diffx parity)

`.modify(_.path).ignore`, `.modify(_.path).setTo(d)`, `.modify(_.path).using(fn)`. This
is diffx's most distinctive feature.

### 9.2 ObjectMatcher system (P1 if claiming diffx parity)

Matching set/seq/map elements by key or property for structural alignment.

### 9.3 `Either` and `Tuple` diff (P2)

Built-in instances for `Either[A, B]` and `Tuple2`-`Tuple22`.

### 9.4 `Diff.approximate[T](epsilon)` (P2)

Numeric comparison with tolerance.

### 9.5 `Diff.useEquals[T]`, `Diff.ignored[T]`, `Diff.contramap` (P2)

Utility combinators for building diff instances.

### 9.6 Test framework integrations (P2)

scalatest (`DiffMatcher`), munit, weaver, utest, specs2 matchers.

---

## 10. cats-tagless-derivation

Work has begun (compiled artifacts for FunctorK/ContravariantK/InvariantK). Sources are
missing from master (likely in a branch/worktree).

### 10.1 Complete the K-family (P1)

- SemigroupalK (product of two interpreters)
- ApplyK (FunctorK + SemigroupalK composition)

### 10.2 AOP / Instrumentation (P2)

- `Instrument[Alg]` -- wraps results with algebra + method name
- `Aspect[Alg, Dom, Cod]` -- full AOP weaving with domain (args) + codomain (result)

### 10.3 Regular cats type classes for algebra traits (P2)

Functor, Contravariant, Invariant, Semigroupal, Apply, FlatMap, SemigroupK, MonoidK,
Bifunctor, Profunctor -- derived for the algebra's methods, not case classes.

### 10.4 Utility derivations (P2)

- `const[Alg, A](value)` -- all methods return constant
- `void[Alg]` -- all methods return `()`
- `readerT[Alg, F]` -- dependency injection pattern

---

## 11. yaml-derivation

### 11.1 Custom YAML tags (P2)

scala-yaml supports `LoadSettings(Map(CustomTag("!Custom") -> decoder))` for tag-based
dispatch. Kindlings does not.

### 11.2 `orElse` on decoders (P2)

scala-yaml's `YamlDecoder` provides `.orElse` for fallback decoders.

Note: Kindlings is substantially ahead of scala-yaml on features (Scala 2 support,
config system, annotations, value classes, recursive types, multi-level hierarchies).

---

## 12. Bug Pattern Analysis

Analysis of all 15 GitHub issues revealed systematic patterns:

### Root causes

| Root cause | Issues | Action |
|---|---|---|
| Combinatorial gap (features tested alone, never together) | #120, #78, #80, #79 | Combinatorial test matrices (1.2) |
| Annotation tested in one direction only | #110, #108 | Round-trip annotation tests (1.2) |
| Upstream test/fix not ported | #92, #91 | Upstream bug tracker scan (1.4) |
| Hearth/cross-quotes specific | #115, #65 | Already mitigated by guards |
| Performance/codegen quality | #109, #86 | Codegen quality audits (1.5) |
| UX/documentation/design | #119, #85, #62 | `derive` removal (0), docs |

### Specific combinatorial patterns to test across ALL modules

1. `Option[DerivedType]` where inner type has no pre-existing implicit (#120)
2. `Option[SealedTrait]` for union flattening / nested handling (#78)
3. Every annotation on sealed traits AND enums, not just case classes (#80, #108)
4. Every annotation tested in encode AND decode AND round-trip (#110, #108)
5. `Generic[A]` + `Generic[B]` in same parent for name collision (#91)
6. Same-name types in different packages (#79)
7. Nested null in default values (#92)
8. Single-element named tuples on Scala 3 (#115)

---

## 14. Limitation Investigation Results (2025-06-01)

All 6 discovered limitations were investigated in depth. None are Hearth limitations.

### 14.1 HKT derivation for sealed traits — KINDLINGS IMPLEMENTATION GAP

**Affects:** Functor, Foldable, Traverse, Apply, Applicative, Reducible, NonEmptyTraverse,
NonEmptyAlternative, Alternative for sealed traits like `IList[+A]`.

**Root cause:** All HKT derivations only have a `CaseClassRule`. No `EnumRule` exists for
HKT type classes. The monomorphic type classes (Show, Eq, Hash, Order, etc.) all have
enum rules that demonstrate the pattern.

**Hearth APIs available:** `Enum.parse` works with applied types (`IList[Any]`),
`Enum.matchOn` generates type-match dispatch, `subtypeTypeOf` correctly substitutes type
parameters for parameterized children.

**Fix:** Add `FunctorEnumRule` (and similar for each HKT type class) that:
1. Parses `F[Any]` as Enum
2. Uses `Enum.matchOn` for runtime type dispatch
3. Recursively derives Functor for each child's type constructor
4. Uses def-caching for recursive sealed traits

**Precedent:** `ShowEnumRule` for dispatch pattern, `ConsKMacrosImpl` bridge for type
constructor summoning.

**Effort:** Medium-high per type class (new rule + bridge + caching).

### 14.2 Nested type constructors in Functor — KINDLINGS IMPLEMENTATION GAP

**Affects:** Case classes with `Vector[A]`, `Option[A]`, `List[A]`, `Map[String, A]`
fields cannot derive Functor/Foldable/Traverse.

**Root cause:** `FunctorCaseClassRule` classifies fields as direct (`A`) or invariant
(no `A`), and rejects nested fields. The rejection is at lines 44-49 of
`FunctorCaseClassRule.scala`.

**Hearth APIs available:** Platform-specific `AppliedType` destructuring (Scala 3) and
`.typeConstructor` (Scala 2) can decompose `Vector[A]` into `Vector` + `A`.

**Fix:** Add a bridge method `summonFunctorForFieldType` (analogous to
`ConsKMacrosImpl.summonConsKForFieldType`), extend FunctorCaseClassRule to compose
summoned `Functor[Vector]` with the derived `Functor[F]` for nested fields. Add a
`FunctorRuntime.map(functor: Any, fa: Any, f: Any): Any` runtime helper.

**Precedent:** `ConsKMacrosImpl.summonConsKForFieldType` lines 30-31.

**Effort:** Medium.

### 14.3 Bifunctor/Bifoldable/Bitraverse for sealed traits — KINDLINGS IMPLEMENTATION GAP

**Affects:** `Result[+A, +E]` and similar bi-variant sealed traits.

**Root cause:** Same as 14.1 — only `CaseClassRule` exists, no enum rule. Additionally,
Bifoldable and Bitraverse don't even use the rules system (direct `CaseClass.parse`).

**Hearth APIs available:** Same as 14.1 — `Enum.parse[F[Int, Int]]` works,
`Type.Ctor2.fromUntyped` handles child type constructor extraction.

**Fix:** Add `BifunctorEnumRule` following the `ShowEnumRule` pattern + three-probe
technique per child.

**Effort:** Medium.

### 14.4 Scala 3 splice isolation for sealed traits in composite types — LIKELY ALREADY FIXED

**Affects:** `KindlingsEncoder.encode(combOuter)` and `KindlingsCodecAsObject.derived[CombOuter]`
on Scala 3 when `CombOuter` contains sealed trait fields.

**Root cause:** The circe encoder's inline entry points (`encode`, `derived`) expand macros
that derive sealed trait encoders. On Scala 3, the generated tree for sealed traits
(using `parMatchOn` in `EncoderHandleAsEnumRule`) may reference expressions across splice
boundaries.

**Key finding:** The jsoniter-derivation module has the identical pattern (CombOuter with
sealed trait fields) and it WORKS on Scala 3. The circe encoder already uses the
def-caching pattern (`forwardDeclare` + `buildCachedWith` + `toValDefs.use`). The issue
may have been in an earlier version and the workaround was left in place.

**Next step:** Re-add sealed trait fields to circe `CombOuter` and test on Scala 3 to
verify whether the issue still exists. If it does, investigate `LambdaBuilder.build` +
`resetOwner.asTerm` interaction with match expressions.

**Effort:** Small (verification) to medium (if LambdaBuilder fix needed).

### 14.5 Order[Shape] transitivity — GENUINE DERIVATION BUG

**Affects:** `Order` and `PartialOrder` for ALL sealed traits.

**Root cause:** `OrderEnumRule.scala` line 46 uses `Integer.compare(x.hashCode(), y.hashCode())`
for cross-constructor comparison. Hash codes provide NO ordering guarantee — they're not
monotonic, not transitive, and collide frequently.

**Minimal counterexample:** `a = Circle(100.0)`, `b = Rectangle(0.0, 0.0)`,
`c = Circle(0.0)` where `a.hashCode() < b.hashCode() < c.hashCode()` but
`compare(a, c) = Double.compare(100.0, 0.0) > 0`. Transitivity violated.

**How kittens does it correctly:** Uses ordinal indices (position in sealed trait children
list) for cross-constructor comparison. Ordinals are deterministic and transitive.

**Fix:** Replace `Integer.compare(x.hashCode(), y.hashCode())` with ordinal-based
comparison in `OrderEnumRule.scala`. Since `Enum.directChildren` returns a `ListMap`
preserving declaration order, assign each child an ordinal from its position. Generate:
`Integer.compare(ordinalOf(x), ordinalOf(y))`.

**Also affects:** `PartialOrderMacrosImpl` which delegates to Order.

**Effort:** Small — single line change in the rule + ordinal assignment.

### 14.6 Direct recursive Arbitrary stack overflow — KINDLINGS IMPLEMENTATION GAP

**Affects:** `Arbitrary` for directly recursive sealed traits like
`BNode(left: BinaryTree, right: BinaryTree)`.

**Root cause:** Neither `ArbitraryHandleAsEnumRule` nor `ArbitraryHandleAsCaseClassRule`
uses `Gen.sized` or `Gen.resize`. The enum rule produces `Gen.oneOf(genBNode, genBLeaf)`
without size gating. The case class rule uses `sequenceGens` (plain `flatMap`) without
size reduction for recursive fields.

**Collections/Options work because:** `ArbitraryHandleAsCollectionRule` wraps in
`Gen.sized { n => Gen.resize(max(n/2, 0), Gen.listOf(...)) }`. `ArbitraryHandleAsOptionRule`
uses `Gen.sized { n => if (n <= 0) Gen.const(None) else ... }`.

**How scalacheck-shapeless handles it:** `Recursive[T]` marker + `Gen.sized` in
`MkRecursiveCoproductArbitrary` (line 170-175). For products, splits size budget:
`headSize = size / (n + 1)`.

**Fix (two cooperating rules):**
1. Enum rule: detect self-recursion, wrap in `Gen.sized`, bias toward non-recursive
   children (leaves) at small sizes via `Gen.frequency`
2. Case class rule: for fields matching `derivedType`, use `Gen.resize(n/2, fieldGen)`

**Effort:** Small-medium.

### Summary

| # | Limitation | Classification | Effort | Blocks parity? |
|---|---|---|---|---|
| 14.1 | HKT for sealed traits | Kindlings gap | Medium-high | Yes (kittens) |
| 14.2 | Nested type constructors | Kindlings gap | Medium | Yes (kittens) |
| 14.3 | Bi* for sealed traits | Kindlings gap | Medium | Partial |
| 14.4 | Splice isolation (circe) | Likely already fixed | Small | No (workaround exists) |
| 14.5 | Order transitivity | **Derivation bug** | Small | Yes (correctness) |
| 14.6 | Arbitrary recursion | Kindlings gap | Small-medium | Partial |

---

## 13. Code Coverage Analysis

**Source:** https://app.codecov.io/gh/kubuszok/kindlings (2025-06-01)

**Overall: 73.9%** (15,399 / 20,850 lines hit across 567 files)

### Per-module coverage

| Module | Files | Lines | Hits | Missed | Coverage |
|---|---|---|---|---|---|
| fast-show-pretty | 21 | 541 | 498 | 43 | **92.1%** |
| cats-integration | 4 | 231 | 209 | 22 | **90.5%** |
| json-schema-config-macro-providers | 2 | 9 | 8 | 1 | **88.9%** |
| refined-integration | 1 | 22 | 19 | 3 | **86.4%** |
| iron-integration | 1 | 20 | 17 | 3 | **85.0%** |
| avro-derivation | 57 | 2,210 | 1,821 | 389 | **82.4%** |
| tapir-schema-derivation | 19 | 572 | 461 | 111 | **80.6%** |
| jsoniter-derivation | 35 | 2,529 | 2,008 | 521 | **79.4%** |
| cats-derivation | 135 | 3,897 | 3,047 | 850 | **78.2%** |
| scalacheck-derivation | 40 | 864 | 659 | 205 | **76.3%** |
| circe-derivation | 37 | 1,626 | 1,210 | 416 | **74.4%** |
| yaml-derivation | 37 | 1,247 | 929 | 318 | **74.5%** |
| ubjson-derivation | 32 | 1,639 | 1,176 | 463 | **71.8%** |
| jsoniter-json | 7 | 180 | 128 | 52 | **71.1%** |
| sconfig-derivation | 42 | 1,395 | 879 | 516 | **63.0%** |
| pureconfig-derivation | 37 | 1,263 | 794 | 469 | **62.9%** |
| xml-derivation | 35 | 1,336 | 809 | 527 | **60.6%** |
| diff-derivation | 23 | 1,233 | 717 | 516 | **58.2%** |
| derivation-commons | 2 | 36 | 10 | 26 | **27.8%** |

### 13.1 Def-caching rules: 0% coverage across ALL modules (P0)

Every `*UseCachedDefWhenAvailableRule` file has **0% coverage** -- 17 files across all
derivation modules (269 missed lines total). These rules fire during recursive type
derivation. Since they are completely untested, recursive type scenarios like
`Tree(value, children: List[Tree])` may appear to work in simple tests but the caching
path is never verified.

Affected modules and files:
- avro: `AvroDecoderUseCachedDefWhenAvailableRule`, `AvroEncoderUseCachedDefWhenAvailableRule`, `AvroSchemaForUseCachedDefWhenAvailableRule`
- circe: `EncoderUseCachedDefWhenAvailableRule`
- jsoniter: `DecoderUseCachedDefWhenAvailableRule`, `EncoderUseCachedDefWhenAvailableRule`
- pureconfig: `ReaderUseCachedDefWhenAvailableRule`, `WriterUseCachedDefWhenAvailableRule`
- sconfig: `ReaderUseCachedDefWhenAvailableRule`, `WriterUseCachedDefWhenAvailableRule`
- tapir: `SchemaUseCachedWhenAvailableRule`
- ubjson: `EncoderUseCachedDefWhenAvailableRule`, `DecoderUseCachedDefWhenAvailableRule`
- xml: `EncoderUseCachedDefWhenAvailableRule`, `DecoderUseCachedDefWhenAvailableRule`
- yaml: `DecoderUseCachedDefWhenAvailableRule`, `EncoderUseCachedDefWhenAvailableRule`

**Action:** Add recursive type tests (direct and indirect recursion) to every module.
These rules are the core mechanism for preventing infinite macro expansion -- they must
be exercised.

### 13.2 Named tuple rules: ~8-20% coverage in pureconfig and sconfig (P1)

| File | Coverage | Missed |
|---|---|---|
| pureconfig `ReaderHandleAsNamedTupleRule` | 7.5% | 74 lines |
| sconfig `ReaderHandleAsNamedTupleRule` | 8.0% | 69 lines |
| pureconfig `WriterHandleAsNamedTupleRule` | 17.6% | 28 lines |
| sconfig `WriterHandleAsNamedTupleRule` | 19.4% | 25 lines |

Named tuple support is implemented but barely tested in these modules. Since named tuples
are a Scala 3.5+ feature, these rules only run on Scala 3 -- the low coverage may
indicate CI only runs Scala 2 coverage, or the tests simply don't exist.

### 13.3 Map handling rules: 15-25% coverage across multiple modules (P1)

| File | Coverage | Missed |
|---|---|---|
| yaml `DecoderHandleAsMapRule` | 14.6% | 35 lines |
| pureconfig `ReaderHandleAsMapRule` | 15.0% | 34 lines |
| cats `ShowMapRule` | 24.0% | 19 lines |
| diff `DiffMapRule` | 25.0% | 18 lines |

Map decoding/handling rules are implemented but barely tested. This means `Map[K, V]`
fields in case classes may have untested edge cases (non-string keys, empty maps, nested
maps, maps of sealed traits).

### 13.4 diff-derivation: 58.2% overall, multiple core files under 50% (P1)

The diff module has the worst coverage of any non-trivial module:

| File | Coverage | Missed |
|---|---|---|
| `DiffResult.scala` | 16.3% | 108 lines |
| `DiffRuntime.scala` | 17.2% | 72 lines |
| `DiffRenderer.scala` | 44.3% | 113 lines |
| `MyersCleanup.scala` | 50.8% | 65 lines |

`DiffResult` (the ADT representing diff output) is 16% covered -- most of the result
types are never constructed in tests. `DiffRuntime` (the runtime diff engine) is 17%
covered. `DiffRenderer` (formatting output) is 44% covered. This means the diff module
has significant untested code paths in its core runtime.

### 13.5 sconfig-derivation: 63% overall, reader/writer under 38% (P1)

| File | Coverage | Missed |
|---|---|---|
| `ConfigReader.scala` (built-in instances) | 35.4% | 42 lines |
| `ConfigWriter.scala` (built-in instances) | 37.5% | 50 lines |
| `SConfig.scala` (configuration) | 37.5% | 15 lines |
| `ConfigDecodingError.scala` | 19.0% | 17 lines |
| `SConfigDerivationUtils.scala` | 60.0% | 50 lines |

The built-in reader/writer instances for primitives, Option, etc. are under 38% coverage.
Config convenience methods (`.withSnakeCaseMemberNames`, `.withStrictDecoding`, etc.) are
also mostly untested.

### 13.6 xml-derivation: 60.6% overall, config/utils under 44% (P1)

| File | Coverage | Missed |
|---|---|---|
| `XmlConfig.scala` | 15.2% | 50 lines |
| `XmlDerivationUtils.scala` | 43.8% | 108 lines |
| `DecoderMacrosImpl.scala` | 50.8% | 94 lines |

The XML configuration object (field modes, name mappers, discriminator attribute, etc.)
is 15% covered. Most config options are never exercised in tests.

### 13.7 pureconfig-derivation: 62.9% overall (P1)

| File | Coverage | Missed |
|---|---|---|
| `PureConfigDerivationUtils.scala` | 47.4% | 72 lines |
| `ReaderMacrosImpl.scala` | 72.5% | 38 lines |
| `WriterMacrosImpl.scala` | 71.3% | 37 lines |
| `PureConfig.scala` (configuration) | 37.5% | 15 lines |

### 13.8 circe `CirceDerivationUtils.scala`: 41.4% (109 missed lines) (P1)

This is the runtime utilities file for circe derivation. Over half its code is untested.
This includes helper methods for encoding/decoding maps, collections, options, value
types, etc.

### 13.9 `UBJsonValueCodecExtensions`: 0% (20 lines) (P2)

The `map` and `mapDecode` combinators for transforming existing UBJson codecs are
completely untested.

### 13.10 `EmptyEnumRule` in cats-derivation: 4.2% (P2)

The rule for deriving `Empty` for sealed traits/enums is almost completely untested
(1/24 lines hit). Only the entry point is reached; the actual derivation logic never runs.

### 13.11 Cats Show/Hash/Group for specific type shapes (P2)

| File | Coverage | Missed |
|---|---|---|
| cats `ShowMacrosImpl.scala` | 60.0% | 34 lines |
| cats `HashMacrosImpl.scala` | 59.8% | 33 lines |
| cats `GroupBuiltInRule.scala` | 38.1% | 26 lines |
| cats `MonoidBuiltInRule.scala` | 44.0% | 14 lines |
| cats `ShowPrettyEnumRule.scala` | 34.4% | 21 lines |
| cats `ShowCollectionRule.scala` | 30.0% | 14 lines |
| cats `ShowOptionRule.scala` | 33.3% | 12 lines |

Show for collections, maps, and options; Hash multi-method caching; Group/Monoid built-in
rules -- all have significant untested paths. The `ShowPrettyEnumRule` for sealed traits
is 34% covered.

### 13.12 `DerivationTimeout`: 13.3% (P2)

The configurable derivation timeout from `derivation-commons` is barely tested. The
timeout parsing logic (seconds, milliseconds, minutes formats, invalid value fallback)
has 26 of 30 lines uncovered.

### 13.13 Scalacheck `ShrinkUtils`: 30.8% (P2)

The runtime utilities for Shrink derivation (shrinking sealed trait members to case
objects, shrinking collections, etc.) are 31% covered. 36 of 52 lines untested.

### 13.14 `EqOptionRule`: 23.8% (P2)

The rule for `Eq[Option[A]]` handling is barely tested (5/21 lines).

### Coverage: summary of critical untested areas

| Priority | Area | Impact |
|---|---|---|
| P0 | Def-caching rules (0% across 17 files) | Recursive types may silently break |
| P1 | Named tuple rules (8-20% in pureconfig/sconfig) | Named tuples may silently break |
| P1 | Map handling rules (15-25% across 4 modules) | Map fields may have untested edge cases |
| P1 | diff-derivation core (16-44%) | Most diff result types and rendering untested |
| P1 | sconfig built-in instances (35-38%) | Basic type read/write partially untested |
| P1 | xml config + utils (15-44%) | Most XML config options never exercised |
| P1 | pureconfig utils (37-47%) | PureConfig convenience methods untested |
| P1 | circe runtime utils (41%) | Runtime encoding/decoding helpers partially untested |
| P2 | cats Show/Hash/Group for enums/collections/maps | Specific type shapes untested |
| P2 | DerivationTimeout (13%) | Timeout parsing barely tested |
| P2 | Scalacheck ShrinkUtils (31%) | Shrink runtime largely untested |
