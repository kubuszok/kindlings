# FAQ

## Why not contribute these improvements to Circe, Jsoniter Scala, etc.?

**Different foundations require different codebases.** Kindlings' derivation is built on [Hearth](https://github.com/kubuszok/hearth), a macro toolkit that provides high-level, cross-platform abstractions over Scala 2 and Scala 3 metaprogramming. Existing libraries use their own macro infrastructure (or Shapeless/Mirrors). Replacing the internals of a library with a completely different macro foundation is not a patch — it's a rewrite of the derivation layer.

**Independence allows faster iteration.** Kindlings can support new type categories (named tuples, opaque types, Java enums), experiment with better error messages, and optimize compilation speed without being blocked by the release cycle or design philosophy of upstream libraries.

**Maintaining cross-version compatibility is a design constraint.** Most libraries have separate Scala 2 and Scala 3 implementations with different capabilities. Kindlings shares a single derivation logic across both versions.

## What does "sanely-automatic" mean?

Sanely-automatic derivation means three things:

1. **Semi-automatic is recursive.** When you call `KindlingsEncoder.derived[Person]`, it derives instances for `Person` and all its nested types (`Address`, `List[Address]`, etc.) in a single macro expansion — no need to declare instances for each type manually.

2. **Automatic has no overhead over semi-automatic.** For a single derivation site, automatic and semi-automatic produce identical generated code — same compilation cost, same runtime performance. The generated code is as fast as what you'd write by hand.

3. **Errors are informative and actionable.** When derivation fails, you get a clear message telling you which type is missing an instance and where in the type hierarchy the problem is — not a cryptic `diverging implicit expansion`.

If the same type is auto-derived at multiple call sites, each site derives independently. This is still cheaper than Shapeless/Mirrors-based automatic derivation, but if you want to guarantee a type is derived exactly once, use semi-automatic (`KindlingsEncoder.derived[A]`) and assign it to an `implicit val` / `given`.

## Why not separate automatic and semi-automatic derivation?

Most Scala libraries split derivation into "automatic" (an import makes instances appear wherever needed) and
"semi-automatic" (you must write `deriveX[A]` for every type). Kindlings deliberately does **not** offer a separate
semi-automatic-only mechanism, and it does **not** make recursion optional. The reasoning:

- **The auto/semi distinction is mostly arbitrary.** Every library always supports primitives, collections, `Option`,
  newtypes, refined types, etc. unconditionally. "Automatic vs semi-automatic" really only governs **case classes and
  sealed hierarchies**. Explaining to a newcomer why a `List[String]` inside a case class "just works" while the case
  class itself needs a special import — and why disabling recursion still leaves `Option`/`List`/newtypes working — is
  hard, because the line is an implementation detail leaking into the API.

- **The two-imports model hurts the people we most want to help.** Needing to learn that there are two sets of imports,
  where swapping one silently changes your program's semantics, empowers Scala experts at the cost of newcomers and
  people focused on the business problem rather than the language. Kindlings optimizes for lowering that barrier.

- **Semi-automatic doesn't actually guarantee coherence anyway.** One stray import, a new instance added to a companion,
  or an instance introduced up an inheritance hierarchy can still change runtime behavior — invisibly, during a
  refactor. If you care about a wire contract, **golden tests** are the real guard (and they also catch serde library
  bugs and migrations between JSON libraries, which no derivation strategy can).

What Kindlings *does* offer for teams that want tighter control is a **global, per-library opt-out** of automatic
derivation: the [Derivation Policy](derivation-policy.md). Set it to `opt-in` and structural derivation is only allowed
in the scopes you designate (or behind an explicit import) — everywhere else you get a clear compile error telling you
to define the instance explicitly. This achieves the "instances only appear where I decided" goal **without** a second
API surface and **without** giving up recursive, single-expansion derivation. See
[issue #85](https://github.com/kubuszok/kindlings/issues/85) for the full discussion.

## Can I use both Kindlings and the original library's derivation?

Yes. Kindlings type classes extend their parent library's types (`KindlingsEncoder[A] extends Encoder[A]`, `KindlingsDecoder[A] extends Decoder[A]`, etc.). You can mix manually written instances with derived ones.

If both Kindlings' and the original library's automatic derivation are in scope, you may get ambiguous implicits. In that case, use semi-automatic derivation (`KindlingsEncoder.derived[A]`) to be explicit.

## How do I migrate from circe-generic?

1. Replace the dependency: `circe-generic` / `circe-generic-extras` with `kindlings-circe-derivation`
2. Replace imports: `io.circe.generic.auto._` or `io.circe.generic.semiauto._` with `hearth.kindlings.circederivation._`
3. Replace `deriveEncoder[A]` / `deriveDecoder[A]` with `KindlingsEncoder.derived[A]` / `KindlingsDecoder.derived[A]`
4. If using `@ConfiguredJsonCodec` or circe `Configuration`, switch to Kindlings' `Configuration` class (same builder API)

## How do I migrate from kittens?

1. Replace the dependency: `kittens` with `kindlings-cats-derivation`
2. Replace imports: `cats.derived._` / `cats.derived.semiauto.*` with `hearth.kindlings.catsderivation.extensions._`
   (this brings the `.derived` extension methods into scope), and `cats.derived.auto.show._` (etc.) with
   `hearth.kindlings.catsderivation.auto.show._` (Scala 3: `auto.show.given`)
3. For Scala 3 `derives` syntax: keep `derives cats.Show` as it is and import
   `hearth.kindlings.catsderivation.extensions.*` (Kindlings provides the `derived` method as an extension on the
   companion, so `derives` resolves to it)
4. For semi-automatic: replace `cats.derived.semiauto.show[A]` with `cats.Show.derived[A]`

## How do I migrate from jsoniter-scala macros?

1. Replace the dependency: `jsoniter-scala-macros` with `kindlings-jsoniter-derivation`
2. Replace `JsonCodecMaker.make[A]` with `KindlingsJsonValueCodec.derived[A]`
3. Replace `CodecMakerConfig` with `JsoniterConfig` (similar builder API)

## Which modules are JVM-only?

- `kindlings-avro-derivation` — depends on `org.apache.avro:avro` (JVM-only)
- `kindlings-pureconfig-derivation` — depends on `com.typesafe:config` (JVM-only)
- `kindlings-newtype-integration` — depends on `com.kubuszok:newtype-compat` (JVM-only)

All other modules are cross-compiled for JVM, Scala.js, and Scala Native.

## Which module is Scala 3-only?

- `kindlings-iron-integration` — Iron is a Scala 3-only library (opaque types)
- `kindlings-neotype-integration` — neotype is a Scala 3-only library (opaque types)

All other modules support both Scala 2.13 and Scala 3.

## Why does Scala 2.13 fail with "HearthAssertionError: MIO timeout is already set"?

A known Hearth bug (see `docs/research/hearth-bug-mio-timeout-reentry.md` in the repository): when a derivation reads
the subtypes of a sealed trait, the Scala 2 compiler may type another `val` of the same file whose type is inferred
from a derived instance, and so start that second derivation while the first is still running. Give the `val` (or
`def`) holding a derived instance an explicit type:

```scala
object Other { val encoder: Encoder[Http] = KindlingsEncoder.derived[Http] } // not `val encoder = ...`
```

## Why does Scala 3 crash with "missing outer accessor in class X"?

A known Hearth bug (see `docs/research/hearth-bug-enum-child-this-type-prefix.md` in the repository): deriving for a
sealed trait declared inside a **class** (a test suite, a `.sc` script) whose case object lives in the trait's
companion (`object Shape { case object Empty extends Shape }`) crashes the compiler. Declare the hierarchy at the top
level or inside an `object` (e.g. the suite's companion object) instead.

## Which parts of Kindlings end up in my application at runtime? Can I remove the rest from a fat JAR?

Kindlings modules are normal `Compile` dependencies, so all of their code ends up on your runtime classpath and in every
fat JAR (uberjar) that sbt-assembly, Maven Shade, Gradle Shadow, etc. build. Most of that code is macro
implementations, which run only inside the compiler while it expands macros. Your application never loads them. In
the published JVM jars, the compile-time-only packages are **over 80% of the bytes** (about 82% on Scala 3, 84% on
Scala 2.13).

!!! note "This only matters on the JVM"

    Scala.js and Scala Native linkers keep only the code that is reachable from your program, so the compile-time
    part of Kindlings is dropped from the output anyway.

The tables below list packages, not single classes, so they stay valid when modules grow. `*` stands for any one
package segment, usually a module's package (`circederivation`, `jsoniterderivation`, `parser`, ...). When several rows
match a package, the most specific one wins. Our CI checks two things. Every package of every published module must
match a row. Code that Kindlings' macros generate in our test suites must refer only to packages listed as used at
runtime (or in tests).

### Used at runtime: keep

| Package | What is there |
|---|---|
| `hearth.kindlings.*` | Everything not listed in the tables below: the public API (type classes, companions, configuration, extension methods), `jsoniterjson`, `tapiropenapijsoniter` (including its hand-written `internal` codecs), the parser runtime with its `catseffect` and `streams` engines, etc. |
| `hearth.kindlings.*.internal.runtime` | Helpers and factories that macro-generated code calls |
| `hearth.kindlings.*.annotations` | Annotations such as `@fieldName` or `@transientField`. The macros read them during compilation, but some generated code also creates them at runtime (e.g. a derived Tapir `Schema` creates jsoniter's `@named`, and Avro derivation creates `@avroSortPriority`) |

### Used at runtime, but probably only in tests

These modules are meant for tests. Add them with `% Test`, so they never get into a fat JAR. If one does, fix the
dependency scope instead of filtering it out.

| Package | Artifact | What is there |
|---|---|---|
| `hearth.kindlings.mock` | `kindlings-mock` | mocking library |
| `hearth.kindlings.scalacheckderivation` | `kindlings-scalacheck-derivation` | `Arbitrary` / `Gen` / `Shrink` / `Cogen` derivation |
| `hearth.kindlings.diffderivation` | `kindlings-diff-derivation` | structural diffs, mostly used in test assertions. Keep it in `Compile` if you compute diffs in production (e.g. for audit logs) |

The `internal.compiletime` packages of these modules are still compile-time only (see below).

### Used only during macro expansion: safe to remove

| Package / file | Artifact | Notes |
|---|---|---|
| `hearth.kindlings.*.internal.compiletime` (with subpackages such as `...internal.compiletime.rules`) | every derivation / macro module, and every integration module | the macro implementations - by far the largest part of Kindlings |
| `hearth.kindlings.derivation.compiletime` | `kindlings-derivation-commons` (whole artifact) | utilities shared by the derivation macros |
| `hearth.kindlings.macros.compiletime` | `kindlings-macro-commons` (whole artifact) | utilities shared by `di`, `mock` and `optics` |
| `hearth.kindlings.jsonschemaconfigs` | `kindlings-json-schema-config-macro-providers` (whole artifact) | lets Tapir Schema derivation find Circe / Jsoniter configuration |
| `META-INF/services/hearth.*` | integration modules, `kindlings-circe-derivation`, `kindlings-jsoniter-derivation` | macro extensions, which the compiler loads with `ServiceLoader` during macro expansion |
| `hearth.kindlings.*.debug`, `hearth.kindlings.*.policy` | every derivation / macro module | implicit markers that you import to [log](debugging.md) or [allow](derivation-policy.md) derivation. The macros look them up during compilation. They are tiny, so remove them only if your code never refers to the marker values (some code assigns one to a `val` to keep `-Wunused` quiet) |

`kindlings-refined-integration`, `kindlings-iron-integration`, `kindlings-neotype-integration` and
`kindlings-newtype-integration` contain nothing but compile-time code. `kindlings-cats-integration` also has a small
runtime part (`hearth.kindlings.catsintegration.internal.runtime`).

Kindlings depends on [Hearth](https://scala-hearth.readthedocs.io/), whose own packages and dependencies (on Scala 2.13
these include `scala-compiler`) are mostly compile-time only too. Hearth sorts them in the same way in
[its FAQ](https://scala-hearth.readthedocs.io/en/latest/faq/#which-parts-of-hearth-end-up-in-my-application-at-runtime-can-i-remove-the-rest-from-a-fat-jar).
The example below combines both lists.

!!! warning "Scala 3 multi-stage programming"

    With `scala.quoted.staging`, or Scala 2's `scala.tools.reflect.ToolBox`, the compiler runs inside your
    application. Macros are then expanded at runtime, and the "compile-time only" packages above become runtime
    dependencies. We have **not tested** whether Kindlings (or Hearth) works with staging. If you use it, keep all of
    Kindlings and Hearth in your fat JAR (and please tell us whether it works).

### Example: sbt-assembly

```scala
// build.sbt (sbt 1.x, sbt-assembly 2.x)
assembly / assemblyExcludedJars := {
  // Hearth: see Hearth's FAQ
  val compileTimeOnlyJars = Seq(
    "hearth-better-printers_",
    "hearth-cross-quotes_",
    "hearth-micro-fp_",
    // Scala 2.13 only - remove these lines if your application uses them itself
    "scala-compiler-",
    "jline-",
    "java-diff-utils-"
  )
  (assembly / fullClasspath).value.filter { jar =>
    compileTimeOnlyJars.exists(jar.data.getName.startsWith)
  }
}

assembly / assemblyMergeStrategy := {
  // Kindlings
  case PathList("hearth", "kindlings", _, "internal", "compiletime", _*)                 => MergeStrategy.discard
  case PathList("hearth", "kindlings", "derivation" | "macros", "compiletime", _*)       => MergeStrategy.discard
  case PathList("hearth", "kindlings", "jsonschemaconfigs", _*)                          => MergeStrategy.discard
  // uncomment if nothing in your code refers to the debug / policy marker values:
  // case PathList("hearth", "kindlings", _, "debug" | "policy", _*) => MergeStrategy.discard
  // Hearth: see Hearth's FAQ
  case PathList("hearth", "typed" | "untyped" | "std" | "debug", _*)                => MergeStrategy.discard
  // macro extensions of both Hearth and Kindlings
  case PathList("META-INF", "services", service) if service.startsWith("hearth.") => MergeStrategy.discard
  case other =>
    val defaultStrategy = (assembly / assemblyMergeStrategy).value
    defaultStrategy(other)
}
```

Run your application (or its integration tests) from the fat JAR afterwards. A `NoClassDefFoundError` that mentions
`hearth/kindlings/...` means that something does need the removed package at runtime. Put the package back, and
please report which class was missing.

## Do I need to import anything for Refined / Iron support?

No. Add the integration dependency to your build and derivation handles refined/iron types automatically:

```scala
// build.sbt
libraryDependencies += "com.kubuszok" %% "kindlings-refined-integration" % "{{ kindlings_version() }}"
// or for Iron (Scala 3 only):
libraryDependencies += "com.kubuszok" %% "kindlings-iron-integration" % "{{ kindlings_version() }}"
```

The macro extension system discovers the integration at compile time. No imports, no configuration.

## Where are Cats collection types like NonEmptyList supported?

Add the `kindlings-cats-integration` dependency. It provides `IsCollection` and `IsMap` providers for `NonEmptyList`, `NonEmptyVector`, `NonEmptySeq`, `NonEmptyLazyList`, `NonEmptyChain`, `Chain`, `NonEmptyMap`, and `NonEmptySet`. These are then automatically available to all derivation modules (Circe, Jsoniter, Avro, etc.).

```scala
libraryDependencies += "com.kubuszok" %% "kindlings-cats-integration" % "{{ kindlings_version() }}"
```
