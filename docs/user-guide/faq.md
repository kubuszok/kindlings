# FAQ

## Why not contribute these improvements to Circe, Jsoniter Scala, etc.?

**Different foundations require different codebases.** Kindlings' derivation is built on [Hearth](https://github.com/kubuszok/hearth), a macro toolkit that provides high-level, cross-platform abstractions over Scala 2 and Scala 3 metaprogramming. Existing libraries use their own macro infrastructure (or Shapeless/Mirrors). Replacing the internals of a library with a completely different macro foundation is not a patch — it's a rewrite of the derivation layer.

**Independence allows faster iteration.** Kindlings can support new type categories (named tuples, opaque types, Java enums), experiment with better error messages, and optimize compilation speed without being blocked by the release cycle or design philosophy of upstream libraries.

**Maintaining cross-version compatibility is a design constraint.** Most libraries have separate Scala 2 and Scala 3 implementations with different capabilities. Kindlings shares a single derivation logic across both versions.

## What does "sanely-automatic" mean?

Sanely-automatic derivation means three things:

1. **Semi-automatic is recursive.** When you call `KindlingsEncoder.derive[Person]`, it derives instances for `Person` and all its nested types (`Address`, `List[Address]`, etc.) in a single macro expansion — no need to declare instances for each type manually.

2. **Automatic has no overhead over semi-automatic.** For a single derivation site, automatic and semi-automatic produce identical generated code — same compilation cost, same runtime performance. The generated code is as fast as what you'd write by hand.

3. **Errors are informative and actionable.** When derivation fails, you get a clear message telling you which type is missing an instance and where in the type hierarchy the problem is — not a cryptic `diverging implicit expansion`.

If the same type is auto-derived at multiple call sites, each site derives independently. This is still cheaper than Shapeless/Mirrors-based automatic derivation, but if you want to guarantee a type is derived exactly once, use semi-automatic (`KindlingsEncoder.derive[A]`) and assign it to an `implicit val` / `given`.

## Can I use both Kindlings and the original library's derivation?

Yes. Kindlings type classes extend their parent library's types (`KindlingsEncoder[A] extends Encoder[A]`, `KindlingsDecoder[A] extends Decoder[A]`, etc.). You can mix manually written instances with derived ones.

If both Kindlings' and the original library's automatic derivation are in scope, you may get ambiguous implicits. In that case, use semi-automatic derivation (`KindlingsEncoder.derive[A]`) to be explicit.

## How do I migrate from circe-generic?

1. Replace the dependency: `circe-generic` / `circe-generic-extras` with `kindlings-circe-derivation`
2. Replace imports: `io.circe.generic.auto._` or `io.circe.generic.semiauto._` with `hearth.kindlings.circederivation.*`
3. Replace `deriveEncoder[A]` / `deriveDecoder[A]` with `KindlingsEncoder.derive[A]` / `KindlingsDecoder.derive[A]`
4. If using `@ConfiguredJsonCodec` or circe `Configuration`, switch to Kindlings' `Configuration` class (same builder API)

## How do I migrate from kittens?

1. Replace the dependency: `kittens` with `kindlings-cats-derivation`
2. Replace imports: `cats.derived._` or `cats.derived.auto.*` with `hearth.kindlings.catsderivation.*`
3. For Scala 3 `derives` syntax: replace `derives cats.Show` with `derives cats.Show` (Kindlings provides the `derived` method as an extension, so `derives` just works)
4. For semi-automatic: replace `cats.derived.semiauto.show[A]` with `cats.Show.derived[A]`

## How do I migrate from jsoniter-scala macros?

1. Replace the dependency: `jsoniter-scala-macros` with `kindlings-jsoniter-derivation`
2. Replace `JsonCodecMaker.make[A]` with `KindlingsJsonValueCodec.derive[A]`
3. Replace `CodecMakerConfig` with `JsoniterConfig` (similar builder API)

## Which modules are JVM-only?

- `kindlings-avro-derivation` — depends on `org.apache.avro:avro` (JVM-only)
- `kindlings-pureconfig-derivation` — depends on `com.typesafe:config` (JVM-only)

All other modules are cross-compiled for JVM, Scala.js, and Scala Native.

## Which module is Scala 3-only?

- `kindlings-iron-integration` — Iron is a Scala 3-only library (opaque types)

All other modules support both Scala 2.13 and Scala 3.

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

Add the `kindlings-cats-integration` dependency. It provides `IsCollection` and `IsMap` providers for `NonEmptyList`, `NonEmptyVector`, `NonEmptyChain`, `Chain`, `NonEmptyMap`, and `NonEmptySet`. These are then automatically available to all derivation modules (Circe, Jsoniter, Avro, etc.).

```scala
libraryDependencies += "com.kubuszok" %% "kindlings-cats-integration" % "{{ kindlings_version() }}"
```
