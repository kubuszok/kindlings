# Kindlings

Type class derivation that compiles faster, runs faster, and works the same on Scala 2.13 and Scala 3. Drop-in replacements for derivation in Circe, Jsoniter Scala, Avro, and more — built on Hearth, powered by macros, free of the trade-offs you've learned to accept.

## Why Kindlings?

Most Scala libraries derive type class instances using Shapeless (Scala 2), Scala 3 Mirrors, or Magnolia. These approaches work, but they come with trade-offs that compound as your project grows: slow compilation, poor error messages, runtime overhead from intermediate representations, and API fragmentation between Scala 2 and Scala 3.

Kindlings takes a different path. Built on [Hearth](https://github.com/MateuszKubuszok/hearth), it uses macros to generate code that is closer to what you'd write by hand — while providing a better developer experience than any of the alternatives.

### One API across Scala 2.13 and Scala 3

No conditional imports, no platform-specific code. Your derivation calls look the same regardless of the Scala version. Migration between Scala 2.13 and Scala 3 requires zero changes to your derivation code.

### Automatic derivation without the usual cost

Traditional automatic derivation (à la `import io.circe.generic.auto._`) is notorious for slowing down compilation: the compiler re-derives instances at every use site, doing redundant work over and over. Semi-automatic derivation avoids this by caching instances as `given`/`implicit` vals — but at the cost of boilerplate.

Kindlings' **sanely-automatic** derivation gives you the best of both worlds. When a type is used in multiple places, the instance is derived once and reused. When a type is used in only one place, the derivation is effectively free — no different from having written it inline. The result: automatic-like convenience with semi-automatic-like performance.

### Inlineable derivation

Need maximum performance? Kindlings lets you inline derivation directly at the call site, eliminating all abstraction overhead. The generated code is indistinguishable from what you'd write by hand.

### Broad type support out of the box

Kindlings handles the types you actually use in real projects, without workarounds or extra configuration:

- Case classes and sealed traits
- Scala 3 enums and Java enums
- Named tuples and opaque types
- Scala and Java collections
- `Array` and `IArray`
- **Recursive data types** — no lazy wrappers, no manual knot-tying, no tricks. It just works.

### Better compilation errors

When derivation fails, you shouldn't have to decode `diverging implicit expansion` or `no implicit argument of type Encoder[???] was found`. Kindlings produces clear, actionable error messages that tell you exactly which type is missing an instance and where in the type hierarchy the problem occurs.

### Generated code preview

Curious what the macro actually produces? Kindlings lets you inspect the generated code, making debugging and optimization straightforward instead of a black-box guessing game.

### Faster compilation, faster runtime

Kindlings aims to compile faster and run faster than derivation based on Shapeless, Mirrors, or Magnolia. There are no intermediate `HList`/`Coproduct` representations, no implicit search chains to resolve at compile time, and no runtime allocations for generic wrappers.

The one exception: [Jsoniter Scala](https://github.com/plokhotnyuk/jsoniter-scala) already generates highly optimized code via its own macros. For Jsoniter, we aim for parity — the same runtime speed, with the added benefits of cross-version compatibility and a unified API.

### Comparison at a glance

| | Kindlings | Shapeless / Mirrors | Magnolia | Library-specific macros |
|---|---|---|---|---|
| Same API on Scala 2.13 and 3 | ✅ | ❌ | ❌ | varies |
| Auto derivation without overhead | ✅ | ❌ | ❌ | varies |
| Inline derivation | ✅ | ❌ | ❌ | some |
| Recursive types (no tricks) | ✅ | needs `Lazy` / workarounds | ✅ | varies |
| Clear error messages | ✅ | ❌ | partial | varies |
| Code preview | ✅ | ❌ | ❌ | rare |
| Named tuples, opaque types | ✅ | ❌ | ❌ | rare |
| Scala 3 enums, Java enums | ✅ | partial | partial | varies |

> For a deeper dive into why mainstream derivation approaches are suboptimal and how sanely-automatic derivation addresses their shortcomings, see [Sanely-automatic derivation](https://kubuszok.com/2025/sanely-automatic-derivation/).

## Why a separate project?

A natural question: why not contribute these derivation improvements directly to Circe, Jsoniter Scala, and other libraries?

**Different foundations require different codebases.** Kindlings' derivation is built on [Hearth](https://github.com/MateuszKubuszok/hearth), a macro toolkit that provides high-level, cross-platform abstractions over Scala 2 and Scala 3 metaprogramming. Existing libraries use their own macro infrastructure (or Shapeless/Mirrors). Replacing the internals of a library with a completely different macro foundation is not a patch — it's a rewrite of the derivation layer, with different trade-offs, different error handling, and different supported types. That's not something you sneak into a PR.

**Maintaining cross-version compatibility is a design constraint, not a feature flag.** Most libraries have separate Scala 2 and Scala 3 implementations with different capabilities and sometimes different APIs. Kindlings is built from the ground up to share a single API and a single derivation logic across both versions. Retrofitting this onto a library that was designed differently would mean restructuring their entire build and module layout.

**Independence allows faster iteration.** Kindlings can support new type categories (named tuples, opaque types, Java enums), experiment with better error messages, and optimize compilation speed without being blocked by the release cycle or design philosophy of upstream libraries. If an approach proves successful here, it can inform upstream improvements — but it doesn't have to wait for consensus to ship.

**Dogfooding Hearth in production conditions.** Kindlings serves as a real-world stress test for Hearth's abstractions. If Hearth can't handle the complexity of deriving codecs for Circe, Avro, or Jsoniter Scala, that's a bug to fix before anyone else hits it. Every corner case discovered here makes Hearth more robust for all its users.

**Learning by example.** Kindlings doubles as a reference for how to use Hearth in practice — complete, tested derivation implementations that you can study, rather than piecing together usage from API docs alone.
