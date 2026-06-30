# Derivation Policy

By default Kindlings derivation is **always allowed**: wherever an instance of a Kindlings type class is needed, the
macro will derive one (recursively, in a single expansion). This is the [sanely-automatic](faq.md#what-does-sanely-automatic-mean)
behavior, and for most projects it is exactly what you want.

Some teams, however, want tighter control over **where** instances come into existence — for example, to ensure every
canonical codec is defined in one designated place rather than materialized ad-hoc at a random call site. The
**derivation policy** provides this without introducing a separate "semi-automatic" API or making recursion optional
(see [Why not separate automatic and semi-automatic derivation?](faq.md#why-not-separate-automatic-and-semi-automatic-derivation)).

The policy is a **compile-time, per-library switch** configured entirely through `-Xmacro-settings`. It gates only
**structural derivation** — generating an instance for your `case class`es and `sealed` hierarchies / `enum`s.
Everything else keeps working unconditionally:

- a type that already has an instance in implicit scope (your hand-written `given`/`implicit`),
- built-in/primitive support, collections, `Option`, value types / newtypes, named tuples.

In other words, the policy never blocks "the parts that always work" — it only decides whether the macro is allowed to
**structurally derive a new instance for a product/sum type at this location**.

## Modes

`<namespace>.policy.enabled` takes one of:

| Value | Behavior |
| --- | --- |
| `always-allowed` *(default)* | Current behavior — structural derivation is permitted everywhere. |
| `opt-in` | Structural derivation is permitted only in **allowed scopes**, or behind an **opt-in import** (see below). Everywhere else it is a compile error with an actionable message. |

The `<namespace>` is per library (the same namespace already used for the [derivation timeout](#per-library-namespaces)).
Each library is configured independently, so you can lock down (say) your JSON codecs while leaving `Show`/`Eq`
derivation wide open.

## Configuration

```text
# Turn the policy on for a library (here: FastShowPretty):
-Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in

# Allow structural derivation in these scopes (packages, objects, or classes — by fully-qualified name).
# A package entry covers everything nested under it; an object/class entry covers its inner derivations.
-Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=com.acme.json;com.acme.Codecs

# Whether importing the opt-in marker also permits derivation in the importing scope (default: true).
-Xmacro-settings:fastShowPrettyDerivation.policy.optInByImport=true
```

!!! warning "Separate `allowedScopes` entries with `;` or `|`, never `,`"
    A comma is **not** portable across compilers: Scala 3 splits a single `-Xmacro-settings:a,b` option on commas into
    two settings, while Scala 2 keeps it as one string. Kindlings therefore splits `allowedScopes` on `;` or `|`, which
    neither compiler touches. (If you prefer, you can also pass several separate `-Xmacro-settings:` options.)

In sbt:

```scala
Compile / scalacOptions ++= Seq(
  "-Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in",
  "-Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=com.acme.json;com.acme.Codecs"
)
```

## How a location is decided

When the macro is about to structurally derive an instance, it checks, **once per expansion**:

1. If `enabled` is `always-allowed` (the default) → **allowed**.
2. Otherwise (`opt-in`): if any enclosing scope (package / object / class) of the expansion point matches an
   `allowedScopes` entry → **allowed**.
3. Otherwise, if `optInByImport=true` **and** the library's opt-in marker is in implicit scope → **allowed**.
4. Otherwise → **compile error**, explaining the current location, the configured scopes, and every way to fix it.

Because a derivation is one macro expansion at one call site, this single decision covers the whole recursive
derivation: if the location is allowed, all nested product/sum types derive too (you define the instance once, in an
allowed place); if it is denied, the macro fails fast before generating anything.

### Opt-in by import

When `optInByImport=true`, importing the library's marker permits derivation in the current scope even if it is not on
`allowedScopes`:

```scala
import hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty

// now FastShowPretty.derived[...] is allowed in this scope
```

Set `optInByImport=false` if you want `allowedScopes` to be the *only* way in.

## Worked example

Build configuration (`opt-in`, one allowed package):

```text
-Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in
-Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=com.acme.show
```

```scala
package com.acme.show

import hearth.kindlings.fastshowpretty.FastShowPretty

final case class User(id: Long, name: String)

object Instances {
  // ✓ Allowed: this object is under the allowed package `com.acme.show`.
  given FastShowPretty[User] = FastShowPretty.derived
}
```

```scala
package com.acme.web

import hearth.kindlings.fastshowpretty.FastShowPretty
import com.acme.show.User

object Handler {
  // ✗ Compile error: `com.acme.web` is not an allowed scope and the opt-in marker is not imported.
  // val show = FastShowPretty.derived[User]

  // ✓ Use the canonical instance defined in the allowed scope instead:
  import com.acme.show.Instances.given
  def render(u: User): String = summon[FastShowPretty[User]].render(new StringBuilder, /*config*/ ???, 0)(u).toString
}
```

The error you would get from the commented-out line looks like:

```text
FastShowPretty derivation of com.acme.show.User is not allowed at this location.

This build enables an opt-in derivation policy (-Xmacro-settings:fastShowPrettyDerivation.policy.enabled=opt-in),
which only permits automatic (structural) derivation of case classes and sealed hierarchies in designated scopes.

  Location:         com.acme.web.Handler (at Handler.scala:...)
  Allowed scopes:   com.acme.show
  Opt-in by import: enabled

To fix this, do ONE of the following:
  1. Define the instance in an allowed scope and import/inherit it here, e.g.:
       given FastShowPretty[com.acme.show.User] = FastShowPretty.derived
  2. Add this location to the allowed scopes (separate entries with ';' or '|', never ','):
       -Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=<package-or-object>[;<more>]
  3. Import the opt-in marker into this scope (opt-in-by-import is enabled):
       import hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty
  4. Disable the policy for this build:
       -Xmacro-settings:fastShowPrettyDerivation.policy.enabled=always-allowed
```

## Per-library namespaces

The policy keys live under each library's settings namespace — the same one used for its
[derivation timeout](debugging.md). The opt-in marker is imported from that library's `policy` package.

| Library | Namespace | Opt-in marker import (`hearth.kindlings.…`) |
| --- | --- | --- |
| FastShowPretty | `fastShowPrettyDerivation` | `fastshowpretty.policy.allowDerivationForFastShowPretty` |
| Circe | `circeDerivation` | `circederivation.policy.allowDerivationForCirceDerivation` |
| Jsoniter Scala | `jsoniterDerivation` | `jsoniterderivation.policy.allowDerivationForJsoniterDerivation` |
| UBJson | `ubjsonDerivation` | `ubjsonderivation.policy.allowDerivationForUbjsonDerivation` |
| Diff | `diffDerivation` | `diffderivation.policy.allowDerivationForDiffDerivation` |
| YAML | `yamlDerivation` | `yamlderivation.policy.allowDerivationForYamlDerivation` |
| XML | `xmlDerivation` | `xmlderivation.policy.allowDerivationForXmlDerivation` |
| sconfig | `sconfigDerivation` | `sconfigderivation.policy.allowDerivationForSconfigDerivation` |
| PureConfig | `pureconfigDerivation` | `pureconfigderivation.policy.allowDerivationForPureconfigDerivation` |
| Avro | `avroDerivation` | `avroderivation.policy.allowDerivationForAvroDerivation` |
| Tapir Schema | `tapirSchemaDerivation` | `tapirschemaderivation.policy.allowDerivationForTapirSchemaDerivation` |
| ScalaCheck | `scalacheckDerivation` | `scalacheckderivation.policy.allowDerivationForScalacheckDerivation` |
| Cats | `catsDerivation` | `catsderivation.policy.allowDerivationForCatsDerivation` |
| Cats Tagless | `catsTaglessDerivation` | `catstaglessderivation.policy.allowDerivationForCatsTaglessDerivation` |

All derivation modules use the identical `<namespace>.policy.*` keys (`enabled`, `allowedScopes`, `optInByImport`) and
expose their opt-in marker from the module's `policy` package. Each library is configured independently — locking down
your JSON codecs does not affect `Show`/`Eq` derivation, and vice versa.
