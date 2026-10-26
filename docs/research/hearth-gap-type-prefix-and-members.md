# Hearth gap: no API for a type's prefix, its type members, or a type symbol's owner

Status: **RESOLVED** by kubuszok/hearth#389 (`Type.pseudoCompanionObject`, Hearth `0.4.3-17-gbd1e277-SNAPSHOT`, to be
released in 0.4.4). Found 2026-10-25 with Hearth 0.4.3 while moving `newtype-integration` and `neotype-integration` off
raw reflection.

## Resolution

Hearth did not add the generic operations listed under "Wanted" (no type-member-by-name lookup, no owner name). It added
one targeted API instead: `Type.pseudoCompanionObject[A]: Option[Expr_??]` (also `Type[A].pseudoCompanionObject` and
`UntypedType.pseudoCompanionObject: Option[(UntypedType, UntypedExpr)]`). It returns the type's real companion when it
has one, and otherwise the object whose abstract/opaque type member `A` is (after dealiasing): `UserName` for both
`UserName` and `UserName.Type`. That covers operation 1 (the prefix). Operations 2-4 are replaced by reading the
object's members through existing APIs, which also proves that `A` really is the newtype's `Type`:

- **neotype**: `pseudoCompanionObject`, then `Type.Ctor1.of[neotype.Newtype]`/`Subtype` `.unapply` on the object's
  type gives the underlying type. The "declared in `neotype.Newtype`" owner check (operation 4) is replaced by
  requiring `make`'s result type (`Method.ApplyValues.knownReturning` after applying the companion) to be
  `Either[String, A]`. `NeotypeReflection` is gone; the provider no longer matches on `MacroCommonsScala3`.
- **scala-newtype**: `pseudoCompanionObject`, then the companion's `unsafeUnwrap` member through the `Method` API
  (`fold`, applying `A`'s type arguments for a generic newtype). Its result type is `Coercible[Type, Repr]`, so
  `Type.Ctor2.of[Coercible].unapply` gives `Repr` (operations 2 and 3) and `from =:= A` proves the match. Both encodings
  always generate `unsafeUnwrap`: the Scala 2 macro annotation (estatico 0.4.4) and the scala-newtype-compat plugin on
  Scala 3. `apply` was not used because the Scala 2 macro generates it only for a `case class`. Both
  `NewtypeReprPlatform` files (`scala-2`/`scala-3`) are deleted; the provider is shared code.

Verified with `integrationTests2_13/testFull` (148 passed) and `integrationTests/testFull` (222 passed), including all
`Newtype*Spec` (monomorphic, `@newsubtype`, generic `NewtypeTags[A]`, user-provided instances) and `Neotype*Spec`. A
mutation check (forcing `reprOf` to `None`) makes the `Newtype*Spec` derivations fail to compile, so the new lookup is
what makes them pass.

Nothing remains open for these two integrations. The "Wanted" API below would still be needed by an encoding that has
no member whose signature names both the newtype and its wrapped type.

## What is missing

Both integrations have to recognize a **path-dependent type member** `Foo.Type` of a companion `object Foo`. That
needs four operations Hearth's `Type`/`UntypedType` API does not expose on either platform:

| # | operation | Scala 3 reflection used today | Scala 2 reflection used today |
|---|---|---|---|
| 1 | the **prefix** (qualifier) of a type reference: `Foo.type` from `Foo.Type` | `TypeRef.qualifier` | `TypeRef(prefix, _, _)` |
| 2 | look up a **type member** by name on a type: `Foo.Repr`, `Foo.Base`, `Foo.Tag` | `Symbol.typeMember(name)` | `Type.member(TypeName(name))` |
| 3 | **select** that member on the prefix and apply type arguments: `Foo.Repr[Args]` | `prefix.select(sym).appliedTo(args)` | `internal.typeRef(prefix, sym, args)` |
| 4 | the **owner** (declaring class) of the type's symbol: is `Type` declared in `neotype.Newtype`? | `typeSymbol.owner.fullName` | (not needed) |

The nearest existing APIs do not cover them:

- `Type.companionObject[A]` looks up `A`'s *companion* (`sym.companionModule`, or a same-named module next to an opaque
  type). For `Foo.Type` it searches for a module called `Type`, so it returns `None`; the object we need is the
  *prefix*, which is not a companion.
- `Type.opaqueUnderlyingType[A]` loops through the **whole** chain of opaque types (`UntypedTypesScala3.scala:170-191`).
  neotype needs exactly one level, so that a neotype over another neotype keeps the inner `validate`. It also cannot
  help with scala-newtype, where `Foo.Type` is an abstract type, not an opaque one.
- `typeArguments`, `typeConstructor`, `baseClasses`, `parents`, `fqcn`, `shortName` and `isOpaqueType` all describe
  the type itself, never its prefix or its members.

## Reproducer

Compiled inside `newtype-integration` (any `this: MacroCommons =>` trait), Hearth 0.4.3:

```scala
trait GapProbe { this: MacroCommons =>
  def probe[A: Type]: Unit = {
    val u = UntypedType.fromTyped[A]
    val prefix = u.prefix                                  // `Foo` of `Foo.Type`
    val member = u.typeMember("Repr")                      // `Foo.Repr`
    val owner = u.owner                                    // declaring class of `Type`
    val selected = UntypedType.select(prefix, "Repr", Nil) // `Foo.Repr[args]`
    ()
  }
}
```

Scala 3.8 (`sbt --client newtypeIntegration/compile`):

```
[error] 10 |    val prefix = u.prefix // `Foo` of `Foo.Type`
[error]    |               value prefix is not a member of GapProbe.this.UntypedType
[error] 11 |    val member = u.typeMember("Repr") // `Foo.Repr`
[error]    |           value typeMember is not a member of GapProbe.this.UntypedType
[error] 12 |    val owner = u.owner // declaring class of `Type`
[error]    |                value owner is not a member of GapProbe.this.UntypedType
[error] 13 |    val selected = UntypedType.select(prefix, "Repr", Nil) // `Foo.Repr[args]`
[error]    |         value select is not a member of GapProbe.this.UntypedTypeModule
[error] four errors found
```

Scala 2.13 (`sbt --client newtypeIntegration2_13/compile`):

```
[error] GapProbe.scala:10:20: value prefix is not a member of GapProbe.this.UntypedType
[error] GapProbe.scala:11:20: value typeMember is not a member of GapProbe.this.UntypedType
[error] GapProbe.scala:12:19: value owner is not a member of GapProbe.this.UntypedType
[error] GapProbe.scala:13:32: value select is not a member of GapProbe.this.UntypedTypeModule
```

## Raw reflection this forced (before kubuszok/hearth#389)

- `newtype-integration/src/main/scala-2/hearth/kindlings/newtypeintegration/internal/compiletime/NewtypeReprPlatform.scala:13-21`:
  operations 1, 2 and 3 (`TypeRef(prefix, sym, args)`, `prefix.member(TypeName("Repr"|"Base"|"Tag"))`,
  `internal.typeRef(prefix, repr, args)`). It also checks that `Type` is abstract and that the prefix is a module.
- `newtype-integration/src/main/scala-3/hearth/kindlings/newtypeintegration/internal/compiletime/NewtypeReprPlatform.scala:15-33`:
  the same on Scala 3 (`typeRef.qualifier`, `companion.typeMember(...)`, `prefix.select(repr).appliedTo(args)`), and
  checks the `Deferred`/`Opaque`/`Module` flags.
- `neotype-integration/src/main/scala/hearth/kindlings/neotypeintegration/internal/compiletime/IsValueTypeProviderForNeotype.scala`,
  class `NeotypeReflection`: operations 1 and 4 only (`tpe.qualifier`, `typeSymbol.owner.fullName`). The rest of the
  neotype provider now uses Hearth. The underlying type comes from `Type.Ctor1.of[neotype.Newtype]` /
  `Type.Ctor1.of[neotype.Subtype]` `.unapply(Foo.type)`, the companion `Expr` from `Expr.singletonOf[Foo.type]`, and
  the `make` call from `Class[Foo.type].method("make")` → `OnInstance.apply` → `ApplyValues.apply` → `Result.build`.

## Wanted

On `UntypedType` (with typed forwarders on `Type`), on both platforms:

```scala
/** `p.T` / `p.T[args]` / `C#T` -> the prefix `p.type` / `C`; `None` for a type with no meaningful prefix. */
def prefix: Option[UntypedType]

/** Names of the type members (abstract, alias, opaque, class) declared in or inherited by this type. */
def typeMemberNames: List[String]

/** `this.Name[args]`, i.e. type member `Name` selected on this type as a prefix (as seen from it), if it exists. */
def selectTypeMember(name: String, args: List[UntypedType] = Nil): Option[UntypedType]

/** Fully qualified name of the class/trait/object that declares this type's symbol (`neotype.Newtype` for `Foo.Type`). */
def declaringTypeName: Option[String]

/** Whether the type's symbol is an abstract type member (`type T <: B`), as opposed to an alias, opaque type or class. */
def isAbstractTypeMember: Boolean
```

With those, `NewtypeReprPlatform` (both files) collapses into shared code:
`tpe.prefix.filter(_.isObject).flatMap(p => p.selectTypeMember("Repr", tpe.typeArguments))`, with
`typeMemberNames` used to check for `Base`/`Tag`. `NeotypeReflection` becomes
`tpe.prefix.filter(_ => tpe.declaringTypeName.exists(neotypeBaseNames))`.
