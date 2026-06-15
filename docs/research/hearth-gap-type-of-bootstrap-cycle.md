# Hearth gap: `Type.of[A]` bootstrap cycle inside StandardMacroExtension contexts

> **UNDER RE-VERIFICATION against Hearth `0.3.1-45-gca3bcdb` (2026-06-15).** Unlike sibling
> gaps #283/#284 (both confirmed resolved upstream), this one is NOT yet confirmed: `UntypedType.toTyped[A]`
> exists but the workaround still needs an `asInstanceOf` bridge to obtain the `UntypedType`, and
> whether cross-quotes `Type.of[A]` still self-cycles inside a `StandardMacroExtension` can only be
> settled by removing the bypass in one extension and recompiling. That check is pending; treat the
> "open" status below as provisional until then. Original report below.

Last updated: 2026-06-12. Status: **open (provisional — pending an empirical recheck, see banner).**
Filed as [kubuszok/hearth#285](https://github.com/kubuszok/hearth/issues/285).

## Problem

Cross-quotes `Type.of[A]` generates code that resolves an implicit/given `Type[A]`. In
most macro code that's fine — the implicit comes from cross-quotes itself. But inside a
`StandardMacroExtension` (e.g. a `JsonFieldConfig` discovery extension) the very value
being defined *is* the implicit candidate in scope, so `Type.of[Configuration]` resolves
to the definition currently being elaborated — an inescapable self-referential cycle
(infinite loop / "recursive value" at expansion).

There is no Hearth-provided way to obtain a `Type[A]` that bypasses implicit resolution.

## Impact in Kindlings

Four files exist only to host this workaround (their annotation-access code is now on the
native Hearth API — #283 is resolved — so only the `Type.of` bypass below remains):

- `circe-derivation/src/main/scala-2/.../CirceJsonFieldConfigExtension.scala` (~102 lines)
- `circe-derivation/src/main/scala-3/.../CirceJsonFieldConfigExtension.scala` (~90 lines)
- `jsoniter-derivation/src/main/scala-2/.../JsoniterJsonFieldConfigExtension.scala` (~99 lines)
- `jsoniter-derivation/src/main/scala-3/.../JsoniterJsonFieldConfigExtension.scala` (~90 lines)

## Reproducer / current workaround

Scala 2 — bypass via raw `typeOf` + cast:

```scala
implicit val ConfigT: Type[Configuration] =
  UntypedType.toTyped[Configuration](
    sc2.c.universe.typeOf[Configuration].asInstanceOf[UntypedType]
  )
```

Scala 3 — bypass via `scala.quoted.Type.of` + cast:

```scala
given scala.quoted.Quotes = sc3.quotes
implicit val ConfigT: Type[Configuration] =
  scala.quoted.Type.of[Configuration].asInstanceOf[Type[Configuration]]
```

Both casts are unchecked bridges between the compiler's `Type`/`WeakTypeTag` and
Hearth's `Type` — exactly the kind of `asInstanceOf` Hearth exists to make unnecessary.

## Proposed Hearth API

Any one of:

1. `Type.ofDirect[A]` (name negotiable): same as `Type.of[A]` but generated code does
   NOT go through implicit resolution — it always materializes from the literal type
   argument. Safe to call where a `Type[A]` given/implicit is being defined.
2. Cross-quotes detection: when `Type.of[A]` expansion would resolve to the enclosing
   definition itself, fall back to direct materialization instead of looping.
3. At minimum: documented, supported `UntypedType.fromLiteral[A]`-style constructor so
   the workaround doesn't need `asInstanceOf` across the typed/untyped boundary.

## Reference

- `CLAUDE.md` § "Cross-Compilation Pitfalls" → "`Type.of[A]` bootstrap cycle in
  extensions".
- The four extension files listed above; each contains a comment block headed
  "Bootstrap Type values by bypassing cross-quotes Type.of".
