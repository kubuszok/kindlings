---
name: hearth-hkt-derivation
description: >
  Polymorphic (kind * -> *) type class derivation with Hearth. Erased approach with AnyF,
  two-probe field classification (Int/String probes), bridge methods for type constructor
  summoning, runtime helpers for erased HKT calls, ConsK algorithm.
paths:
  - "**/compiletime/*Functor*.scala"
  - "**/compiletime/*Traverse*.scala"
  - "**/compiletime/*Foldable*.scala"
  - "**/compiletime/*Bifunctor*.scala"
  - "**/compiletime/*ConsK*.scala"
user-invocable: false
---

# Skill: Polymorphic (HKT) Type Class Derivation

Patterns for deriving type classes parameterized by a type constructor `F[_]` (e.g.,
`Functor[F]`, `Foldable[F]`, `ConsK[F]`). These differ significantly from monomorphic
derivation.

**Reference files:** `FunctorMacrosImpl.scala`, `ConsKMacrosImpl.scala`,
`ContravariantMacrosImpl.scala`, `ApplicativeMacrosImpl.scala`,
`NonEmptyAlternativeMacrosImpl.scala`

## The erased approach

Scala 2 macros report "free type variable" errors when method-level type parameters appear
in macro-generated expression trees. To work around this, all polymorphic derivations use
an **erased approach**:

1. Build the body for `F[Any]` using `Any` as the type parameter
2. Use `Any => Any` for function parameters (e.g., `map`'s `f: A => B` becomes `Any => Any`)
3. Cast with `asInstanceOf` at the boundaries

```scala
Expr.quote {
  new cats.Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      val anyFa: F[Any] = fa.asInstanceOf[F[Any]]
      val anyF: Any => Any = f.asInstanceOf[Any => Any]
      // ... body works entirely with F[Any] and Any ...
      Expr.splice(doMap(Expr.quote(anyFa), Expr.quote(anyF))).asInstanceOf[F[B]]
    }
  }
}
```

This is safe because JVM erasure means `F[A]` and `F[Any]` have the same runtime
representation.

**Reference:** `FunctorMacrosImpl.scala`, `PureMacrosImpl.scala`

## Two-probe field classification

To determine how each field relates to the type parameter `A` in `F[A]`, parse the case
class at two different type applications and compare field types:

```scala
val ccInt = CaseClass.parse(using FCtor.apply[Int])
val ccString = CaseClass.parse(using FCtor.apply[String])

fieldsInt.zip(fieldsString).foreach { case ((name, pInt), (_, pString)) =>
  val tInt = pInt.tpe.Underlying
  val tString = pString.tpe.Underlying
  if (tInt =:= IntType && tString =:= StringType) {
    // DIRECT: field type IS the type parameter A
  } else if (tInt =:= tString) {
    // INVARIANT: field type doesn't depend on A (e.g., String, Int)
  } else {
    // NESTED: field contains A but isn't A directly (e.g., List[A], A => Boolean)
  }
}
```

**Field classification summary:**

| Category | In `F[Int]` | In `F[String]` | Example | Treatment |
|---|---|---|---|---|
| Direct | `Int` | `String` | `value: A` | Maps/transforms directly |
| Invariant | `String` | `String` | `label: String` | Copy unchanged or combine |
| Nested | `List[Int]` | `List[String]` | `items: List[A]` | Recurse or summon |

**Reference:** `FunctorMacrosImpl.scala` (direct + invariant), `ConsKMacrosImpl.scala`
(all three)

## Preserving field types for `primaryConstructor`

`CaseClass.primaryConstructor(Map[String, Expr_??])` performs strict `<:<` subtype checks.
Always preserve the original field type:

```scala
fields.foreach { case (fieldName, fieldValue) =>
  import fieldValue.Underlying as Field
  val fieldExpr = fieldValue.value.asInstanceOf[Expr[Field]]

  if (directFields.contains(fieldName)) {
    resultFields += ((fieldName, carriedExpr.as_??))
  } else if (needsTransform) {
    val transformed: Expr[Field] = Expr.quote {
      someTransform(Expr.splice(fieldExpr.upcast[Any])).asInstanceOf[Field]
    }
    resultFields += ((fieldName, transformed.as_??))
  } else {
    resultFields += ((fieldName, fieldExpr.as_??))
  }
}
```

The `.asInstanceOf[Field]` inside `Expr.quote` works because `import fieldValue.Underlying
as Field` brings an implicit `Type[Field]` into scope.

**Reference:** `ConsKMacrosImpl.scala` -- `deriveConsKBody`

## Bridge method pattern for type constructor summoning

When a nested field like `List[A]` requires summoning a type class for its type constructor
(e.g., `ConsK[List]`), you can't construct `Type[ConsK[List]]` in shared code because
`ConsK` takes `F[_]` (kind `* -> *`) and Hearth's `Type.Ctor1` handles kind `*` only.

**Solution:** Define an abstract method in the shared trait, implemented in each bridge:

```scala
// Shared trait (ConsKMacrosImpl.scala):
protected def summonConsKForFieldType(fieldType: Type[Any]): Option[Expr[Any]]
```

**Scala 3 bridge:**
```scala
protected def summonConsKForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
  import q.reflect.*
  val repr = TypeRepr.of(using fieldType.asInstanceOf[scala.quoted.Type[Any]])
  repr.dealias match {
    case AppliedType(fieldCtor, _ :: Nil) =>
      val consKCtor = TypeRepr.of[alleycats.ConsK[List]] match {
        case AppliedType(ctor, _) => ctor
      }
      Implicits.search(consKCtor.appliedTo(fieldCtor)) match {
        case iss: ImplicitSearchSuccess => Some(iss.tree.asExpr.asInstanceOf[Expr[Any]])
        case _ => None
      }
    case _ => None
  }
}
```

**Scala 2 bridge:**
```scala
protected def summonConsKForFieldType(fieldType: Type[Any]): Option[Expr[Any]] = {
  val tpe = fieldType.asInstanceOf[c.WeakTypeTag[Any]].tpe
  val ctor = tpe.typeConstructor
  val consKCtor = c.universe.weakTypeOf[alleycats.ConsK[List]].typeConstructor
  c.inferImplicitValue(c.universe.appliedType(consKCtor, List(ctor))) match {
    case c.universe.EmptyTree => None
    case tree => Some(c.Expr[Any](tree).asInstanceOf[Expr[Any]])
  }
}
```

The trick: use a concrete type like `ConsK[List]` and destructure it to get the `ConsK`
type constructor, then re-apply it to the field's type constructor.

**Reference:** `ConsKMacros.scala` (both Scala 2 and Scala 3 bridges)

## Runtime helper pattern for erased HKT calls

When macro-generated code needs to call a method on a type class with higher-kinded
parameters (e.g., `ConsK[G].cons(hd, tl)` where `G` is not known statically), wrap the
call in a runtime helper:

```scala
// runtime/ConsKRuntime.scala:
object ConsKRuntime {
  def cons(consKInstance: Any, hd: Any, tl: Any): Any =
    consKInstance.asInstanceOf[alleycats.ConsK[List]].cons[Any](hd, tl.asInstanceOf[List[Any]])
}
```

At JVM runtime, `ConsK[List]` and `ConsK[Vector]` are both just `ConsK` (erasure), so
the cast to `ConsK[List]` is a no-op.

The macro-generated code calls the helper:
```scala
Expr.quote {
  ConsKRuntime.cons(Expr.splice(consKExpr), Expr.splice(carried),
    Expr.splice(fieldExpr.upcast[Any])).asInstanceOf[Field]
}
```

**Reference:** `ConsKRuntime.scala`, `ConsKMacrosImpl.scala`

## ConsK carry-and-absorb algorithm

For `ConsK[F]` derivation (`cons[A](hd: A, tl: F[A]): F[A]`), walk fields left-to-right
with a "carried" element:

1. Initialize carried = `hd`
2. For each field:
   - **Direct** (`A`): shift -- field gets carried, old value becomes new carried
   - **Nested with ConsK** (`G[A]`): absorb -- `ConsK[G].cons(carried, tl.field)`, carried cleared
   - **Invariant or non-absorbable**: copy from `tl`
3. After all fields: error if carried not absorbed

This handles common patterns:
- `ListWrap[A](items: List[A])` -- cons directly into items
- `NEL[A](head: A, tail: List[A])` -- shift head, cons displaced head into tail
- `NamedList[A](items: List[A], name: String)` -- cons into items, copy name

**Reference:** `ConsKMacrosImpl.scala` -- `deriveConsKBody`

## Composed type class derivation

Some type classes combine multiple interfaces (e.g., `NonEmptyAlternative` = `Applicative`
\+ `SemigroupK`). Rather than delegating to existing derived instances, derive each method
body independently to avoid runtime overhead:

```scala
// NonEmptyAlternativeMacrosImpl generates bodies for:
// - pure(a): from Applicative pattern (direct fields = a, invariant = Monoid.empty)
// - map(fa)(f): from Functor pattern (apply f to direct fields)
// - ap(ff)(fa): from Applicative pattern (apply function field, Semigroup.combine invariant)
// - combineK(x, y): from SemigroupK pattern (Semigroup.combine all fields)
```

This generates a single anonymous class with all methods, rather than composing at runtime.

**Reference:** `NonEmptyAlternativeMacrosImpl.scala`, `AlternativeMacrosImpl.scala`

## Related skills

- [hearth-macro-basics](../hearth-macro-basics/SKILL.md) -- core architecture
- [hearth-case-class-rules](../hearth-case-class-rules/SKILL.md) -- monomorphic case class derivation
- [hearth-enum-rules](../hearth-enum-rules/SKILL.md) -- enum derivation (used for sealed trait HKT)
- [hearth-cross-compilation](../hearth-cross-compilation/SKILL.md) -- Function1 unapply, erased approach pitfalls
- [hearth-def-caching](../hearth-def-caching/SKILL.md) -- multi-method caching with parTuple
