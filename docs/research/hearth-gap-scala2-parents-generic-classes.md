# Hearth gap: `UntypedType.parents` is empty for applied generic classes on Scala 2

Status: **OPEN** (found 2026-10-26 with Hearth `0.4.3-17-gbd1e277-SNAPSHOT`, kubuszok/hearth#389, while replacing
di/di-cats' `fqcn.startsWith("scala.Function")` with `Type.isFunction`). Scala 2.13 only; Scala 3 is correct.

## What is wrong

`Type.isFunction` is true for function SUBTYPES (`List[A]` is an `Int => A`, `Map[K, V]` is a `K => V`). To keep
those as plain instances, di first checked "the type is a function, but none of its direct parents is":

```scala
Type.isFunction[D] && !UntypedType.dealias(UntypedType.fromTyped[D]).parents.exists(_.isFunction)
```

On Scala 3 this works. On Scala 2.13, `List[Plugin]` passed the check, so it was treated as a factory and
`DI.autowire[MakerHolder](maker, plugins)` failed with "cannot find a provided dependency ... for:
scala.collection.immutable.List[...Plugin]". The non-generic `class FinderMaker extends (DatabaseAccess => UserFinder)`
was classified correctly on both platforms.

## Likely cause

`UntypedTypesScala2.parents`:

```scala
instanceTpe.typeSymbol.typeSignatureIn(instanceTpe) match {
  case ci: ClassInfoTypeApi => ci.parents.map(_.asSeenFrom(instanceTpe, instanceTpe.typeSymbol))
  case _                    => Nil
}
```

For a polymorphic class the signature is a `PolyType(typeParams, ClassInfoType(...))`, not a `ClassInfoType`, so every
applied generic class (`List[A]`, `Map[K, V]`, `Function1[A, B]`, ...) gets `Nil`. Unwrapping the `PolyType`
(`resultType`) before matching, then substituting the type arguments, should fix it.

## Kindlings workaround

di (`WiringMacrosImpl.isFactoryType`) and di-cats (`ResourceWiringMacrosImpl.isFunction`) count function classes in
`baseClasses` instead: a type is exactly a function type when its linearization holds exactly one function class (its
own). Tests: `WiringSpec` "treats a value whose class extends a function type as an instance, not a factory",
`ResourceWiringSpec` "a value whose class extends a function type is an instance, not a factory".
