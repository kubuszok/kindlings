# Hearth gap: on Scala 2, the members of a self-typed enclosing class are never `isDeclared`

Found by kindlings-di while implementing macwire's ranking of enclosing members (declared before inherited).

## Symptom

```scala
trait DepProvider { def providedDep: Dep }
trait SelfTyped { this: DepProvider =>
  lazy val declaredDep: Dep = new Dep
  lazy val holder: Holder = wire[Holder]
}
```

`Environment.enclosingScope` gives an `Enclosure.Class` for `SelfTyped` whose `members` contain both `declaredDep` and
`providedDep`. On Scala 3 `declaredDep.isDeclared == true` and `providedDep.isDeclared == false`. On Scala 2.13 **every**
member has `isDeclared == false`, so a macro cannot tell the class's own members from the self-type's.

## Cause

`hearth/src/main/scala-2/hearth/EnvironmentsScala2.scala`, `toEnclosure`: for a class with a self-type the members are
listed on `typeOfThis` (or `classTpe with selfTpe`), so that the self-type's members are visible. That type is a
refinement, and `UntypedMethodsScala2.unsortedMethods` computes `isDeclared` from `instanceTpe.decls` - empty for a
refinement. Scala 3 lists the members on `symbol.typeRef` (the class itself), so it is not affected.

## Workaround in kindlings-di

`WiringMacrosImpl.declaredInSelfTypedClass`: when the immediately-enclosing class has no declared member at all (there is
always one otherwise: the member holding the macro call), find the class among `Type.baseClasses` of the enclosure type
by its full name and take the names of its declared members. Tested by `DeclaredMemberPrioritySpec` ("over a member
provided by a self-type") on both Scala versions.

## Suggested fix in Hearth

Compute `isDeclared` against the enclosing class symbol (`symbol.asClass.toType.decls`) rather than the refinement used to
list the members, e.g. by passing the declared set into `UntypedMethod.methods` for an `Enclosure.Class`.
