# Hearth bug: `caseFieldValuesAt` asserts when a case class declares a method named like a field

**Status:** OPEN (Hearth `0.4.5-18-g99ac2ae-SNAPSHOT`, Scala 3.8.3; found 2026-11-27 while running quicklens' test
suite against `kindlings-optics`). Scala 2.13 was not checked separately.

## Symptom

```scala
final case class Paths(pathItems: Map[String, String] = Map.empty)
final case class Docs(paths: Paths = Paths()) {
  def paths(paths: Paths): Docs = copy(paths = paths) // an overload of the field's name
}

Docs().modify(_.paths.pathItems).using(_ + ("a" -> "b"))
```

`CaseClass.parse[Docs]` is `Compatible`, but `caseClass.caseFieldValuesAt(expr)` throws inside the macro:

```
java.lang.AssertionError: Field paths of Docs is not nullary instance method: arity=1, invocation=OnInstance
  at hearth.typed.Classes$CaseClass.caseFieldValuesAt$$anonfun$2(Classes.scala:550)
```

The lookup of the case field by name picks the unary method `paths(paths: Paths)` instead of the case accessor `paths`
(both are members named `paths`), then asserts that it is nullary.

## Expected

`caseFieldValuesAt` returns the value of the case accessor (`isCaseField`), whatever other overloads of that name the
class declares. An unsupported shape should be a `Left`/`None`, not an `AssertionError`.

## Reproducer in Kindlings

`optics/src/test/scala-3/hearth/kindlings/optics/upstream/ExplicitCopyTest.scala`, "modify a class that has a method
with the same name as a field" (ported from quicklens, which supports it).

## Workaround in Kindlings

`ModifyMacrosImpl.caseFieldValuesOf` catches the `AssertionError` and the field step falls back to the path used for
classes that are not case classes: read the field with the nullary member of that name
(`Method.unsortedMethodsNamed[S](field).filter(_.isNullary)`) and rebuild with `s.copy(field = ...)`. The test passes
through that path; remove the `try`/`catch` once Hearth returns the accessor.

## Related: exceptions from `Method.fold` on a Scala 3 extension `copy`

Calling an extension method `copy(x: Double = v.x, y: Double = v.y)` (declared in the companion, quicklens'
`ExtensionCopyTest`) through `Method.fold` with only `x` provided throws

```
hearth.HearthRequirementError: Expected that VecCompanion's copy$default$2 parameter `v` would be provided or have a
default value.
```

instead of returning `Left`: the default-argument accessor of an extension method takes the extension's receiver, which
the default-value call does not pass. `ModifyMacrosImpl.callOrError` turns the exception into a `Left` so the user gets
the regular "no usable `copy`" compilation error. Extension-method `copy`/accessors are not supported by
`kindlings-optics` (documented in `feature-parity.md`).
