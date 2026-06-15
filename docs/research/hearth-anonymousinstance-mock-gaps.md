# Hearth `AnonymousInstance` gaps found while porting ScalaMock (`mock` module)

Status: found 2026-06-15 while raising `mock` test coverage to ScalaMock parity. Each item
below is a method shape that ScalaMock mocks but that Kindlings `mock` cannot yet override,
because Hearth's `AnonymousInstance.construct` / `OverrideContext` does not surface enough
information (or `unsafeNewSubtype` emits the wrong member shape). Per the project rule
("Kindlings stress-tests Hearth"), these should become Hearth issues + minimal reproducers.

Each gap is covered by an **ignored** test in `mock/src/test/.../MockSpec.scala` (or
`scala-3/MockScala3Spec.scala`) — the bodies are elided/`ignore`d because the macro expands
even under `.ignore`, so leaving real bodies would break the build. Un-ignore them to
reproduce once the corresponding Hearth fix lands.

## 1. Generic methods — `def f[T](x: T): R`
`OverrideContext.returnType` (and parameter types) come back **erased to `Any`** for a method
with its own type parameters: `method.knownReturning.getOrElse(Type.of[Any])`. Overriding then
fails to typecheck ("found `Any`, required `String`"). Hearth does not type-apply the method's
type params at the override site. **Needed:** expose the method's type-parameter list so the
override can be generic, or at least the declared (un-erased) return type.

## 2. Operator / symbolic method names — `def +(x: T): T`
No override is emitted for a symbolic-named abstract method → `construct` fails with
"Missing implementation". The classifier/`mustOverride` set or `unsafeNewSubtype` drops
operator-named members. **Needed:** treat symbolic names like alphabetic ones.

## 3. Implicit parameter clauses — `def f(x: A)(implicit y: B): R`
The trailing `implicit` clause is reified as a **second explicit** parameter clause, so the
abstract member is not considered implemented (Scala 2 `unsafeNewSubtype` builds the params
with `Flag.PARAM`, dropping `IMPLICIT`). The generated subtype then still has an abstract
member. **Needed:** preserve the `implicit`/`using` modifier on override parameter clauses.

## 4. Abstract `val` — `val v: A`
Overriding an abstract `val` emits a `def`, which the compiler rejects: "stable, immutable
value required to override". **Needed:** `OverrideBody` for a `val` member should emit a
`val`, not a `def`.

## 5. `this.type` return (Scala 3) — `def chain(): this.type`
The override is typed as the **parent trait**, not the new anonymous subtype's `this.type`,
so it doesn't conform. **Needed:** thread the fresh subtype's `this.type` into the override
return position.

## 6. Context-function return (Scala 3) — `def f(x: A): B ?=> C`
A context-function return type is treated as a **trailing implicit parameter clause**, giving
the generated member the wrong arity (runtime arity mismatch). **Needed:** treat
`B ?=> C` as a return type, not a parameter clause.

---

These are the only ScalaMock method shapes `mock` cannot yet handle; everything else
(curried, varargs, by-name, default args, polymorphic *traits*, union/intersection/opaque
params, many-arg methods, abstract classes, ctor args) works on JVM/JS/Native, 2.13 + 3.
See also the `di` module's two related Hearth-gap notes:
[`di-self-type-limitation.md`](di-self-type-limitation.md) (self-type members not exposed on
Scala 2) and [`di-by-name-limitation.md`](di-by-name-limitation.md) (`ByNameType` underlying
not exposed on Scala 3).
