# Hearth gap: `Type.of` before `import x.Underlying as T` fails on Scala 3 ("Not found: type T")

Status: **RESOLVED by kubuszok/hearth#389** (Hearth 0.4.4; Kindlings pins `0.4.3-17-gbd1e277-SNAPSHOT`). di-cats'
`preciseDependency` is back in the natural order (`Type.of[Any]`, then `import parsed.tpe.Underlying as D`) and compiles on
Scala 3.

Originally: **OPEN** (found 2026-10-25 with Hearth 0.4.3 / hearth-cross-quotes, Scala 3 only; 2.13 compiles fine).

## Reproducer

In a shared (`MacroCommons`) macro implementation:

```scala
def preciseDependency(dependency: Expr[Any]): Expr_?? = {
  implicit val anyType: Type[Any] = Type.of[Any]
  val parsed = DestructuredExpr.parse(dependency)
  import parsed.tpe.Underlying as D
  parsed.toUntypedExpr.asTyped[D].as_??
}
```

On Scala 3 this fails at the `Type.of[Any]` position with `[E006] Not Found Error: Not found: type D`. The same thing
happened with `implicit val r: Type[Resource[F, X]] = Type.of[Resource[F, X]]` followed by
`import companion.{Underlying as Companion, ...}`, reported at the method signature: "Not found: type Companion".

It seems the plugin's `Type.of` rewrite picks up every `Underlying` alias imported in the enclosing block, including
ones imported *after* the `Type.of`, which are not in scope at that point.

## Workaround

Put the `import ... Underlying as T` before the `Type.of`, or move the import into a separate method. Both are used in
`di-cats/.../ResourceWiringMacrosImpl.scala` (`companionResourceMethod`, `preciseDependency`/`asPreciselyTyped`).
