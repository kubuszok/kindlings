package hearth.kindlings.optics

/** Quicklens-style modify DSL: `obj.modify(_.a.b.c).setTo(v)` / `.using(f)`.
  *
  * The `modify` extension parses the field path and produces a [[PathModify]] carrying `obj` together with a nested
  * copy-with-modification function; the terminal operations on `PathModify` supply the actual `A => A` transformation.
  *
  * Available either via `import hearth.kindlings.optics.syntax.*` or directly via `import hearth.kindlings.optics.*`
  * (the package object mixes the same extension in).
  */
private[optics] trait OpticsSyntax {

  extension [S](inline obj: S) {
    inline def modify[A](inline path: S => A): PathModify[S, A] =
      ${ internal.compiletime.ModifyMacros.modifyImpl[S, A]('obj, 'path) }
  }

  // Marker extensions that let a `modify` path type-check past a `.each`/`.eachWhere` step. They have no usable runtime
  // behaviour — `modify(_.xs.each.field)` is rewritten by the macro, which never evaluates these bodies; they exist only
  // so `field` resolves against the element type `A`. The element type is recovered from the *exact* container `C` via
  // `IsElementOf.Aux[C, A]` (pinned invariantly), so `.each` yields the precise element type with no covariant widening.
  extension [C, A](c: C)(using IsElementOf.Aux[C, A]) {
    @scala.annotation.compileTimeOnly("`.each` is only usable inside a `modify(...)` path")
    def each: A = sys.error("`.each` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.eachWhere` is only usable inside a `modify(...)` path")
    def eachWhere(cond: A => Boolean): A = sys.error("`.eachWhere` is only usable inside a `modify(...)` path")
  }
}

object syntax extends OpticsSyntax
