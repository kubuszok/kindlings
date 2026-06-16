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
}

object syntax extends OpticsSyntax
