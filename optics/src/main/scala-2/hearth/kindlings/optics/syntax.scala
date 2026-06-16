package hearth.kindlings.optics

import scala.language.experimental.macros

/** Quicklens-style modify DSL: `obj.modify(_.a.b.c).setTo(v)` / `.using(f)`.
  *
  * The `modify` implicit class parses the field path and produces a [[PathModify]] carrying `obj` together with a nested
  * copy-with-modification function; the terminal operations on `PathModify` supply the actual `A => A` transformation.
  *
  * Available either via `import hearth.kindlings.optics.syntax._` or directly via `import hearth.kindlings.optics._`
  * (the package object mixes the same implicit class in).
  */
private[optics] trait OpticsSyntax {

  implicit class ModifyOps[S](val obj: S) {
    def modify[A](path: S => A): PathModify[S, A] = macro internal.compiletime.ModifyMacros.modifyImpl[S, A]
  }
}

object syntax extends OpticsSyntax
