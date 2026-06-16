package hearth.kindlings.optics

import scala.language.experimental.macros

/** Quicklens-style modify DSL: `obj.modify(_.a.b.c).setTo(v)` / `.using(f)`.
  *
  * The `modify` implicit class parses the field path and produces a [[PathModify]] carrying `obj` together with a
  * nested copy-with-modification function; the terminal operations on `PathModify` supply the actual `A => A`
  * transformation.
  *
  * Available either via `import hearth.kindlings.optics.syntax._` or directly via `import hearth.kindlings.optics._`
  * (the package object mixes the same implicit class in).
  */
private[optics] trait OpticsSyntax {

  implicit class ModifyOps[S](val obj: S) {
    def modify[A](path: S => A): PathModify[S, A] = macro internal.compiletime.ModifyMacros.modifyImpl[S, A]
  }

  // Marker extensions that let a `modify` path type-check past a `.each`/`.eachWhere` step. They have no usable runtime
  // behaviour — `modify(_.xs.each.field)` is rewritten by the macro, which never evaluates these bodies; they exist only
  // so `field` resolves against the element type `A`. The element type is recovered from the *exact* container `C` via
  // `IsElementOf.Aux[C, A]` (pinned invariantly), so `.each` yields the precise element type with no covariant widening.
  implicit final class EachOps[C, A](@scala.annotation.unused private val c: C)(implicit
      @scala.annotation.unused ev: IsElementOf.Aux[C, A]
  ) {
    @scala.annotation.compileTimeOnly("`.each` is only usable inside a `modify(...)` path")
    def each: A = sys.error("`.each` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.eachWhere` is only usable inside a `modify(...)` path")
    def eachWhere(@scala.annotation.unused cond: A => Boolean): A =
      sys.error("`.eachWhere` is only usable inside a `modify(...)` path")
  }
}

object syntax extends OpticsSyntax
