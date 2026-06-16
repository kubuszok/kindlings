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

  // Marker extensions for the indexed `.at`/`.index`/`.atOrElse` steps over a `Seq` (Int index) or `Map` (key). The
  // index type `I` and element type `A` are pinned invariantly from the *exact* container `C` via
  // `IsIndexedElementOf.Aux[C, I, A]`, so the path type-checks past `.at(i): A` with no covariant widening.
  implicit final class AtOps[C, I, A](@scala.annotation.unused private val c: C)(implicit
      @scala.annotation.unused ev: IsIndexedElementOf.Aux[C, I, A]
  ) {
    @scala.annotation.compileTimeOnly("`.at` is only usable inside a `modify(...)` path")
    def at(@scala.annotation.unused idx: I): A = sys.error("`.at` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.index` is only usable inside a `modify(...)` path")
    def index(@scala.annotation.unused idx: I): A = sys.error("`.index` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.atOrElse` is only usable inside a `modify(...)` path")
    def atOrElse(@scala.annotation.unused idx: I, @scala.annotation.unused default: => A): A =
      sys.error("`.atOrElse` is only usable inside a `modify(...)` path")
  }

  // Marker extensions for the no-index `.at`/`.index`/`.atOrElse` over an `Option`-like container. Only one of `AtOps`
  // (indexed, for Seq/Map) and `SingleAtOps` (for Option) ever applies, since their evidences are mutually exclusive.
  implicit final class SingleAtOps[C, A](@scala.annotation.unused private val c: C)(implicit
      @scala.annotation.unused ev: IsSingleElementOf.Aux[C, A]
  ) {
    @scala.annotation.compileTimeOnly("`.at` is only usable inside a `modify(...)` path")
    def at: A = sys.error("`.at` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.index` is only usable inside a `modify(...)` path")
    def index: A = sys.error("`.index` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.atOrElse` is only usable inside a `modify(...)` path")
    def atOrElse(@scala.annotation.unused default: => A): A =
      sys.error("`.atOrElse` is only usable inside a `modify(...)` path")
  }

  // Marker extensions for `.eachLeft`/`.eachRight` over an `Either[L, R]`. The branch types are pinned invariantly via
  // `IsEither.Aux[C, L, R]`.
  implicit final class EitherOps[C, L, R](@scala.annotation.unused private val c: C)(implicit
      @scala.annotation.unused ev: IsEither.Aux[C, L, R]
  ) {
    @scala.annotation.compileTimeOnly("`.eachLeft` is only usable inside a `modify(...)` path")
    def eachLeft: L = sys.error("`.eachLeft` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.eachRight` is only usable inside a `modify(...)` path")
    def eachRight: R = sys.error("`.eachRight` is only usable inside a `modify(...)` path")
  }

  // Marker extension for the `.when[Subtype]` prism: narrows the focus `C` to a subtype `T <: C`. The macro emits a
  // non-exhaustive 2-case match (`case t: T => f(t); case other => other`).
  implicit final class WhenOps[C](@scala.annotation.unused private val c: C) {
    @scala.annotation.compileTimeOnly("`.when` is only usable inside a `modify(...)` path")
    def when[T <: C]: T = sys.error("`.when` is only usable inside a `modify(...)` path")
  }
}

object syntax extends OpticsSyntax
