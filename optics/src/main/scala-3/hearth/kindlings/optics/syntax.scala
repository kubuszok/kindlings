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

    /** `obj.modifyAll(_.a, _.b, _.c)` — modify several same-focus paths from one object at once. */
    inline def modifyAll[A](inline path: S => A, inline paths: (S => A)*): PathModify[S, A] =
      ${ internal.compiletime.ModifyMacros.modifyAllImpl[S, A]('obj, 'path, 'paths) }
  }

  /** `modifyLens[T](_.a.b)` — a reusable lens NOT bound to an object; its terminals return a `T => T`. */
  inline def modifyLens[T]: ModifyLensFactory[T] = new ModifyLensFactory[T]

  /** `modifyAllLens[T](_.a, _.b)` — the multi-path reusable lens form. */
  inline def modifyAllLens[T]: ModifyAllLensFactory[T] = new ModifyAllLensFactory[T]

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

  // Indexed `.at`/`.index`/`.atOrElse` over a `Seq` (Int index) or `Map` (key). `I` and `A` are pinned invariantly from
  // the exact container `C` via `IsIndexedElementOf.Aux[C, I, A]`.
  extension [C, I, A](c: C)(using IsIndexedElementOf.Aux[C, I, A]) {
    @scala.annotation.compileTimeOnly("`.at` is only usable inside a `modify(...)` path")
    def at(idx: I): A = sys.error("`.at` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.index` is only usable inside a `modify(...)` path")
    def index(idx: I): A = sys.error("`.index` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.atOrElse` is only usable inside a `modify(...)` path")
    def atOrElse(idx: I, default: => A): A = sys.error("`.atOrElse` is only usable inside a `modify(...)` path")
  }

  // No-index `.at`/`.index`/`.atOrElse` over an `Option`-like container.
  extension [C, A](c: C)(using IsSingleElementOf.Aux[C, A]) {
    @scala.annotation.compileTimeOnly("`.at` is only usable inside a `modify(...)` path")
    def at: A = sys.error("`.at` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.index` is only usable inside a `modify(...)` path")
    def index: A = sys.error("`.index` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.atOrElse` is only usable inside a `modify(...)` path")
    def atOrElse(default: => A): A = sys.error("`.atOrElse` is only usable inside a `modify(...)` path")
  }

  // `.eachLeft`/`.eachRight` over an `Either[L, R]`, branch types pinned invariantly via `IsEither.Aux[C, L, R]`.
  extension [C, L, R](c: C)(using IsEither.Aux[C, L, R]) {
    @scala.annotation.compileTimeOnly("`.eachLeft` is only usable inside a `modify(...)` path")
    def eachLeft: L = sys.error("`.eachLeft` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.eachRight` is only usable inside a `modify(...)` path")
    def eachRight: R = sys.error("`.eachRight` is only usable inside a `modify(...)` path")
  }

  // `.when[Subtype]` prism: narrows the focus `C` to a subtype `T <: C`.
  extension [C](c: C) {
    @scala.annotation.compileTimeOnly("`.when` is only usable inside a `modify(...)` path")
    def when[T <: C]: T = sys.error("`.when` is only usable inside a `modify(...)` path")
  }
}

/** Curries the explicit root type `T` of `modifyLens[T](_.a.b)`, leaving the focused type `U` to be inferred from the
  * path lambda.
  */
final class ModifyLensFactory[T] {
  inline def apply[U](inline path: T => U): PathLazyModify[T, U] =
    ${ internal.compiletime.ModifyMacros.modifyLensImpl[T, U]('path) }
}

/** Curries the explicit root type `T` of `modifyAllLens[T](_.a, _.b)`. */
final class ModifyAllLensFactory[T] {
  inline def apply[U](inline path: T => U, inline paths: (T => U)*): PathLazyModify[T, U] =
    ${ internal.compiletime.ModifyMacros.modifyAllLensImpl[T, U]('path, 'paths) }
}

object syntax extends OpticsSyntax
