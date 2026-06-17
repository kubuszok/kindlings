package hearth.kindlings.optics

import scala.language.experimental.macros
import scala.language.implicitConversions

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

    /** `obj.modifyAll(_.a, _.b, _.c)` — modify several same-focus paths from one object at once. */
    def modifyAll[A](path: S => A, paths: (S => A)*): PathModify[S, A] =
      macro internal.compiletime.ModifyMacros.modifyAllImpl[S, A]
  }

  /** `modifyLens[T](_.a.b)` — a reusable lens NOT bound to an object; its terminals return a `T => T`. */
  def modifyLens[T]: ModifyLensFactory[T] = new ModifyLensFactory[T]

  /** `modifyAllLens[T](_.a, _.b)` — the multi-path reusable lens form. */
  def modifyAllLens[T]: ModifyAllLensFactory[T] = new ModifyAllLensFactory[T]

  // Marker extensions that let a `modify` path type-check past a `.each`/`.eachWhere` step. They have no usable runtime
  // behaviour — `modify(_.xs.each.field)` is rewritten by the macro, which never evaluates these bodies; they exist only
  // so `field` resolves against the element type `A`. The element type is recovered from the *exact* container `C` via
  // `IsElementOf.Aux[C, A]` (pinned invariantly), so `.each` yields the precise element type with no covariant widening.
  // `.each`/`.eachWhere` recover the element type from a whitebox-materialized `IsElementOf[C]`. The element type is
  // projected into the ops class' type parameter `A` by a manual implicit CONVERSION: `toEachOps[C]` summons the
  // evidence as a METHOD implicit (kept at its refined `Aux[C, Elem]` type), and returns `EachOps[C, ev.Elem]` — so
  // `ev.Elem` reduces to the concrete element type at the call site and is baked into `A`. (An `implicit class
  // EachOps[C, A]` instead would leave `A` un-inferred — Scala 2 can't infer a class type param from a whitebox
  // expansion; a class field `val ev` would widen the refinement away.)
  final class EachOps[C, A](@scala.annotation.unused private val c: C) {
    @scala.annotation.compileTimeOnly("`.each` is only usable inside a `modify(...)` path")
    def each: A = sys.error("`.each` is only usable inside a `modify(...)` path")

    @scala.annotation.compileTimeOnly("`.eachWhere` is only usable inside a `modify(...)` path")
    def eachWhere(@scala.annotation.unused cond: A => Boolean): A =
      sys.error("`.eachWhere` is only usable inside a `modify(...)` path")
  }

  implicit def toEachOps[C](c: C)(implicit ev: IsElementOf[C]): EachOps[C, ev.Elem] = new EachOps[C, ev.Elem](c)

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

/** Curries the explicit root type `T` of `modifyLens[T](_.a.b)`, leaving the focused type `U` to be inferred from the
  * path lambda.
  */
final class ModifyLensFactory[T] {
  // The focused type is named `Foc` (not `U`) to avoid shadowing the `Universe`-typed `U` the Scala 2 `WeakTypeTag`
  // materializer introduces into this method's generated body.
  def apply[Foc](path: T => Foc): PathLazyModify[T, Foc] =
    macro internal.compiletime.ModifyMacros.modifyLensImpl[T, Foc]
}

/** Curries the explicit root type `T` of `modifyAllLens[T](_.a, _.b)`. */
final class ModifyAllLensFactory[T] {
  def apply[Foc](path: T => Foc, paths: (T => Foc)*): PathLazyModify[T, Foc] =
    macro internal.compiletime.ModifyMacros.modifyAllLensImpl[T, Foc]
}

object syntax extends OpticsSyntax
