package hearth.kindlings.optics

// `.each` / `.eachWhere` (collections, map values, Option) and `.eachLeft` / `.eachRight` (Either) are handled by the
// `modify` macro directly through Hearth's `IsCollection` / `IsMap` / `IsOption` / `IsEither` SPI — no runtime functor
// type class. What remains here are (a) the runtime type classes for the indexed `.at` / `.index` / `.atOrElse` steps
// (for which Hearth has no positional-update SPI, so they stay bespoke — see docs/research) and (b) the invariant
// compile-time marker evidences that let the path lambda type-check.

/** Runtime type class for the indexed `.at` / `.index` / `.atOrElse` steps over a container `F[T]` keyed by `I` (a
  * `Seq` keyed by `Int`, or a `Map[K, V]` keyed by `K`).
  *
  *   - [[at]] transforms the element at `i`, throwing if it is absent;
  *   - [[index]] transforms the element at `i` if present, leaving the container unchanged otherwise;
  *   - [[atOrElse]] transforms the element at `i`, inserting `default` first if it is absent.
  *
  * Mirrors SoftwareMill quicklens' `QuicklensIndexedFunctor`.
  */
trait QuicklensIndexedFunctor[F[_], I] {
  def at[A](fa: F[A], idx: I)(f: A => A): F[A]
  def atOrElse[A](fa: F[A], idx: I, default: => A)(f: A => A): F[A]
  def index[A](fa: F[A], idx: I)(f: A => A): F[A]
}

object QuicklensIndexedFunctor {

  /** `Seq`-like containers indexed by `Int` (covers `List`/`Vector`/`Seq`). The runtime helper always rebuilds via the
    * generic `Seq` API, so a single given for `Seq` serves all of them on the JVM (the concrete subtype is preserved by
    * the underlying collection's `updated`).
    */
  // Bounded on `scala.collection.SeqOps` so the rebuild uses the collection's own `updated`/`appended`, which preserve
  // the concrete type (`List.updated` returns a `List`, `Vector.updated` a `Vector`, ...) — rebuilding via a generic
  // `Vector` would break the macro's `.asInstanceOf[F[A]]` cast on the focused field's declared type.
  implicit def seqIndexedFunctor[F[X] <: scala.collection.immutable.Seq[X] & scala.collection.SeqOps[X, F, F[X]]]
      : QuicklensIndexedFunctor[F, Int] =
    new QuicklensIndexedFunctor[F, Int] {
      def at[A](fa: F[A], idx: Int)(f: A => A): F[A] =
        if (idx >= 0 && idx < fa.length) fa.updated(idx, f(fa(idx)))
        else throw new NoSuchElementException(s"Index $idx not found")
      def atOrElse[A](fa: F[A], idx: Int, default: => A)(f: A => A): F[A] =
        if (idx >= 0 && idx < fa.length) fa.updated(idx, f(fa(idx)))
        else if (idx == fa.length) fa.appended(f(default)) // absent index just past the end: insert the default
        else throw new IndexOutOfBoundsException(s"Index $idx out of range for atOrElse")
      def index[A](fa: F[A], idx: Int)(f: A => A): F[A] =
        if (idx >= 0 && idx < fa.length) fa.updated(idx, f(fa(idx))) else fa
    }
}

/** Runtime type class for the indexed `.at` / `.index` / `.atOrElse` steps over the *values* of a map-like container
  * `M[K, _]`, keyed by `K`. Mirrors quicklens' map-keyed indexing.
  */
trait QuicklensMapAtFunctor[M[_, _], K] {
  def at[A](fa: M[K, A], key: K)(f: A => A): M[K, A]
  def atOrElse[A](fa: M[K, A], key: K, default: => A)(f: A => A): M[K, A]
  def index[A](fa: M[K, A], key: K)(f: A => A): M[K, A]
}

object QuicklensMapAtFunctor {

  implicit def mapAtFunctor[K]: QuicklensMapAtFunctor[Map, K] = new QuicklensMapAtFunctor[Map, K] {
    def at[A](fa: Map[K, A], key: K)(f: A => A): Map[K, A] =
      fa.updated(key, f(fa.getOrElse(key, throw new NoSuchElementException(s"key not found: $key"))))
    def atOrElse[A](fa: Map[K, A], key: K, default: => A)(f: A => A): Map[K, A] =
      fa.updated(key, f(fa.getOrElse(key, default)))
    def index[A](fa: Map[K, A], key: K)(f: A => A): Map[K, A] =
      fa.get(key).fold(fa)(a => fa.updated(key, f(a)))
  }

  /** Unary projection for a fixed key `K`, so the macro can summon `QuicklensMapAtFunctor[Map, K]` via Hearth's unary
    * `Type.Ctor1` (there is no `* -> (*,*) -> *` constructor primitive). The binary slot is pinned to `Map`.
    */
  type ForMap[K] = QuicklensMapAtFunctor[Map, K]
}

/** Runtime type class for the single-element `.at` / `.index` / `.atOrElse` steps over an `Option`-like container
  * `F[_]`. Mirrors quicklens' `QuicklensSingleAtFunctor`.
  *
  *   - [[at]] transforms the contained element, throwing if it is absent;
  *   - [[index]] transforms it if present, leaving the container unchanged otherwise;
  *   - [[atOrElse]] transforms it, inserting `default` first if it is absent.
  */
trait QuicklensSingleAtFunctor[F[_]] {
  def at[A](fa: F[A])(f: A => A): F[A]
  def atOrElse[A](fa: F[A], default: => A)(f: A => A): F[A]
  def index[A](fa: F[A])(f: A => A): F[A]
}

object QuicklensSingleAtFunctor {

  implicit val optionSingleAtFunctor: QuicklensSingleAtFunctor[Option] = new QuicklensSingleAtFunctor[Option] {
    def at[A](fa: Option[A])(f: A => A): Option[A] =
      Some(f(fa.getOrElse(throw new NoSuchElementException("None.at"))))
    def atOrElse[A](fa: Option[A], default: => A)(f: A => A): Option[A] = Some(f(fa.getOrElse(default)))
    def index[A](fa: Option[A])(f: A => A): Option[A] = fa.map(f)
  }
}

/** Runtime entry points the indexed `.at` / `.index` / `.atOrElse` macro codegen calls into. The macro summons the
  * relevant indexed functor (erased to `Any` at the call site, since the discovered constructor is not statically
  * known) and forwards the container, index/key and per-element function here, where the casts are concentrated. Safe
  * because the macro only ever pairs a functor with a container of the matching constructor.
  */
object QuicklensRuntime {

  // The element/value type `A` is erased to `Any`; `F`/`M` are erased on the JVM, so summoning an indexed functor and
  // calling it through `Id`/`MapId` is sound — the underlying operations run on the real runtime container.
  private type Id[X] = X
  private type MapId[K, V] = V

  // --- Indexed `.at` / `.index` / `.atOrElse` over a `Seq`-like container (keyed by `Int`). ---

  def atIndexed(functor: Any, fa: Any, idx: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensIndexedFunctor[Id, Any]].at[Any](fa, idx)(f)
  def atOrElseIndexed(functor: Any, fa: Any, idx: Any, default: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensIndexedFunctor[Id, Any]].atOrElse[Any](fa, idx, default)(f)
  def indexIndexed(functor: Any, fa: Any, idx: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensIndexedFunctor[Id, Any]].index[Any](fa, idx)(f)

  // --- Indexed `.at` / `.index` / `.atOrElse` over the values of a map-like container (keyed by `K`). ---

  def atMap(functor: Any, fa: Any, key: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensMapAtFunctor[MapId, Any]].at[Any](fa, key)(f)
  def atOrElseMap(functor: Any, fa: Any, key: Any, default: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensMapAtFunctor[MapId, Any]].atOrElse[Any](fa, key, default)(f)
  def indexMap(functor: Any, fa: Any, key: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensMapAtFunctor[MapId, Any]].index[Any](fa, key)(f)

  // --- Single-element `.at` / `.index` / `.atOrElse` over an `Option`-like container. ---

  def atSingle(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensSingleAtFunctor[Id]].at[Any](fa)(f)
  def atOrElseSingle(functor: Any, fa: Any, default: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensSingleAtFunctor[Id]].atOrElse[Any](fa, default)(f)
  def indexSingle(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensSingleAtFunctor[Id]].index[Any](fa)(f)
}

/** Compile-time evidence that lets `_.xs.each` type-check, pinning the traversed element type as the member `Elem`
  * (invariantly, so it cannot be widened). This is ONLY a typer gate: whether `C` is actually traversable — and the
  * rebuild — is decided by the `modify` macro via Hearth's `IsCollection`/`IsMap`/`IsOption` SPI. Keeping the evidence
  * permissive (any `F[A]`/`Array`/`Map`) is what lets a provider jar on the classpath (cats `NonEmpty*`, java
  * collections, ...) light up `.each` with NO per-type given.
  */
@scala.annotation.implicitNotFound("`.each` expects a collection/map/Option-shaped container, but ${C} is not one")
sealed trait IsElementOf[C] { type Elem }

object IsElementOf extends LowPriorityIsElementOf {
  type Aux[C, A] = IsElementOf[C] { type Elem = A }
  private[optics] val instance: IsElementOf[Any] = new IsElementOf[Any] { type Elem = Any }
  private[optics] def of[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]

  // Map values traverse to the value type `V`. `Array[A]` does not unify the unary `F[A]` shape, so it needs its own
  // rule. Both sit at higher priority than the catch-all `fromUnary`, so `Map`/`Array` resolve unambiguously.
  implicit def map[K, V]: Aux[Map[K, V], V] = of
  implicit def array[A]: Aux[Array[A], A] = of
}

sealed trait LowPriorityIsElementOf {

  /** Catch-all: any unary `F[A]` is `.each`-able at the type level (precise element `A` by unification). Lower priority
    * than [[IsElementOf.map]]/[[IsElementOf.array]] so `Map`/`Array` don't become ambiguous. The `modify` macro decides
    * real support via the `IsCollection`/`IsMap`/`IsOption` SPI and errors clearly otherwise.
    */
  implicit def fromUnary[F[_], A]: IsElementOf.Aux[F[A], A] = IsElementOf.of
}

/** Common (non-parameterised) supertype of the Phase 3 marker evidences, so the macro can recognise a synthesized
  * evidence value argument by a single `Type.isSubtypeOf[T, PathStepEvidence]` check (an invariant `IsXxx[C]` is not a
  * subtype of `IsXxx[Any]`, so a constructor-agnostic marker is needed).
  */
sealed trait PathStepEvidence

/** Evidence that `C` is a container indexed by `I` whose element (for `.at`/`.index`/`.atOrElse`) is `Elem`, both fixed
  * *invariantly* by the matching `given` (so neither widens). Instances: `Seq`-like `F[A]` keyed by `Int`, `Map[K, V]`
  * keyed by `K`, and `Option[A]` (keyed by `Unit` — the index is supplied as `()` but ignored at runtime).
  */
@scala.annotation.implicitNotFound(
  "`.at`/`.index`/`.atOrElse` is not supported on ${C} — it works on Seq (Int index), Map (key) and Option"
)
sealed trait IsIndexedElementOf[C] extends PathStepEvidence { type Idx; type Elem }

object IsIndexedElementOf {
  type Aux[C, I, A] = IsIndexedElementOf[C] { type Idx = I; type Elem = A }
  private val instance: IsIndexedElementOf[Any] = new IsIndexedElementOf[Any] { type Idx = Any; type Elem = Any }
  private def of[C, I, A]: Aux[C, I, A] = instance.asInstanceOf[Aux[C, I, A]]

  // Map values, keyed by the key type `K`.
  implicit def map[K, V]: Aux[Map[K, V], K, V] = of

  // Any `Seq`-like `F[A]` with a `QuicklensIndexedFunctor[F, Int]` in scope, keyed by `Int`.
  implicit def fromIndexed[F[_], A](implicit
      @scala.annotation.unused F: QuicklensIndexedFunctor[F, Int]
  ): Aux[F[A], Int, A] = of
}

/** Evidence that `C` is a single-element container `F[A]` (an `Option`-like) whose contained element is `Elem`, fixed
  * invariantly. Used by the no-index `.at`/`.index`/`.atOrElse` over `Option`.
  */
@scala.annotation.implicitNotFound("the no-index `.at`/`.index`/`.atOrElse` is only supported on Option-like ${C}")
sealed trait IsSingleElementOf[C] extends PathStepEvidence { type Elem }

object IsSingleElementOf {
  type Aux[C, A] = IsSingleElementOf[C] { type Elem = A }
  private val instance: IsSingleElementOf[Any] = new IsSingleElementOf[Any] { type Elem = Any }
  private def of[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]

  implicit def fromSingleAt[F[_], A](implicit
      @scala.annotation.unused F: QuicklensSingleAtFunctor[F]
  ): Aux[F[A], A] = of
}

/** Evidence that `C` is an `Either[L, R]` whose left branch is `Left` and right branch is `Right`, both fixed
  * invariantly. Used by `.eachLeft` (focuses `Left`) and `.eachRight` (focuses `Right`).
  */
@scala.annotation.implicitNotFound("`.eachLeft`/`.eachRight` is only supported on Either, not ${C}")
sealed trait IsEither[C] extends PathStepEvidence { type Left; type Right }

object IsEither {
  type Aux[C, L, R] = IsEither[C] { type Left = L; type Right = R }
  private val instance: IsEither[Any] = new IsEither[Any] { type Left = Any; type Right = Any }
  private def of[C, L, R]: Aux[C, L, R] = instance.asInstanceOf[Aux[C, L, R]]

  implicit def either[L, R]: Aux[Either[L, R], L, R] = of
}
