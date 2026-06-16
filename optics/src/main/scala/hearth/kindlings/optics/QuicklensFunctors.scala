package hearth.kindlings.optics

/** Runtime type class for the `.each` collection traversal step of a `modify` path.
  *
  * `obj.modify(_.xs.each.field)` summons a `QuicklensFunctor[F]` for the collection constructor `F` (recovered from the
  * focused field's type) and emits `functor.each(xs, elem => <continue path on elem>)`. The macro only ever calls
  * [[each]] — the conditional traversal of `.eachWhere` is implemented on top of [[each]] at runtime.
  *
  * Mirrors SoftwareMill quicklens' `QuicklensFunctor`. Users can provide their own givens to make `.each` work over
  * custom container types.
  */
trait QuicklensFunctor[F[_]] {

  /** Apply `f` to every element of `fa`, rebuilding the container. */
  def each[A, B](fa: F[A])(f: A => B): F[B]

  /** Apply `f` only to the elements of `fa` matching `cond`, leaving the rest untouched. Defaults to a filtered
    * [[each]]; containers with a cheaper conditional traversal may override.
    */
  def eachWhere[A](fa: F[A], cond: A => Boolean)(f: A => A): F[A] =
    each(fa)(a => if (cond(a)) f(a) else a)
}

object QuicklensFunctor {

  def apply[F[_]](implicit F: QuicklensFunctor[F]): QuicklensFunctor[F] = F

  implicit val optionQuicklensFunctor: QuicklensFunctor[Option] = new QuicklensFunctor[Option] {
    def each[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit val listQuicklensFunctor: QuicklensFunctor[List] = new QuicklensFunctor[List] {
    def each[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit val vectorQuicklensFunctor: QuicklensFunctor[Vector] = new QuicklensFunctor[Vector] {
    def each[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }

  implicit val seqQuicklensFunctor: QuicklensFunctor[Seq] = new QuicklensFunctor[Seq] {
    def each[A, B](fa: Seq[A])(f: A => B): Seq[B] = fa.map(f)
  }

  implicit val setQuicklensFunctor: QuicklensFunctor[Set] = new QuicklensFunctor[Set] {
    def each[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
  }

  // A single, element-type-agnostic `Array` functor. Rebuilding an array normally needs a `ClassTag`, which the macro
  // cannot supply when summoning for the erased constructor. Since `.each`/`modify` keeps the element type (the leaf
  // modification is `A => A`, so `B = A`), we `clone()` the input array — which preserves its component type and links
  // on JVM/JS/Native (unlike `java.lang.reflect.Array`, which is unsupported on Scala.js) — and overwrite each slot in
  // place. The generic array-update unboxes into primitive arrays as needed.
  implicit val arrayQuicklensFunctor: QuicklensFunctor[Array] = new QuicklensFunctor[Array] {
    def each[A, B](fa: Array[A])(f: A => B): Array[B] = {
      val out = fa.clone().asInstanceOf[Array[B]]
      var i = 0
      while (i < fa.length) { out(i) = f(fa(i)); i += 1 }
      out
    }
  }
}

/** Runtime type class for the `.each` traversal over the *values* of a map-like container `M[K, _]` (the keys are left
  * untouched). `obj.modify(_.m.each.field)` over a `Map[K, V]` summons a `QuicklensMapFunctor[Map, K]`.
  *
  * Mirrors quicklens' map-values `each`.
  */
trait QuicklensMapFunctor[M[_, _], K] {

  /** Apply `f` to every value of `fa`, leaving the keys untouched. */
  def each[A, B](fa: M[K, A])(f: A => B): M[K, B]

  /** Apply `f` only to the values matching `cond`. */
  def eachWhere[A](fa: M[K, A], cond: A => Boolean)(f: A => A): M[K, A] =
    each(fa)(a => if (cond(a)) f(a) else a)
}

object QuicklensMapFunctor {

  def apply[M[_, _], K](implicit F: QuicklensMapFunctor[M, K]): QuicklensMapFunctor[M, K] = F

  implicit def mapQuicklensFunctor[K]: QuicklensMapFunctor[Map, K] = new QuicklensMapFunctor[Map, K] {
    def each[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.view.mapValues(f).toMap
  }

  /** Unary (`* -> *`) projection of [[QuicklensMapFunctor]] over `scala.collection.immutable.Map` for a fixed key `K`.
    *
    * The macro discovers the key type `K` of a `Map[K, V]` container and summons `QuicklensMapFunctor[Map, K]` by
    * building `Type[ForMap[K]]` via Hearth's unary `Type.Ctor1` (there is no `* -> (*,*) -> *` constructor primitive in
    * Hearth, so the binary `M[_, _]` slot is pinned to `Map` here).
    */
  type ForMap[K] = QuicklensMapFunctor[Map, K]
}

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

/** Runtime type class for the `.eachLeft` / `.eachRight` steps over an `Either[L, R]`. Mirrors quicklens'
  * `QuicklensEitherFunctor`. The non-targeted branch is left untouched.
  */
trait QuicklensEitherFunctor[T[_, _], L, R] {
  def eachLeft(fa: T[L, R])(f: L => L): T[L, R]
  def eachRight(fa: T[L, R])(f: R => R): T[L, R]
}

object QuicklensEitherFunctor {

  implicit def eitherFunctor[L, R]: QuicklensEitherFunctor[Either, L, R] =
    new QuicklensEitherFunctor[Either, L, R] {
      def eachLeft(fa: Either[L, R])(f: L => L): Either[L, R] = fa.left.map(f)
      def eachRight(fa: Either[L, R])(f: R => R): Either[L, R] = fa.map(f)
    }

  /** Unary projection for a fixed `Either` and right type `R`, used to summon the left-branch functor by `L`. */
  type ForLeft[L] = QuicklensEitherFunctor[Either, L, Any]

  /** Unary projection for a fixed `Either` and left type `L`, used to summon the right-branch functor by `R`. */
  type ForRight[R] = QuicklensEitherFunctor[Either, Any, R]
}

/** Runtime entry points the `.each` macro codegen calls into. The macro summons a [[QuicklensFunctor]] /
  * [[QuicklensMapFunctor]] (erased to `Any` at the call site, since the discovered constructor is not statically known)
  * and forwards the container and the per-element function here, where the casts are concentrated. Safe because the
  * macro only ever pairs a functor with a container of the matching constructor.
  */
object QuicklensRuntime {

  // The element/value type `A` is erased to `Any`; `F`/`M` are erased on the JVM, so summoning a `QuicklensFunctor[F]`
  // and calling it as a `QuicklensFunctor[Id]` is sound — the underlying `map`/`mapValues` operate on the real runtime
  // container regardless of the phantom constructor.
  private type Id[X] = X

  /** `functor.each(fa)(f)` for a unary container functor, all arguments erased. */
  def eachFunctor(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensFunctor[Id]].each[Any, Any](fa)(f)

  /** `functor.eachWhere(fa, cond)(f)` for a unary container functor, all arguments erased. */
  def eachFunctorWhere(functor: Any, fa: Any, cond: Any => Boolean, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensFunctor[Id]].eachWhere[Any](fa, cond)(f)

  private type MapId[K, V] = V

  /** `functor.each(fa)(f)` for a map-values functor, all arguments erased. */
  def eachMap(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensMapFunctor[MapId, Any]].each[Any, Any](fa)(f)

  /** `functor.eachWhere(fa, cond)(f)` for a map-values functor, all arguments erased. */
  def eachMapWhere(functor: Any, fa: Any, cond: Any => Boolean, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensMapFunctor[MapId, Any]].eachWhere[Any](fa, cond)(f)

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

  // --- `.eachLeft` / `.eachRight` over an `Either`. ---

  private type EitherId[L, R] = Any

  def eachLeft(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensEitherFunctor[EitherId, Any, Any]].eachLeft(fa)(f)
  def eachRight(functor: Any, fa: Any, f: Any => Any): Any =
    functor.asInstanceOf[QuicklensEitherFunctor[EitherId, Any, Any]].eachRight(fa)(f)
}

/** Evidence that `C` is a container whose traversable element (for `.each`) is the type member `Elem`. `Elem` is fixed
  * *invariantly* by the matching `given`, so it cannot be widened to a supertype the way a bare `F[A]` element would
  * be. Instances cover the unary functors (`Seq`/`List`/`Vector`/`Set`/`Option`/`Array` via `QuicklensFunctor`) and map
  * values.
  */
@scala.annotation.implicitNotFound(
  "`.each` is not supported on ${C} — provide a QuicklensFunctor/QuicklensMapFunctor for its element type"
)
sealed trait IsElementOf[C] { type Elem }

object IsElementOf {
  type Aux[C, A] = IsElementOf[C] { type Elem = A }
  private val instance: IsElementOf[Any] = new IsElementOf[Any] { type Elem = Any }
  private def of[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]

  // Map values traverse to the value type `V` (the `Map[_, _]` constructor is binary, so it does not match the unary
  // `fromFunctor` rule).
  implicit def map[K, V]: Aux[Map[K, V], V] = of

  // `Array[A]` does not unify against the unary `F[A]` shape of `fromFunctor` (Hearth/Scala treat it specially), so it
  // gets a dedicated rule. The runtime `arrayQuicklensFunctor` handles the rebuild reflectively.
  implicit def array[A]: Aux[Array[A], A] = of

  // Any unary container `F[A]` with a `QuicklensFunctor[F]` in scope — built-in (`List`/`Vector`/`Seq`/`Set`/`Option`)
  // or user-provided. `F` and `A` are unified from the *exact* container type `F[A]`, so `A` is the precise element type
  // (no covariant widening).
  implicit def fromFunctor[F[_], A](implicit @scala.annotation.unused F: QuicklensFunctor[F]): Aux[F[A], A] = of
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
