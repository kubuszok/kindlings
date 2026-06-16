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

  // A single, element-type-agnostic `Array` functor: rebuilding an array needs a `ClassTag`, but rather than requiring
  // one per element type (which the macro cannot supply when summoning for the erased constructor), the result array is
  // built reflectively. `.each`/`modify` keeps the element type, so the output reuses the *input* array's component type
  // (e.g. `int` for an `Array[Int]`), and `java.lang.reflect.Array.set` unboxes into primitive arrays as needed.
  implicit val arrayQuicklensFunctor: QuicklensFunctor[Array] = new QuicklensFunctor[Array] {
    def each[A, B](fa: Array[A])(f: A => B): Array[B] = {
      val componentType = fa.getClass.getComponentType
      val out = java.lang.reflect.Array.newInstance(componentType, fa.length)
      var i = 0
      while (i < fa.length) { java.lang.reflect.Array.set(out, i, f(fa(i)).asInstanceOf[AnyRef]); i += 1 }
      out.asInstanceOf[Array[B]]
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
}

/** Evidence that `C` is a container whose traversable element (for `.each`) is the type member `Elem`. `Elem` is fixed
  * *invariantly* by the matching `given`, so it cannot be widened to a supertype the way a bare `F[A]` element would be.
  * Instances cover the unary functors (`Seq`/`List`/`Vector`/`Set`/`Option`/`Array` via `QuicklensFunctor`) and map
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
