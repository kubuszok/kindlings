package hearth.kindlings.optics

// ALL traversal/indexed steps (`.each`/`.eachWhere`, `.at`/`.index`/`.atOrElse`, `.eachLeft`/`.eachRight`) are handled
// by the `modify` macro directly through Hearth's `IsCollection`/`IsMap`/`IsOption`/`IsEither` SPI — there are NO runtime
// functor type classes. What remains here are only the invariant compile-time marker evidences that let the path lambda
// type-check (the macro decides real support and does the rebuild).

// `IsElementOf` (the `.each` evidence) lives in the per-platform `IsElementOf.scala` files: it is materialized by a
// whitebox (Scala 2) / transparent inline (Scala 3) given that consults the std SPI, so `.each` type-checks ONLY when the
// container actually has an `IsCollection`/`IsMap`/`IsOption` provider, with the element type taken from it.

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

  // Any immutable `Seq`-like `F[A]`, keyed by `Int`. The macro rebuilds positionally via `IsCollection`.
  implicit def seq[F[X] <: scala.collection.immutable.Seq[X], A]: Aux[F[A], Int, A] = of
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

  // `Option` only; the macro handles it via `IsOption`.
  implicit def option[A]: Aux[Option[A], A] = of
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
