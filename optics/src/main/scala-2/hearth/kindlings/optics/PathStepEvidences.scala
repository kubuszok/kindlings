package hearth.kindlings.optics

import scala.language.experimental.macros

// The indexed (`.at`/`.index`/`.atOrElse`) and `.eachLeft`/`.eachRight` marker evidences. Each is materialized by a
// WHITEBOX macro (`val c: whitebox.Context` on the bundle) that consults Hearth's std SPI and computes the type
// member(s), so the step type-checks ONLY on a real provider-backed container, with the precise types taken from it.

/** Evidence that `C` is an indexed container whose index is `Idx` and element is `Elem` (a `Seq` keyed by `Int`, or a
  * `Map` keyed by its key). Materialized from `IsCollection`/`IsMap`.
  */
@scala.annotation.implicitNotFound(
  "`.at`/`.index`/`.atOrElse` is not supported on ${C} — it works on Seq (Int index) and Map (key)"
)
sealed trait IsIndexedElementOf[C] extends PathStepEvidence { type Idx; type Elem }

object IsIndexedElementOf {
  type Aux[C, I, A] = IsIndexedElementOf[C] { type Idx = I; type Elem = A }
  private val instance: IsIndexedElementOf[Any] = new IsIndexedElementOf[Any] { type Idx = Any; type Elem = Any }
  def witness[C, I, A]: Aux[C, I, A] = instance.asInstanceOf[Aux[C, I, A]]

  implicit def derived[C]: IsIndexedElementOf[C] = macro internal.compiletime.ModifyMacros.isIndexedElementOfImpl[C]
}

/** Evidence that `C` is a single-element container (an `Option`-like) whose element is `Elem`. Materialized from
  * `IsOption`. Used by the no-index `.at`/`.index`/`.atOrElse`.
  */
@scala.annotation.implicitNotFound("the no-index `.at`/`.index`/`.atOrElse` is only supported on Option-like ${C}")
sealed trait IsSingleElementOf[C] extends PathStepEvidence { type Elem }

object IsSingleElementOf {
  type Aux[C, A] = IsSingleElementOf[C] { type Elem = A }
  private val instance: IsSingleElementOf[Any] = new IsSingleElementOf[Any] { type Elem = Any }
  def witness[C, A]: Aux[C, A] = instance.asInstanceOf[Aux[C, A]]

  implicit def derived[C]: IsSingleElementOf[C] = macro internal.compiletime.ModifyMacros.isSingleElementOfImpl[C]
}

/** Evidence that `C` is an `Either[L, R]` whose left branch is `Left` and right branch is `Right`. Materialized from
  * `IsEither`. Used by `.eachLeft`/`.eachRight`.
  */
@scala.annotation.implicitNotFound("`.eachLeft`/`.eachRight` is only supported on Either, not ${C}")
sealed trait IsEither[C] extends PathStepEvidence { type Left; type Right }

object IsEither {
  type Aux[C, L, R] = IsEither[C] { type Left = L; type Right = R }
  private val instance: IsEither[Any] = new IsEither[Any] { type Left = Any; type Right = Any }
  def witness[C, L, R]: Aux[C, L, R] = instance.asInstanceOf[Aux[C, L, R]]

  implicit def derived[C]: IsEither[C] = macro internal.compiletime.ModifyMacros.isEitherImpl[C]
}
