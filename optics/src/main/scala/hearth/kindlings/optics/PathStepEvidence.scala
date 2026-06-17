package hearth.kindlings.optics

// ALL traversal/indexed steps (`.each`/`.eachWhere`, `.at`/`.index`/`.atOrElse`, `.eachLeft`/`.eachRight`) are handled
// by the `modify` macro directly through Hearth's `IsCollection`/`IsMap`/`IsOption`/`IsEither` SPI — there are NO runtime
// functor type classes. The compile-time marker evidences that let the path lambda type-check
// (`IsElementOf`/`IsIndexedElementOf`/`IsSingleElementOf`/`IsEither`) are each materialized by a whitebox (Scala 2) /
// transparent inline (Scala 3) macro that consults the SPI; they live in the per-platform `IsElementOf.scala` and
// `PathStepEvidences.scala` files. Only their common (non-parameterised) supertype is shared here.

/** Common (non-parameterised) supertype of the indexed/option/either marker evidences, so the macro can recognise a
  * synthesized evidence value argument by a single `Type.isSubtypeOf[T, PathStepEvidence]` check (an invariant
  * `IsXxx[C]` is not a subtype of `IsXxx[Any]`, so a constructor-agnostic marker is needed). NOT `sealed`: the
  * evidences extend it from the per-platform `PathStepEvidences.scala` files.
  */
trait PathStepEvidence
