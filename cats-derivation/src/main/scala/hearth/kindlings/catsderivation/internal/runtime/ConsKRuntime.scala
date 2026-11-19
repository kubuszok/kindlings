package hearth.kindlings.catsderivation.internal.runtime

/** Runtime helper for ConsK derivation.
  *
  * Wraps `alleycats.ConsK.cons` calls to avoid higher-kinded type issues in macro-generated code. At JVM runtime, all
  * type parameters are erased so the casts are no-ops.
  */
object ConsKRuntime {

  private type AnyF[X] = Any

  // Erased to `AnyF`: casting the field to `List[Any]` (as it used to) threw a ClassCastException for any other `G`.
  def cons(consKInstance: Any, hd: Any, tl: Any): Any =
    consKInstance.asInstanceOf[alleycats.ConsK[AnyF]].cons[Any](hd, tl)
}
