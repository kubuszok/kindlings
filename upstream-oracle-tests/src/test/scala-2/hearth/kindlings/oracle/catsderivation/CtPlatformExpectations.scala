package hearth.kindlings.oracle.catsderivation

/** Expected Kindlings-vs-kittens differences on Scala 2 (kittens' Scala 2 and Scala 3 derivations are separate
  * implementations that render zero-field types differently; Kindlings renders them the same on both). Documented in
  * `docs/user-guide/feature-parity.md` (cats-derivation, "Upstream oracle").
  */
object CtPlatformExpectations {
  val show: List[String] = List(
    // kittens 2 names a case object by its runtime class (`getSimpleName`, which ends with `$`).
    "show shape [4]: kindlings=CtMOrigin() kittens=CtMOrigin$()"
  )
  val showPretty: List[String] = List(
    // kittens 2's `ShowPretty` puts the parentheses of a type without fields on two lines (and names a case object by
    // its runtime class); Kindlings renders `Name()` like kittens 3.
    "showPretty no fields [0]: kindlings=List(CtNoFields()) kittens=List(CtNoFields(, ))",
    "showPretty shape [4]: kindlings=List(CtMOrigin()) kittens=List(CtMOrigin$(, ))"
  )
}
