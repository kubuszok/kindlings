package hearth.kindlings.oracle.catsderivation

/** Expected Kindlings-vs-kittens differences on Scala 3 (kittens' Scala 2 and Scala 3 derivations are separate
  * implementations that render zero-field types differently; Kindlings renders them the same on both). Documented in
  * `docs/user-guide/feature-parity.md` (cats-derivation, "Upstream oracle").
  */
object CtPlatformExpectations {
  val show: List[String] = List(
    // kittens 3's `Show` renders a case class without fields and a case object by name only (its `ShowPretty`, and
    // kittens 2's `Show`, keep the parentheses); Kindlings renders `Name()` in both type classes on both versions.
    "show no fields [0]: kindlings=CtNoFields() kittens=CtNoFields",
    "show shape [4]: kindlings=CtMOrigin() kittens=CtMOrigin"
  )
  val showPretty: List[String] = Nil
}
