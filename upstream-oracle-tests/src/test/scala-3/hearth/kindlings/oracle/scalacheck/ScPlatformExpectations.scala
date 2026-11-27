package hearth.kindlings.oracle.scalacheck

/** Expected differences between Kindlings and scalacheck-derived (Scala 3). Documented in
  * `docs/user-guide/feature-parity.md` (scalacheck-derivation, "Upstream oracle").
  */
object ScPlatformExpectations {

  val arbitrary: List[String] = List(
    // At size 0 scalacheck-derived fails (`Gen.fail`) for these sealed-trait-based types, even with a
    // `RecursionFallback`; Kindlings still generates a value (only the cases that cannot recurse), and needs no marker.
    "generates at every size [0]._3: kindlings=true upstream=false",
    "generates at every size [0]._5: kindlings=true upstream=false"
  )

  val values: List[String] = Nil

  val shrink: List[String] = Nil

  val cogen: List[String] = Nil
}
