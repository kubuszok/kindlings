package hearth.kindlings.oracle.scalacheck

/** Expected differences between Kindlings and scalacheck-shapeless (Scala 2). Kindlings has one derivation for both
  * Scala versions; where scalacheck-shapeless and scalacheck-derived (Scala 3) disagree it follows scalacheck-derived.
  * Documented in `docs/user-guide/feature-parity.md` (scalacheck-derivation, "Upstream oracle").
  */
object ScPlatformExpectations {

  val arbitrary: List[String] = Nil

  val shrink: List[String] = List(
    // scalacheck-shapeless also shrinks a sealed-trait value into the other subtypes (e.g. `ScCircle(4)` first to
    // `ScOrigin`); Kindlings (like scalacheck-derived) only shrinks within the value's own subtype.
    "shrink shape: kindlings and upstream differ",
    "shrink wrapper: kindlings and upstream differ",
    "shrink shape keeps the subtype [0]: kindlings=true upstream=false",
    "shrink shape keeps the subtype [1]: kindlings=true upstream=false",
    "shrink shape keeps the subtype [2]: kindlings=true upstream=false"
  )

  val cogen: List[String] = List(
    // Sealed traits: scalacheck-shapeless advances the seed once per preceding subtype and then perturbs it with the
    // subtype's Cogen; Kindlings (like scalacheck-derived) perturbs with the subtype's Cogen and then with its index.
    // Products, options and collections perturb exactly like scalacheck-shapeless.
    "cogen shape seeds: kindlings and upstream differ",
    "cogen tree seeds: kindlings and upstream differ"
  )
}
