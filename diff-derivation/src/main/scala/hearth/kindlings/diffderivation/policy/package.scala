package hearth.kindlings.diffderivation

// $COVERAGE-OFF$
package object policy {
  /** Import to permit structural derivation in the current scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForDiffDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
