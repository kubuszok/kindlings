package hearth.kindlings.jsoniterderivation

// $COVERAGE-OFF$
package object policy {
  /** Import to permit structural derivation in the current scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForJsoniterDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
