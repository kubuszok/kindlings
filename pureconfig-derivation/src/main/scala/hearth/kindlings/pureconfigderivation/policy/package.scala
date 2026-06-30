package hearth.kindlings.pureconfigderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForPureconfigDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
