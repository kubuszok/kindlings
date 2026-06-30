package hearth.kindlings.sconfigderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForSconfigDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
