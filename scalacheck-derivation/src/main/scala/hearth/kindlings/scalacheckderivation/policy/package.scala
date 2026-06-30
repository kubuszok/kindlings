package hearth.kindlings.scalacheckderivation

// $COVERAGE-OFF$
package object policy {
  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForScalacheckDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
