package hearth.kindlings.tapirschemaderivation

// $COVERAGE-OFF$
package object policy {
  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForTapirSchemaDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
