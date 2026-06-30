package hearth.kindlings.yamlderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForYamlDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
