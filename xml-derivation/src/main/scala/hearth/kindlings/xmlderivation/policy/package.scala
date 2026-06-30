package hearth.kindlings.xmlderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation in this scope under `opt-in` policy with `optInByImport=true`. */
  implicit val allowDerivationForXmlDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
