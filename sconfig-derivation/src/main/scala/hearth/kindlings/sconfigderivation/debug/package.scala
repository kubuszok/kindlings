package hearth.kindlings.sconfigderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[ConfigReader.LogDerivation]] in scope to preview reader derivation. */
  implicit val logDerivationForConfigReader: ConfigReader.LogDerivation =
    ConfigReader.LogDerivation

  /** Import [[ConfigWriter.LogDerivation]] in scope to preview writer derivation. */
  implicit val logDerivationForConfigWriter: ConfigWriter.LogDerivation =
    ConfigWriter.LogDerivation
}
// $COVERAGE-ON$
