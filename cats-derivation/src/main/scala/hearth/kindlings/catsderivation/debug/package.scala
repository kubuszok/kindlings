package hearth.kindlings.catsderivation

// $COVERAGE-OFF$
package object debug {

  /** Import in scope to preview how the derivation is done for cats type classes. */
  implicit val logDerivationForCatsDerivation: LogDerivation = LogDerivation.instance
}
// $COVERAGE-ON$
