package hearth.kindlings.pureconfigderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsConfigReader.LogDerivation]] in the scope to preview how the reader derivation is done.
    *
    * Put outside of [[KindlingsConfigReader]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsConfigReader: KindlingsConfigReader.LogDerivation =
    KindlingsConfigReader.LogDerivation

  /** Import [[KindlingsConfigWriter.LogDerivation]] in the scope to preview how the writer derivation is done.
    *
    * Put outside of [[KindlingsConfigWriter]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsConfigWriter: KindlingsConfigWriter.LogDerivation =
    KindlingsConfigWriter.LogDerivation
}
// $COVERAGE-ON$
