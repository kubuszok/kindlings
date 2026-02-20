package hearth.kindlings.circederivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsEncoder.LogDerivation]] in the scope to preview how the encoder derivation is done.
    *
    * Put outside of [[KindlingsEncoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsEncoder: KindlingsEncoder.LogDerivation = KindlingsEncoder.LogDerivation

  /** Import [[KindlingsDecoder.LogDerivation]] in the scope to preview how the decoder derivation is done.
    *
    * Put outside of [[KindlingsDecoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsDecoder: KindlingsDecoder.LogDerivation = KindlingsDecoder.LogDerivation
}
// $COVERAGE-ON$
