package hearth.kindlings.xmlderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsXmlEncoder.LogDerivation]] in the scope to preview how the encoder derivation is done.
    *
    * Put outside of [[KindlingsXmlEncoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsXmlEncoder: KindlingsXmlEncoder.LogDerivation =
    KindlingsXmlEncoder.LogDerivation

  /** Import [[KindlingsXmlDecoder.LogDerivation]] in the scope to preview how the decoder derivation is done.
    *
    * Put outside of [[KindlingsXmlDecoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsXmlDecoder: KindlingsXmlDecoder.LogDerivation =
    KindlingsXmlDecoder.LogDerivation
}
// $COVERAGE-ON$
