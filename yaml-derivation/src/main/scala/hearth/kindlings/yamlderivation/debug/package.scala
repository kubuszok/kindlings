package hearth.kindlings.yamlderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsYamlEncoder.LogDerivation]] in the scope to preview how the encoder derivation is done.
    *
    * Put outside of [[KindlingsYamlEncoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsYamlEncoder: KindlingsYamlEncoder.LogDerivation =
    KindlingsYamlEncoder.LogDerivation

  /** Import [[KindlingsYamlDecoder.LogDerivation]] in the scope to preview how the decoder derivation is done.
    *
    * Put outside of [[KindlingsYamlDecoder]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsYamlDecoder: KindlingsYamlDecoder.LogDerivation =
    KindlingsYamlDecoder.LogDerivation
}
// $COVERAGE-ON$
