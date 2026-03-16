package hearth.kindlings.ubjsonderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[UBJsonValueCodec.LogDerivation]] in the scope to preview how the codec derivation is done.
    *
    * Put outside of [[UBJsonValueCodec]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForUBJsonValueCodec: UBJsonValueCodec.LogDerivation =
    UBJsonValueCodec.LogDerivation
}
// $COVERAGE-ON$
