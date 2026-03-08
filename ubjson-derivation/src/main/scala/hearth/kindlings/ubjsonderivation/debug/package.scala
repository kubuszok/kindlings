package hearth.kindlings.ubjsonderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsUBJsonValueCodec.LogDerivation]] in the scope to preview how the codec derivation is done.
    *
    * Put outside of [[KindlingsUBJsonValueCodec]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsUBJsonValueCodec: KindlingsUBJsonValueCodec.LogDerivation =
    KindlingsUBJsonValueCodec.LogDerivation
}
// $COVERAGE-ON$
