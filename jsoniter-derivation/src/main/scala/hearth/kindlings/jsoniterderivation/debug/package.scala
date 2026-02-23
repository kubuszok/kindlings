package hearth.kindlings.jsoniterderivation

// $COVERAGE-OFF$
package object debug {

  /** Import [[KindlingsJsonValueCodec.LogDerivation]] in the scope to preview how the codec derivation is done.
    *
    * Put outside of [[KindlingsJsonValueCodec]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForKindlingsJsonValueCodec: KindlingsJsonValueCodec.LogDerivation =
    KindlingsJsonValueCodec.LogDerivation
}
// $COVERAGE-ON$
