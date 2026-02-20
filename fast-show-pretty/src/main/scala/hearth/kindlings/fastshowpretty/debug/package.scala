package hearth.kindlings.fastshowpretty

// $COVERAGE-OFF$
package object debug {

  /** Import [[FastShowPretty.LogDerivation]] in the scope to preview how the derivation is done.
    *
    * Put outside of [[FastShowPretty]] companion to prevent the implicit from being summoned automatically!
    */
  implicit val logDerivationForFastShowPretty: FastShowPretty.LogDerivation = FastShowPretty.LogDerivation
}
// $COVERAGE-ON$
