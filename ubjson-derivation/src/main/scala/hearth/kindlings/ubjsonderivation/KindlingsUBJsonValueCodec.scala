package hearth.kindlings.ubjsonderivation

trait KindlingsUBJsonValueCodec[A] extends UBJsonValueCodec[A]
object KindlingsUBJsonValueCodec extends KindlingsUBJsonValueCodecCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.ubjsonderivation.debug.logDerivationForKindlingsUBJsonValueCodec]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
