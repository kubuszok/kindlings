package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

trait KindlingsJsonValueCodec[A] extends JsonValueCodec[A]
object KindlingsJsonValueCodec extends KindlingsJsonValueCodecCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.jsoniterderivation.debug.logDerivationForKindlingsJsonValueCodec]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
