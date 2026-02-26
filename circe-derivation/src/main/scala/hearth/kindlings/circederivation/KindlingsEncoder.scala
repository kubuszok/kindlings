package hearth.kindlings.circederivation

import io.circe.{Encoder, Json, JsonObject}

trait KindlingsEncoder[A] extends Encoder[A] {
  def apply(a: A): Json
}

trait KindlingsEncoderAsObject[A] extends KindlingsEncoder[A] with Encoder.AsObject[A] {
  def encodeObject(a: A): JsonObject
}

object KindlingsEncoder extends KindlingsEncoderCompanionCompat {

  /** Special type - if its implicit is in scope then macros will log the derivation process.
    *
    * @see
    *   [[hearth.kindlings.circederivation.debug.logDerivationForKindlingsEncoder]] for details
    */
  sealed trait LogDerivation
  object LogDerivation extends LogDerivation
}
