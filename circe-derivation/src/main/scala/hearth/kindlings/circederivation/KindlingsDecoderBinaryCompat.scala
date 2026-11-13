package hearth.kindlings.circederivation

import io.circe.{Decoder, Encoder}

/** Members of the `KindlingsDecoder` companion kept for binary compatibility of code compiled against Kindlings 0.3.x.
  *
  * A parent of `KindlingsDecoderCompanionCompat`, so that its `patch` (the macro) is defined in a subclass and wins the
  * overload resolution of `KindlingsDecoder.patch[A]` on both Scala 2 and Scala 3.
  */
private[circederivation] trait KindlingsDecoderBinaryCompat {

  /** The 0.3.x `patch`: the patch JSON is deep-merged into the encoded original, which is decoded back. */
  @deprecated(
    "kept for binary compatibility of code compiled against Kindlings 0.3.x; use KindlingsDecoder.patch[A] (with an implicit Configuration)",
    "0.3.4"
  )
  def patch[A](implicit decoder: Decoder[A], encoder: Encoder[A]): Decoder[A => A] =
    internal.runtime.CirceDerivationUtils.patchDecoder(decoder, encoder)
}
