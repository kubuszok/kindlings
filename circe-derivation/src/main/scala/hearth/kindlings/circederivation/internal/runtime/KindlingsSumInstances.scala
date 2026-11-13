package hearth.kindlings.circederivation.internal.runtime

/** Implemented by the encoders Kindlings derives for sum types (sealed traits/classes).
  *
  * When such an encoder is the instance of an intermediate sealed trait of a hierarchy (`sealed trait Parent extends
  * GrandParent`, with e.g. `implicit val codec = KindlingsCodecAsObject.derived[Parent]` in `Parent`'s companion), the
  * encoder derived for `GrandParent` writes its output as it is, without wrapping it under `"Parent"` or adding a
  * discriminator, like circe's `ConfiguredEncoder.encodeSum` does for a `ConfiguredEncoder` of a sum.
  */
trait KindlingsSumEncoder

/** Implemented by the decoders Kindlings derives for sum types (sealed traits/classes).
  *
  * When such a decoder is the instance of an intermediate sealed trait of a hierarchy, the decoder derived for the
  * outer sum delegates the constructor names it does not know itself to it, like circe's `ConfiguredDecoder` flattens
  * the constructors of a `ConfiguredDecoder` of a sum (`decodersDict`).
  */
trait KindlingsSumDecoder {

  /** Whether this decoder dispatches on `constructorName` (as transformed by its own configuration), either for one of
    * its own subtypes or through a nested [[KindlingsSumDecoder]].
    */
  def kindlingsHandlesConstructor(constructorName: String): Boolean
}
