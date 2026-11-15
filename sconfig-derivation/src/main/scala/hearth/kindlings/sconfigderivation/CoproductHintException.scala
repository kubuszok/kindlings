package hearth.kindlings.sconfigderivation

/** Thrown by derived writers when a sealed family's subtype cannot be written with the coproduct hint (matches
  * PureConfig's `pureconfig.generic.error.CoproductHintException`): with a discriminator field, a subtype that has a
  * field named like the discriminator ([[ConfigDecodingError.CollidingKeys]]), or whose value is not an object
  * ([[ConfigDecodingError.WrongType]]).
  */
final case class CoproductHintException(failure: ConfigDecodingError) extends RuntimeException {
  override def getMessage: String = failure.message
}
