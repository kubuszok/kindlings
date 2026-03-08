package hearth.kindlings.xmlderivation

sealed abstract class XmlDecodingError(val message: String) extends Exception(message) {
  override def toString: String = s"XmlDecodingError: $message"
}
object XmlDecodingError {
  final case class MissingAttribute(attributeName: String, elementName: String)
      extends XmlDecodingError(s"Missing attribute '$attributeName' in element <$elementName>")

  final case class MissingElement(childName: String, parentName: String)
      extends XmlDecodingError(s"Missing child element <$childName> in <$parentName>")

  final case class InvalidValue(value: String, expectedType: String, context: String)
      extends XmlDecodingError(s"Invalid value '$value' for type $expectedType in $context")

  final case class UnexpectedElement(elementName: String, context: String)
      extends XmlDecodingError(s"Unexpected element <$elementName> in $context")

  final case class MissingContent(elementName: String)
      extends XmlDecodingError(s"Missing text content in element <$elementName>")

  final case class UnknownDiscriminator(discriminatorValue: String, knownSubtypes: List[String])
      extends XmlDecodingError(
        s"Unknown discriminator value '$discriminatorValue'. Expected one of: ${knownSubtypes.mkString(", ")}"
      )

  final case class MissingDiscriminator(attributeName: String, elementName: String)
      extends XmlDecodingError(s"Missing discriminator attribute '$attributeName' in element <$elementName>")

  final case class Multiple(errors: List[XmlDecodingError])
      extends XmlDecodingError(s"Multiple errors:\n${errors.map(e => s"  - ${e.message}").mkString("\n")}")

  final case class General(msg: String) extends XmlDecodingError(msg)
}
