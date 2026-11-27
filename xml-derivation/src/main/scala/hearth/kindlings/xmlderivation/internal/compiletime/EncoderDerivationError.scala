package hearth.kindlings.xmlderivation.internal.compiletime

sealed abstract private[compiletime] class EncoderDerivationError(val message: String) extends Exception(message)
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(typeName: String, reasons: List[String])
      extends EncoderDerivationError(
        s"Cannot derive XmlEncoder for type $typeName:\n${reasons.mkString("\n")}"
      )
  final case class InvalidFieldAnnotations(typeName: String, errors: List[String])
      extends EncoderDerivationError(
        s"Cannot derive XmlEncoder for type $typeName:\n${errors.map(" - " + _).mkString("\n")}"
      )
  final case class EnumAsStringsWithNonSingletonCases(typeName: String, nonSingletons: List[String])
      extends EncoderDerivationError(
        s"enumAsStrings is enabled but $typeName has cases that are not singletons (${nonSingletons.mkString(", ")}); " +
          "only enums of case objects/parameterless enum cases (and Java enums, Enumerations) can be written as " +
          "strings. Derive the instance for this type with a configuration without enumAsStrings (and put it in " +
          "the implicit scope, so that it is used where the type is a field)"
      )
}
