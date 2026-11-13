package hearth.kindlings.circederivation.internal.compiletime

sealed private[compiletime] trait EncoderDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object EncoderDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any encoder derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends EncoderDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
  final case class EnumAsStringsWithNonSingletonCases(tpeName: String, nonSingletons: List[String])
      extends EncoderDerivationError {
    override def message: String =
      s"enumAsStrings is enabled but $tpeName has cases that are not singletons (${nonSingletons.mkString(", ")}); " +
        "only sums of case objects/parameterless enum cases can be encoded as strings (like circe's ConfiguredEnumCodec)"
  }
}
