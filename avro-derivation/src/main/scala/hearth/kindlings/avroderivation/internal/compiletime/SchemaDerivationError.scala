package hearth.kindlings.avroderivation.internal.compiletime

sealed private[compiletime] trait SchemaDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object SchemaDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends SchemaDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any schema derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends SchemaDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends SchemaDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
  final case class AvroFixedOnNonByteArray(fieldName: String, tpeName: String, fieldType: String)
      extends SchemaDerivationError {
    override def message: String =
      s"@avroFixed on field '$fieldName' of $tpeName requires Array[Byte], but field type is $fieldType"
  }
  final case class ConflictingDefaultAnnotations(fieldName: String, tpeName: String) extends SchemaDerivationError {
    override def message: String =
      s"@avroNoDefault and @avroDefault cannot both be present on field '$fieldName' of $tpeName"
  }
}
