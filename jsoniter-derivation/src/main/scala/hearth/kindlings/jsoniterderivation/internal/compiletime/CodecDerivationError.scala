package hearth.kindlings.jsoniterderivation.internal.compiletime

sealed private[compiletime] trait CodecDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object CodecDerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends CodecDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any codec derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class TransientFieldMissingDefault(fieldName: String, tpeName: String) extends CodecDerivationError {
    override def message: String =
      s"@transientField on field '$fieldName' of $tpeName requires a default value"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends CodecDerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
  final case class CannotConstructType(tpeName: String, isSingleton: Boolean, constructorError: Option[String] = None)
      extends CodecDerivationError {
    override def message: String = {
      val prefix =
        if (isSingleton) s"Cannot construct singleton $tpeName" else s"Cannot construct $tpeName"
      constructorError.fold(prefix)(err => s"$prefix: $err")
    }
  }
  final case class UnexpectedParameterInSingleton(tpeName: String, context: String) extends CodecDerivationError {
    override def message: String = s"$context: $tpeName"
  }
  final case class StringifiedOnNonNumeric(fieldName: String, tpeName: String, fieldTypeName: String)
      extends CodecDerivationError {
    override def message: String =
      s"@stringified on field '$fieldName' of $tpeName requires a numeric type, but found $fieldTypeName"
  }
}
