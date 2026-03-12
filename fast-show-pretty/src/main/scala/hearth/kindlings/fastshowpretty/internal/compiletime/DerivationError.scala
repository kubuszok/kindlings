package hearth.kindlings.fastshowpretty.internal.compiletime

sealed private[compiletime] trait DerivationError extends util.control.NoStackTrace with Product with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object DerivationError {
  final case class UnsupportedType(tpeName: String, reasons: List[String]) extends DerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any derivation rule:\n${reasons.mkString("\n")}"
  }
  final case class NoChildrenInSealedTrait(tpeName: String) extends DerivationError {
    override def message: String =
      s"The type $tpeName does not have any children!"
  }
}
