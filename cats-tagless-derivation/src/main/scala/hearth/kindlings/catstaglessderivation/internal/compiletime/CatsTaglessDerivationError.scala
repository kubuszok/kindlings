package hearth.kindlings.catstaglessderivation.internal.compiletime

sealed private[compiletime] trait CatsTaglessDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object CatsTaglessDerivationError {

  /** No derivation rule handled the algebra type. */
  final case class UnsupportedType(typeClass: String, tpeName: String, reasons: List[String])
      extends CatsTaglessDerivationError {
    override def message: String =
      s"The type $tpeName was not handled by any $typeClass derivation rule:\n${reasons.mkString("\n")}"
  }

  /** The algebra is neither a case class nor a trait, so no derivation strategy applies. */
  final case class NotCaseClassOrTrait(typeClass: String, tpeName: String) extends CatsTaglessDerivationError {
    override def message: String =
      s"Cannot derive $typeClass for $tpeName: type is neither a case class nor a trait"
  }

  /** A probe/source/target application of the algebra (e.g. Alg[Option]) could not be parsed as a case class or as an
    * anonymous instance.
    */
  final case class CannotParseAlgebra(algebra: String, reason: String) extends CatsTaglessDerivationError {
    override def message: String = s"Cannot parse $algebra: $reason"
  }

  /** A case class field has a shape that the type class being derived cannot handle. */
  final case class UnsupportedField(typeClass: String, fieldName: String, reason: String)
      extends CatsTaglessDerivationError {
    override def message: String = s"Cannot derive $typeClass: field '$fieldName' $reason"
  }

  /** A trait method has a shape that the type class being derived cannot handle. */
  final case class UnsupportedMethod(typeClass: String, methodName: String, reason: String)
      extends CatsTaglessDerivationError {
    override def message: String = s"Cannot derive $typeClass for trait: method '$methodName' $reason"
  }

  /** A method required by the target instance was not found on the source instance. */
  final case class SourceMethodNotFound(methodName: String, details: String = "")
      extends CatsTaglessDerivationError {
    override def message: String = s"Source method $methodName not found$details"
  }

  /** Building the forwarding call (Method.fold) for a trait method failed. */
  final case class MethodForwardingFailed(methodName: String, reason: String) extends CatsTaglessDerivationError {
    override def message: String = s"Cannot build forwarding call for method '$methodName': $reason"
  }

  /** Constructing the final type class instance (case class construction or anonymous instance) failed. */
  final case class CannotConstructResult(typeClass: String, reason: String) extends CatsTaglessDerivationError {
    override def message: String = s"Cannot construct $typeClass result: $reason"
  }
}
