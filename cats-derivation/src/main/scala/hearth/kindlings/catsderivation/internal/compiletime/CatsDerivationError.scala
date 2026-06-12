package hearth.kindlings.catsderivation.internal.compiletime

/** Macro-time derivation errors shared by all cats-derivation macros.
  *
  * Routed through the MIO error channel (`Log.error(err.message) >> MIO.fail(err)`) instead of throwing, so that the
  * compiler reports a proper derivation error rather than crashing.
  */
sealed private[compiletime] trait CatsDerivationError
    extends util.control.NoStackTrace
    with Product
    with Serializable {
  def message: String
  override def getMessage(): String = message
}
private[compiletime] object CatsDerivationError {

  /** A probe type (e.g. `F[Int]`, `F[A]`, an enum child) could not be parsed as a case class. */
  final case class CannotParseCaseClass(tpeName: String, reason: String) extends CatsDerivationError {
    override def message: String = s"Cannot parse $tpeName as a case class: $reason"
  }

  /** A type could not be parsed as an enum (sealed trait / Scala 3 enum). */
  final case class CannotParseEnum(tpeName: String, reason: String) extends CatsDerivationError {
    override def message: String = s"Cannot parse $tpeName as an enum: $reason"
  }

  /** Calling the primary constructor (or another instance-free method) failed. */
  final case class CannotConstructResult(tpeName: String, reason: String) extends CatsDerivationError {
    override def message: String = s"Cannot construct $tpeName: $reason"
  }

  /** No implicit instance could be summoned (nor derived) for a field. */
  final case class MissingInstanceForField(
      typeClassName: String,
      fieldName: String,
      ownerName: String,
      reason: Option[String] = None
  ) extends CatsDerivationError {
    override def message: String =
      s"Cannot find or derive $typeClassName for field $fieldName in $ownerName" +
        reason.fold("")(r => s": $r")
  }

  /** A self-recursive field was classified, but no self-instance reference was provided to the body builder. */
  final case class MissingSelfReference(typeClassName: String) extends CatsDerivationError {
    override def message: String =
      s"Self-recursive field encountered but no self $typeClassName reference is available"
  }

  /** A field's shape is not supported by the derivation (e.g. not direct/invariant/nested in classification). */
  final case class UnsupportedFieldShape(typeClassName: String, fieldName: String, detail: String)
      extends CatsDerivationError {
    override def message: String =
      s"Unsupported field shape for $typeClassName derivation at field '$fieldName': $detail"
  }

  /** Enum dispatch found a child for which no derivation info was prepared. */
  final case class MissingDerivationInfo(typeClassName: String, childName: String) extends CatsDerivationError {
    override def message: String = s"No $typeClassName derivation info for enum child $childName"
  }

  /** Enum has no children to match on. */
  final case class EnumHasNoChildren(tpeName: String) extends CatsDerivationError {
    override def message: String = s"Enum $tpeName has no children to match on"
  }

  /** Catch-all for shapes that do not fit the cases above. */
  final case class DerivationFailed(typeClassName: String, detail: String) extends CatsDerivationError {
    override def message: String = s"$typeClassName derivation failed: $detail"
  }
}
