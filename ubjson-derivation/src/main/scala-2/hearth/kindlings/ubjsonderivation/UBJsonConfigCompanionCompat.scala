package hearth.kindlings.ubjsonderivation

/** Scala 2 members of the `UBJsonConfig` companion. */
private[ubjsonderivation] trait UBJsonConfigCompanionCompat {

  /** The `unapply` of Kindlings 0.3.2: Scala 2 synthesizes no `unapply` for a case class of more than 22 fields, which
    * `UBJsonConfig` has had since. Matches the fields of 0.3.2 only.
    */
  @deprecated("kept for binary compatibility of code compiled against Kindlings 0.3.x; use the fields", "0.3.4")
  def unapply(config: UBJsonConfig): Option[
    (
        String => String,
        String => String,
        Option[String],
        Boolean,
        Boolean,
        Boolean,
        Boolean,
        Boolean,
        Boolean,
        Boolean,
        Boolean,
        Int,
        Int,
        Int,
        Int,
        Int
    )
  ] =
    if (config == null) None
    else
      Some(
        (
          config.fieldNameMapper,
          config.adtLeafClassNameMapper,
          config.discriminatorFieldName,
          config.skipUnexpectedFields,
          config.enumAsStrings,
          config.transientDefault,
          config.transientEmpty,
          config.transientNone,
          config.requireCollectionFields,
          config.requireDefaultFields,
          config.checkFieldDuplication,
          config.bigDecimalPrecision,
          config.bigDecimalScaleLimit,
          config.bigDecimalDigitsLimit,
          config.mapMaxInsertNumber,
          config.setMaxInsertNumber
        )
      )
}
