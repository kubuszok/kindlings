package hearth.kindlings.sconfigderivation

/** Per-type override of the global [[SConfig]] config for sealed-trait / enum derivation. The macro looks for an
  * implicit `CoproductHint[A]` for every sealed family it derives; when present, it overrides the global config's
  * `discriminator` / `transformConstructorNames` for that specific type. The annotation `@configKey` on a subtype
  * always wins over the hint's `transformConstructorNames`.
  *
  * Three variants mirror upstream PureConfig:
  *
  *   - [[Field]] — discriminator-based encoding (matches upstream `FieldCoproductHint`)
  *   - [[Wrapped]] — single-key wrapping
  *   - [[FirstSuccess]] — no disambiguation information (matches upstream `FirstSuccessCoproductHint`)
  *
  * Declare a hint with its concrete variant as the type (`implicit val hint: CoproductHint.FirstSuccess[Source] =
  * CoproductHint.FirstSuccess()`), so that the macro finds it.
  */
sealed trait CoproductHint[A] {
  def transformConstructorNames: String => String
}
object CoproductHint {

  /** Discriminator-based encoding: each variant produces an object with an extra string field `fieldName` whose value
    * is `transformConstructorNames(variantClassName)`.
    */
  final case class Field[A](
      fieldName: String = "type",
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends CoproductHint[A]

  /** Single-key wrapping: each variant is encoded as `{"variantName": {…fields…}}`. */
  final case class Wrapped[A](
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends CoproductHint[A]

  /** Matches upstream PureConfig's `FirstSuccessCoproductHint`: no disambiguation information. Reading attempts every
    * subtype on the same value, in the alphabetical order of the subtypes' names (the order upstream's derivation
    * passes them to the hint, not the declaration order), and the first one read successfully wins; when none can be
    * read, the failure is [[ConfigDecodingError.NoValidCoproductOptionFound]] listing every attempt's failure. Writing
    * writes the subtype's value as is.
    *
    * Every attempt reads the value, so the cost grows with the number of subtypes, and a subtype that accepts the input
    * (e.g. a case object, which reads any object) shadows the subtypes after it.
    */
  final case class FirstSuccess[A]() extends CoproductHint[A] {
    override def transformConstructorNames: String => String = identity
  }
}
