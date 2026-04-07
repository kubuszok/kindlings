package hearth.kindlings.sconfigderivation

/** Per-type override of the global [[SConfig]] config for sealed-trait / enum derivation.
  * The macro looks for an implicit `CoproductHint[A]` for every sealed family it derives;
  * when present, it overrides the global config's `discriminator` /
  * `transformConstructorNames` for that specific type. The annotation `@configKey` on a
  * subtype always wins over the hint's `transformConstructorNames`.
  *
  * Three variants mirror upstream PureConfig:
  *
  *  - [[Field]] — discriminator-based encoding (matches upstream `FieldCoproductHint`)
  *  - [[Wrapped]] — single-key wrapping
  *  - [[FirstSuccess]] — try every subtype reader in sequence (matches upstream
  *    `FirstSuccessCoproductHint`)
  */
sealed trait CoproductHint[A] {
  def transformConstructorNames: String => String
}
object CoproductHint {

  /** Discriminator-based encoding: each variant produces an object with an extra string
    * field `fieldName` whose value is `transformConstructorNames(variantClassName)`.
    */
  final case class Field[A](
      fieldName: String = "type",
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends CoproductHint[A]

  /** Single-key wrapping: each variant is encoded as `{"variantName": {…fields…}}`. */
  final case class Wrapped[A](
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends CoproductHint[A]

  /** Try every subtype reader in order until one succeeds. */
  final case class FirstSuccess[A]() extends CoproductHint[A] {
    override def transformConstructorNames: String => String = identity
  }
}
