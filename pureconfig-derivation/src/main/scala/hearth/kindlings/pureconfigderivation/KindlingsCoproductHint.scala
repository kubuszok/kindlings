package hearth.kindlings.pureconfigderivation

import pureconfig.{ConfigFieldMapping, KebabCase, PascalCase}

/** Per-type override of the global [[PureConfig]] config for sealed-trait / enum derivation. The macro looks for an
  * implicit `KindlingsCoproductHint[A]` for every sealed family it derives; when present, it overrides the global
  * config's `discriminator` / `transformConstructorNames` for that specific type. The annotation `@configKey` on a
  * subtype always wins over the hint's `transformConstructorNames`.
  *
  * Three variants mirror upstream PureConfig:
  *
  *   - [[Field]] — discriminator-based encoding (matches upstream `FieldCoproductHint`)
  *   - [[Wrapped]] — single-key wrapping (kindlings-specific, but functionally equivalent to omitting the discriminator
  *     field by setting `PureConfig.discriminator = None`)
  *   - [[FirstSuccess]] — try every subtype reader in sequence (matches upstream `FirstSuccessCoproductHint`)
  *
  * Like [[KindlingsProductHint]], this trait does not have an implicit `default[A]`. The macro falls back to the global
  * [[PureConfig]] config when no per-type hint is in scope.
  */
sealed trait KindlingsCoproductHint[A] {
  def transformConstructorNames: String => String
}
object KindlingsCoproductHint {

  /** Discriminator-based encoding: each variant produces an object with an extra string field `fieldName` whose value
    * is `transformConstructorNames(variantClassName)`. Matches upstream PureConfig's `FieldCoproductHint`.
    */
  final case class Field[A](
      fieldName: String = "type",
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends KindlingsCoproductHint[A]

  /** Single-key wrapping: each variant is encoded as `{"variantName": {…fields…}}`. Use when the HOCON consumer expects
    * a tagged-union shape rather than a flat object with an embedded discriminator. There's no exact upstream
    * PureConfig analogue — users have to write a custom `CoproductHint` to get this shape with PureConfig.
    */
  final case class Wrapped[A](
      transformConstructorNames: String => String = ConfigFieldMapping(PascalCase, KebabCase)
  ) extends KindlingsCoproductHint[A]

  /** Try every subtype reader in order until one succeeds. There is no discriminator — the cursor is handed to each
    * subtype in turn, and the first one whose reader succeeds wins. Matches upstream PureConfig's
    * `FirstSuccessCoproductHint`.
    *
    * Use sparingly: it's quadratic in the number of subtypes and produces hard-to-read error messages when nothing
    * matches.
    */
  final case class FirstSuccess[A]() extends KindlingsCoproductHint[A] {
    override def transformConstructorNames: String => String = identity
  }
}
