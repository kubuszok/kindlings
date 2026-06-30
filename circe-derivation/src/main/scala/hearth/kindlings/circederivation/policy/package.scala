package hearth.kindlings.circederivation

// $COVERAGE-OFF$
package object policy {

  /** Import this value to permit Circe (`KindlingsEncoder` / `KindlingsDecoder` / `KindlingsCodecAsObject`) structural
    * derivation in the current scope when the build runs the derivation policy in `opt-in` mode with
    * `optInByImport=true`:
    *
    * {{{
    * import hearth.kindlings.circederivation.policy.allowDerivationForCirceDerivation
    * }}}
    *
    * Has no effect when the policy is `always-allowed` (the default) or when the scope is already on
    * `-Xmacro-settings:circeDerivation.policy.allowedScopes`.
    */
  implicit val allowDerivationForCirceDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
