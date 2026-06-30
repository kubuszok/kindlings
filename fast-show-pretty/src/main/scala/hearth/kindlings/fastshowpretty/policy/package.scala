package hearth.kindlings.fastshowpretty

// $COVERAGE-OFF$
package object policy {

  /** Import this value to permit `FastShowPretty` structural derivation in the current scope when the build runs the
    * derivation policy in `opt-in` mode with `optInByImport=true`:
    *
    * {{{
    * import hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty
    * }}}
    *
    * Has no effect when the policy is `always-allowed` (the default) or when the scope is already on
    * `-Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes`.
    */
  implicit val allowDerivationForFastShowPretty: FastShowPretty.AllowDerivation = FastShowPretty.AllowDerivation
}
// $COVERAGE-ON$
