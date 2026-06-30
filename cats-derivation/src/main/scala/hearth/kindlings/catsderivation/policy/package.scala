package hearth.kindlings.catsderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation of the cats/alleycats type classes in the current scope when the build runs
    * the derivation policy in `opt-in` mode with `optInByImport=true`:
    *
    * {{{
    * import hearth.kindlings.catsderivation.policy.allowDerivationForCatsDerivation
    * }}}
    *
    * Has no effect when the policy is `always-allowed` (the default) or when the scope is already on
    * `-Xmacro-settings:catsDerivation.policy.allowedScopes`.
    */
  implicit val allowDerivationForCatsDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
