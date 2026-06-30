package hearth.kindlings.catstaglessderivation

// $COVERAGE-OFF$
package object policy {

  /** Import to permit structural derivation of the cats-tagless type classes in the current scope when the build runs
    * the derivation policy in `opt-in` mode with `optInByImport=true`:
    *
    * {{{
    * import hearth.kindlings.catstaglessderivation.policy.allowDerivationForCatsTaglessDerivation
    * }}}
    *
    * Has no effect when the policy is `always-allowed` (the default) or when the scope is already on
    * `-Xmacro-settings:catsTaglessDerivation.policy.allowedScopes`.
    */
  implicit val allowDerivationForCatsTaglessDerivation: AllowDerivation = AllowDerivation
}
// $COVERAGE-ON$
