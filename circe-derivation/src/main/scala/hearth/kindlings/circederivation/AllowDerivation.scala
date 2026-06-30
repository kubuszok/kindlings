package hearth.kindlings.circederivation

/** Opt-in marker for the [[https://github.com/kubuszok/kindlings/issues/85 derivation policy]].
  *
  * When the build runs the policy in `opt-in` mode with `optInByImport=true`, importing an implicit of this type (see
  * [[hearth.kindlings.circederivation.policy.allowDerivationForCirceDerivation]]) permits structural derivation of
  * `KindlingsEncoder` / `KindlingsDecoder` / `KindlingsCodecAsObject` in the current scope.
  *
  * Kept OUTSIDE the type classes' implicit scope so it is never summoned automatically.
  */
sealed trait AllowDerivation
object AllowDerivation extends AllowDerivation
