package hearth.kindlings.policytest.viaimport

import hearth.kindlings.fastshowpretty.FastShowPretty
// Bringing the opt-in marker into scope permits structural derivation here even though this package is NOT on
// `allowedScopes` - the build also sets `fastShowPrettyDerivation.policy.optInByImport=true`.
import hearth.kindlings.fastshowpretty.policy.allowDerivationForFastShowPretty
import hearth.kindlings.policytest.model.Imported1

object ImportedInstances {
  // Public reference to the marker so `-Wunused`/`-Werror` builds do not flag the import as unused
  // (the macro's compile-time implicit summon does not count as a use to scalac's import linter).
  val allowMarker: FastShowPretty.AllowDerivation = allowDerivationForFastShowPretty

  val instance: FastShowPretty[Imported1] = FastShowPretty.derived[Imported1]
}
