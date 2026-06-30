package hearth.kindlings.policytest.allowed

import hearth.kindlings.fastshowpretty.FastShowPretty
import hearth.kindlings.policytest.model.Allowed1

/** This object lives in package `hearth.kindlings.policytest.allowed`, which the test module configures as an allowed
  * scope via `-Xmacro-settings:fastShowPrettyDerivation.policy.allowedScopes=hearth.kindlings.policytest.allowed`.
  *
  * Structural derivation here must therefore succeed even though the build runs the policy in `opt-in` mode. If the
  * policy gate were wrong, THIS FILE WOULD FAIL TO COMPILE - that is itself a positive assertion.
  */
object AllowedInstances {
  val instance: FastShowPretty[Allowed1] = FastShowPretty.derived[Allowed1]
}
