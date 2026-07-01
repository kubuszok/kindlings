package hearth.kindlings.fastshowpretty.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait FastShowPrettyDerivationPolicyRuleImpl {
  this: FastShowPrettyMacrosImpl & MacroCommons & StdExtensions =>

  /** Root rule for the derivation policy (issue kubuszok/kindlings#85): runs the single policy check once per
    * expansion, after the implicit/cache rules and before any derivation rule, then yields so derivation proceeds.
    */
  object FastShowPrettyDerivationPolicyRule extends DerivationRule("derivation policy") {
    def apply[A: DerivationCtx]: MIO[Rule.Applicability[Nothing]] =
      checkDerivationPolicyOncePerExpansion(Type[A].prettyPrint).map(_ => Rule.yielded())
  }
}
