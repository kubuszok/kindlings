package hearth.kindlings.circederivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait DecoderDerivationPolicyRuleImpl {
  this: DecoderMacrosImpl & MacroCommons & StdExtensions & AnnotationSupport =>

  /** Root rule for the derivation policy (issue kubuszok/kindlings#85): runs the single policy check once per
    * expansion, after the implicit/cache rules and before any derivation rule, then yields so derivation proceeds.
    */
  object DecoderDerivationPolicyRule extends DecoderDerivationRule("derivation policy") {
    def apply[A: DecoderCtx]: MIO[Rule.Applicability[Nothing]] =
      checkDerivationPolicyOncePerExpansion(Type[A].prettyPrint).map(_ => Rule.yielded())
  }
}
