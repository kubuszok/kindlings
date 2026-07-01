package hearth.kindlings.avroderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait AvroEncoderDerivationPolicyRuleImpl {
  this: EncoderMacrosImpl & MacroCommons & StdExtensions & SchemaForMacrosImpl & AnnotationSupport =>

  /** Root rule for the derivation policy (issue kubuszok/kindlings#85): runs the single policy check once per
    * expansion, after the implicit/cache rules and before any derivation rule, then yields so derivation proceeds.
    */
  object AvroEncoderDerivationPolicyRule extends EncoderDerivationRule("derivation policy") {
    def apply[A: EncoderCtx]: MIO[Rule.Applicability[Nothing]] =
      checkDerivationPolicyOncePerExpansion(Type[A].prettyPrint).map(_ => Rule.yielded())
  }
}
