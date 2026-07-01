package hearth.kindlings.tapirschemaderivation.internal.compiletime
package rules

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

import hearth.kindlings.jsonschemaconfigs.JsonSchemaConfigs

trait SchemaDerivationPolicyRuleImpl {
  this: SchemaMacrosImpl & MacroCommons & StdExtensions & JsonSchemaConfigs & AnnotationSupport =>

  /** Root rule for the derivation policy (issue kubuszok/kindlings#85): runs the single policy check once per
    * expansion, after the implicit/cache rules and before any derivation rule, then yields so derivation proceeds.
    */
  object SchemaDerivationPolicyRule extends SchemaDerivationRule("derivation policy") {
    def apply[A: SchemaCtx]: MIO[Rule.Applicability[Nothing]] =
      checkDerivationPolicyOncePerExpansion(Type[A].prettyPrint).map(_ => Rule.yielded())
  }
}
