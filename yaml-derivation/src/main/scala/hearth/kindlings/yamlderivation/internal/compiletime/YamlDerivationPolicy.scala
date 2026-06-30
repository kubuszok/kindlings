package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$
trait YamlDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>

  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.yamlderivation.policy.allowDerivationForYamlDerivation"

  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.yamlderivation.AllowDerivation] =
      Type.of[hearth.kindlings.yamlderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.yamlderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
