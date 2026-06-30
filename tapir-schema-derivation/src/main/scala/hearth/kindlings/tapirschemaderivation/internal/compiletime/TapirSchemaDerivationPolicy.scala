package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$
trait TapirSchemaDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy {
  this: MacroCommons =>
  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.tapirschemaderivation.policy.allowDerivationForTapirSchemaDerivation"
  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.tapirschemaderivation.AllowDerivation] =
      Type.of[hearth.kindlings.tapirschemaderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.tapirschemaderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
