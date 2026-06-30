package hearth.kindlings.xmlderivation.internal.compiletime

import hearth.MacroCommons

// $COVERAGE-OFF$ macro-only (compile-time) policy glue
trait XmlDerivationPolicy extends hearth.kindlings.derivation.compiletime.DerivationPolicy { this: MacroCommons =>

  override protected def derivationOptInImportHint: String =
    "import hearth.kindlings.xmlderivation.policy.allowDerivationForXmlDerivation"

  override protected def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationT: Type[hearth.kindlings.xmlderivation.AllowDerivation] =
      Type.of[hearth.kindlings.xmlderivation.AllowDerivation]
    Expr.summonImplicit[hearth.kindlings.xmlderivation.AllowDerivation].isDefined
  }
}
// $COVERAGE-ON$
