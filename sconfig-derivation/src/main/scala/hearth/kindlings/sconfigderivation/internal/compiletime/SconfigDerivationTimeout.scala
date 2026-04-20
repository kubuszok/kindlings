package hearth.kindlings.sconfigderivation.internal.compiletime

trait SconfigDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "sconfigDerivation"
}
