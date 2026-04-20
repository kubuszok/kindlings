package hearth.kindlings.pureconfigderivation.internal.compiletime

trait PureconfigDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "pureconfigDerivation"
}
