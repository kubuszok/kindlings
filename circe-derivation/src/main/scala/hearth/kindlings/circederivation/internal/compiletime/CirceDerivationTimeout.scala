package hearth.kindlings.circederivation.internal.compiletime

trait CirceDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "circeDerivation"
}
