package hearth.kindlings.yamlderivation.internal.compiletime

trait YamlDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "yamlDerivation"
}
