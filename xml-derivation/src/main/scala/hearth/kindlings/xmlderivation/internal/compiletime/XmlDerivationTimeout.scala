package hearth.kindlings.xmlderivation.internal.compiletime

trait XmlDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "xmlDerivation"
}
