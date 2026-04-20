package hearth.kindlings.avroderivation.internal.compiletime

trait AvroDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "avroDerivation"
}
