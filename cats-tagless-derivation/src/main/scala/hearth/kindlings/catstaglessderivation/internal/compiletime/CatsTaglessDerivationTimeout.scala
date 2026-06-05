package hearth.kindlings.catstaglessderivation.internal.compiletime

trait CatsTaglessDerivationTimeout extends hearth.kindlings.derivation.compiletime.DerivationTimeout {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "catsTaglessDerivation"
}
