package hearth.kindlings.catsderivation.internal.compiletime

trait CatsDerivationTimeout
    extends hearth.kindlings.derivation.compiletime.DerivationTimeout
    with CatsDerivationPolicy {
  this: hearth.MacroCommons =>
  override protected def derivationSettingsNamespace: String = "catsDerivation"
}
