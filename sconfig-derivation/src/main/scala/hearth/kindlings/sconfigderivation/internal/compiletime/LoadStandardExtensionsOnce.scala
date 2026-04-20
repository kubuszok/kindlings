package hearth.kindlings.sconfigderivation.internal.compiletime

trait LoadStandardExtensionsOnce extends hearth.kindlings.derivation.compiletime.LoadStandardExtensionsOnce {
  this: hearth.MacroCommons & hearth.std.StdExtensions =>
}
