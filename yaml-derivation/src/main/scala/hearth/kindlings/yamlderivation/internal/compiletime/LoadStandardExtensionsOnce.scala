package hearth.kindlings.yamlderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Per-macro-expansion guard so that [[Environment.loadStandardExtensions]] is invoked at most once even when a single
  * derivation chains together multiple entry points (e.g. the codec derivation that internally calls both the encoder
  * and the decoder entry point).
  *
  * Hearth deduplicates already-applied extensions internally, but each redundant call still pays the ServiceLoader cost
  * and emits a "skipping re-initialization" info log (issue kubuszok/kindlings#65).
  */
trait LoadStandardExtensionsOnce { this: MacroCommons & StdExtensions =>

  private var standardExtensionsLoaded: Boolean = false

  /** Loads the standard Hearth extensions exactly once per macro bundle instance. Safe to call from any derivation
    * entry point; subsequent invocations are no-ops.
    */
  protected def ensureStandardExtensionsLoaded(): MIO[Unit] =
    if (standardExtensionsLoaded) MIO.pure(())
    else
      Environment.loadStandardExtensions().toMIO(allowFailures = false).map { _ =>
        standardExtensionsLoaded = true
        ()
      }
}
