package hearth.kindlings.derivation.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

/** Per-macro-expansion guard so that [[Environment.loadStandardExtensions]] is invoked at most once even when a single
  * derivation chains together multiple entry points (e.g. a codec derivation that internally calls both the encoder and
  * the decoder entry point, or a decoder entry point that re-enters via a cached helper builder).
  *
  * Hearth deduplicates already-applied extensions internally, but each redundant call still pays the ServiceLoader cost
  * and emits a "skipping re-initialization" info log (issue kubuszok/kindlings#65).
  */
trait LoadStandardExtensionsOnce { this: MacroCommons & StdExtensions =>

  private var standardExtensionsLoaded: Boolean = false

  /** Set by [[ensureStandardExtensionsLoadedOutsideMioRun]]: the "Successfully loaded N extensions" log of that
    * loading, replayed by the next [[ensureStandardExtensionsLoaded]] inside the MIO run (whose logs are rendered).
    */
  private var pendingStandardExtensionsLog: Option[MIO[Unit]] = None

  /** Loads the standard Hearth extensions exactly once per macro bundle instance. Safe to call from any derivation
    * entry point; subsequent invocations are no-ops.
    */
  protected def ensureStandardExtensionsLoaded(): MIO[Unit] =
    if (standardExtensionsLoaded)
      MIO.pure(()).flatMap { _ =>
        pendingStandardExtensionsLog match {
          case Some(log) =>
            pendingStandardExtensionsLog = None
            log
          case None => MIO.pure(())
        }
      }
    else
      Environment.loadStandardExtensions().toMIO(allowFailures = false).map { _ =>
        standardExtensionsLoaded = true
        ()
      }

  /** [[ensureStandardExtensionsLoaded]] for code running BEFORE the derivation's MIO run (e.g. a check deciding which
    * entry point to use): loads the extensions immediately, and keeps their loading log so that the first
    * [[ensureStandardExtensionsLoaded]] inside the run still logs it (logs of a separate run would be lost).
    */
  protected def ensureStandardExtensionsLoadedOutsideMioRun(): Unit =
    if (!standardExtensionsLoaded) {
      val result = Environment.loadStandardExtensions()
      result.toMIO(allowFailures = false).unsafe.runSync: Unit
      standardExtensionsLoaded = true
      pendingStandardExtensionsLog = Some(result.toMIO(allowFailures = false).map(_ => ()))
    }
}
