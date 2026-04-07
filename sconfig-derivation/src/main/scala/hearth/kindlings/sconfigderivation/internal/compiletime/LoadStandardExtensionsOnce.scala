package hearth.kindlings.sconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.fp.effect.*
import hearth.std.*

trait LoadStandardExtensionsOnce { this: MacroCommons & StdExtensions =>

  private var standardExtensionsLoaded: Boolean = false

  protected def ensureStandardExtensionsLoaded(): MIO[Unit] =
    if (standardExtensionsLoaded) MIO.pure(())
    else
      Environment.loadStandardExtensions().toMIO(allowFailures = false).map { _ =>
        standardExtensionsLoaded = true
        ()
      }
}
