package hearth.kindlings.pureconfigderivation.internal.compiletime

import hearth.MacroCommons
import hearth.std.*

/** Thin delegate re-exporting the shared implementation from derivation-commons. */
trait AnnotationSupport extends hearth.kindlings.derivation.compiletime.AnnotationSupport {
  this: MacroCommons & StdExtensions =>
}
