package hearth.kindlings.avroderivation
package internal.compiletime

import hearth.MacroCommonsScala2

/** Thin delegate re-exporting the shared implementation from derivation-commons. */
trait AnnotationSupportScala2
    extends AnnotationSupport
    with hearth.kindlings.derivation.compiletime.AnnotationSupportScala2 { this: MacroCommonsScala2 => }
