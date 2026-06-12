package hearth.kindlings.ubjsonderivation
package internal.compiletime

import hearth.MacroCommonsScala3

/** Thin delegate re-exporting the shared implementation from derivation-commons. */
trait AnnotationSupportScala3
    extends AnnotationSupport
    with hearth.kindlings.derivation.compiletime.AnnotationSupportScala3 { this: MacroCommonsScala3 => }
