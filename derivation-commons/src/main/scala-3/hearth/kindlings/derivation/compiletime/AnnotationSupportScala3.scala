package hearth.kindlings.derivation.compiletime

import hearth.MacroCommonsScala3

/** Obsolete since Hearth issue #283: [[AnnotationSupport]] is now fully cross-platform (implemented on Hearth's typed
  * annotation API), so no Scala 3-specific overrides are required. Retained only as an empty alias so derivation
  * modules that still mix in `AnnotationSupportScala3` keep compiling; new code should mix in [[AnnotationSupport]]
  * directly and this trait (and its module re-exports) can then be deleted. See `circe-derivation` for the fully
  * migrated form.
  */
trait AnnotationSupportScala3 extends AnnotationSupport { this: MacroCommonsScala3 => }
