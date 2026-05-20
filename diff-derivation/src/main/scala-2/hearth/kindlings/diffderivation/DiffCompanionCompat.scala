package hearth.kindlings.diffderivation

import scala.language.experimental.macros

private[diffderivation] trait DiffCompanionCompat { this: Diff.type =>

  implicit def derived[A]: Diff[A] = macro internal.compiletime.DiffMacros.deriveTypeClassImpl[A]
}
