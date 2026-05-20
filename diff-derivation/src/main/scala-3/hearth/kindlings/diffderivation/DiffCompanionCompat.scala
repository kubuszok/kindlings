package hearth.kindlings.diffderivation

private[diffderivation] trait DiffCompanionCompat { this: Diff.type =>

  inline given derived[A]: Diff[A] = ${ internal.compiletime.DiffMacros.deriveTypeClassImpl[A] }
}
