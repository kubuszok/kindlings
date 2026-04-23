package hearth.kindlings.scalacheckderivation

import org.scalacheck.Shrink as ScalaCheckShrink

object DeriveShrink {

  /** Derives a Shrink instance for type A.
    *
    * Supports:
    *   - Case classes (shrinks each field independently)
    *   - Sealed traits / enums (shrinks the current variant)
    *   - Collections and Option types
    *   - Singletons (empty shrink stream)
    *
    * @tparam A the type for which to derive a Shrink instance
    * @return a Shrink[A] instance
    */
  inline def derived[A]: ScalaCheckShrink[A] = ${ internal.compiletime.ShrinkMacros.deriveShrink[A] }
}
