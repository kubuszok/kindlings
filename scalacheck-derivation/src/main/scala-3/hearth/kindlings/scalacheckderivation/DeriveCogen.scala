package hearth.kindlings.scalacheckderivation

import org.scalacheck.Cogen as ScalaCheckCogen

object DeriveCogen {

  /** Derives a Cogen instance for type A.
    *
    * Supports:
    *   - Case classes (perturbs seed with each field)
    *   - Sealed traits / enums (perturbs seed based on ordinal and variant)
    *   - Collections and Option types
    *   - Singletons (identity seed)
    *
    * @tparam A the type for which to derive a Cogen instance
    * @return a Cogen[A] instance
    */
  inline def derived[A]: ScalaCheckCogen[A] = ${ internal.compiletime.CogenMacros.deriveCogen[A] }
}
