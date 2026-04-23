package hearth.kindlings.scalacheckderivation

import org.scalacheck.Shrink as ScalaCheckShrink
import scala.language.experimental.macros

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
  def derived[A]: ScalaCheckShrink[A] = macro internal.compiletime.ShrinkMacros.deriveShrinkImpl[A]
}
