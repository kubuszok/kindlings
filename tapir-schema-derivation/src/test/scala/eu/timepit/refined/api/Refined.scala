package eu.timepit.refined.api

// A stand-in with the name and the shape of refined's `Refined` (an AnyVal with a private constructor), to check the
// error hint pointing at kindlings-refined-integration without depending on refined (IntegrationHintSpec).
final class Refined[T, P] private (val value: T) extends AnyVal
object Refined {
  def unsafeApply[T, P](value: T): Refined[T, P] = new Refined[T, P](value)
}
