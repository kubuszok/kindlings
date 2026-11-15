package io.github.iltotore.iron

// A stand-in with the name and the shape of Iron's `RefinedType` (an opaque `T` declared in the companion's parent), to
// check the error hint pointing at kindlings-iron-integration without depending on Iron (IntegrationHintScala3Spec).
trait RefinedType[A, C] {
  opaque type T = A
  def assume(value: A): T = value
}
