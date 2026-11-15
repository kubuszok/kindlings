package neotype

// A stand-in with the name and the shape of neotype's `Newtype` (an opaque `Type` declared in the companion's parent),
// to check the error hint pointing at kindlings-neotype-integration without depending on neotype
// (IntegrationHintScala3Spec).
abstract class Newtype[A] {
  opaque type Type = A
  def unsafeMake(value: A): Type = value
}
