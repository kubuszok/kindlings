package hearth.kindlings.policytest.model

// Plain data types shared by the policy integration tests. Defining a case class does NOT trigger derivation,
// so these are safe to declare in any scope regardless of the active policy.

final case class Allowed1(a: Int, b: String)

final case class Imported1(a: Int)

final case class Denied1(a: Int, b: String)
