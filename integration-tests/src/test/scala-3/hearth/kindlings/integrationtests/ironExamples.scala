package hearth.kindlings.integrationtests

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.string.*

case class IronPerson(name: String :| Not[Blank], age: Int :| Positive)
case class WithIronOption(value: Option[Int :| Positive])
