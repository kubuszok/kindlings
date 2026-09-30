package hearth.kindlings.integrationtests

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive

case class RefinedPerson(name: String Refined NonEmpty, age: Int Refined Positive)
case class WithRefinedOption(value: Option[Int Refined Positive])
case class WithRefinedList(values: List[String Refined NonEmpty])
case class WithRefinedKeyMap(values: Map[String Refined NonEmpty, Int])
case class WithRefinedIntKeyMap(values: Map[Int Refined Positive, Int])

// Plain surrogate for testing refined/iron validation rejection via binary codecs
case class PlainPerson(name: String, age: Int)
