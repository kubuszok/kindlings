package hearth.kindlings.pureconfigderivation

import hearth.kindlings.pureconfigderivation.annotations.{configKey, transientField}

// --- Plain case classes ---

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])

// --- Recursive ---

case class RecursiveTree(value: Int, children: List[RecursiveTree])

// --- Value types ---

final case class WrappedInt(value: Int) extends AnyVal

// --- Sealed traits / enums ---

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

// Mixed sealed trait (case objects + case classes)
sealed trait MixedPet
case object Goldfish extends MixedPet
case class Budgie(name: String, canTalk: Boolean) extends MixedPet
case object Turtle extends MixedPet

// --- Annotations ---

case class WithConfigKey(@configKey("user_name") userName: String, age: Int)
case class WithTransient(name: String, @transientField cache: Option[String] = None)
case class WithBothAnnotations(
    @configKey("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

// --- Defaults ---

case class WithDefaults(name: String, age: Int = 25, active: Boolean = true)

// --- Nested ---

case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// --- Container types ---

case class WithOption(name: String, nickname: Option[String])
case class WithList(items: List[Int])
case class WithMap(scores: Map[String, Int])
