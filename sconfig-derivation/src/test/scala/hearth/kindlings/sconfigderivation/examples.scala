package hearth.kindlings.sconfigderivation

import hearth.kindlings.sconfigderivation.annotations.{configKey, transientField}

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])

final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

case class WithConfigKey(@configKey("user_name") userName: String, age: Int)
case class WithTransient(name: String, @transientField cache: Option[String] = None)
case class WithBothAnnotations(
    @configKey("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

case class WithDefaults(name: String, age: Int = 25, active: Boolean = true)

case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

case class WithOption(name: String, nickname: Option[String])
case class WithList(items: List[Int])
case class WithMap(scores: Map[String, Int])
