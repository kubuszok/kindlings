package hearth.kindlings.avroderivation

import hearth.kindlings.avroderivation.annotations.{fieldName, transientField}

case class SimplePerson(name: String, age: Int)
case class EmptyClass()
case class SingleField(value: Int)
case class Address(street: String, city: String)
case class PersonWithAddress(name: String, age: Int, address: Address)
case class TeamWithMembers(name: String, members: List[SimplePerson])
case class RecursiveTree(value: Int, children: List[RecursiveTree])
final case class WrappedInt(value: Int) extends AnyVal

sealed trait Shape
case class Circle(radius: Double) extends Shape
case class Rectangle(width: Double, height: Double) extends Shape

sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object AvroAliases {
  type Name = String
}
case class WithAlias(name: AvroAliases.Name, age: Int)

// Per-field annotation test types
case class AvroWithFieldName(@fieldName("user_name") userName: String, age: Int)
case class AvroWithTransient(name: String, @transientField cache: Option[String] = None)
case class AvroWithBothAnnotations(
    @fieldName("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

// Case class with logical types
case class EventRecord(
    id: java.util.UUID,
    timestamp: java.time.Instant,
    date: java.time.LocalDate,
    time: java.time.LocalTime,
    localTimestamp: java.time.LocalDateTime
)
