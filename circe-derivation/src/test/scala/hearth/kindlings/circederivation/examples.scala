package hearth.kindlings.circederivation

import hearth.kindlings.circederivation.annotations.{fieldName, transientField}

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

sealed trait Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, indoor: Boolean) extends Animal

sealed trait SimpleEnumCirce
case object Yes extends SimpleEnumCirce
case object No extends SimpleEnumCirce

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

case class CamelCaseFields(firstName: String, lastName: String)

case class PersonWithDefaults(name: String, age: Int = 25)
case class AllDefaults(x: Int = 1, y: String = "hello")

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object CirceAliases {
  type Name = String
}
case class WithAlias(name: CirceAliases.Name, age: Int)

class NotACirceType

// Annotation test types
case class CirceWithFieldName(
    @fieldName("user_name") userName: String,
    age: Int
)
case class CirceWithTransient(
    name: String,
    @transientField cache: Option[String] = None
)
case class CirceWithBothAnnotations(
    @fieldName("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)
