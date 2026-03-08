package hearth.kindlings.xmlderivation

import hearth.kindlings.xmlderivation.annotations.{transientField, xmlAttribute, xmlContent, xmlElement, xmlName}

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

sealed trait CardinalDirection
case object North extends CardinalDirection
case object South extends CardinalDirection
case object East extends CardinalDirection
case object West extends CardinalDirection

case class CamelCasePerson(firstName: String, lastName: String)

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Annotation tests
case class XmlWithFieldName(@xmlName("user_name") userName: String, age: Int)
case class XmlWithTransient(name: String, @transientField cache: Option[String] = None)
case class XmlWithBothAnnotations(
    @xmlName("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

// Attribute mode tests
case class XmlWithAttributes(
    @xmlAttribute name: String,
    @xmlAttribute age: Int,
    address: Address
)

// Content mode tests
case class XmlWithContent(
    @xmlAttribute id: Int,
    @xmlContent body: String
)

// Mixed mode tests
case class XmlMixed(
    @xmlAttribute id: String,
    @xmlElement description: String,
    tags: List[String]
)

// useDefaults test types
case class WithDefaults(name: String, age: Int = 25, active: Boolean = true)
case class AllDefaults(x: Int = 1, y: Int = 2)

// Higher-kinded type test
case class HigherKindedType[F[_]](value: F[Int])

// Mixed sealed trait (case objects + case classes)
sealed trait MixedPet
case object Goldfish extends MixedPet
case class Budgie(name: String, canTalk: Boolean) extends MixedPet
case object Turtle extends MixedPet

// Multi-level sealed hierarchy
sealed trait XmlVehicle
sealed trait XmlMotorized extends XmlVehicle
case class XmlCar(brand: String) extends XmlMotorized
case class XmlTruck(payload: Int) extends XmlMotorized
case class XmlBicycle(gears: Int) extends XmlVehicle

// Wrapper for List[Shape]
case class ShapeCollection(shapes: List[Shape])
