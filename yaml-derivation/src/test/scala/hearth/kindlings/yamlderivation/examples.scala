package hearth.kindlings.yamlderivation

import hearth.kindlings.yamlderivation.annotations.{fieldName, transientField}

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

// Scala Enumeration
object ScalaColor extends Enumeration {
  val Red, Green, Blue = Value
}

case class CamelCasePerson(firstName: String, lastName: String)

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object YamlAliases {
  type Name = String
}
case class WithAlias(name: YamlAliases.Name, age: Int)

case class YamlWithFieldName(@fieldName("user_name") userName: String, age: Int)
case class YamlWithTransient(name: String, @transientField cache: Option[String] = None)
case class YamlWithBothAnnotations(
    @fieldName("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

// useDefaults test types
case class WithDefaults(name: String, age: Int = 25, active: Boolean = true)
case class AllDefaults(x: Int = 1, y: Int = 2)

// Higher-kinded type test
case class HigherKindedType[F[_]](value: F[Int])

// UTF-8 field name test types
case class YamlWithUtf8FieldNames(
    @fieldName("名前") name: String,
    @fieldName("données") data: Int,
    @fieldName("field with spaces") value: Boolean
)
