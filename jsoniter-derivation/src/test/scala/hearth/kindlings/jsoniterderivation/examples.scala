package hearth.kindlings.jsoniterderivation

import hearth.kindlings.jsoniterderivation.annotations.{fieldName, stringified, transientField}

case class CamelCasePerson(firstName: String, lastName: String)
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

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object JsoniterAliases {
  type Name = String
}
case class WithAlias(name: JsoniterAliases.Name, age: Int)

// Per-field annotation test types
case class JsoniterWithFieldName(@fieldName("user_name") userName: String, age: Int)
case class JsoniterWithTransient(name: String, @transientField cache: Option[String] = None)
case class JsoniterWithBothAnnotations(
    @fieldName("display_name") displayName: String,
    @transientField internal: Int = 0,
    active: Boolean
)

// Higher-kinded type test
case class HigherKindedType[F[_]](value: F[Int])

// Non-String map key test types
case class WithIntKeyMap(data: Map[Int, String])
case class WithLongKeyMap(data: Map[Long, String])
final case class UserId(value: Int) extends AnyVal
case class WithUserIdKeyMap(users: Map[UserId, String])

// @stringified test types
case class WithStringifiedInt(@stringified value: Int, name: String)
case class WithStringifiedLong(@stringified id: Long, label: String)
case class WithStringifiedBigDecimal(@stringified amount: BigDecimal)
case class WithMixedStringified(@stringified count: Int, name: String, @stringified score: Double)

// isStringified (global config) test type
case class WithNumericFields(count: Int, score: Double, name: String)

// circeLikeObjectEncoding test type (sealed trait with mixed case objects and case classes)
sealed trait MixedEnum
case object Pending extends MixedEnum
case object Done extends MixedEnum
case class InProgress(progress: Int) extends MixedEnum

// transientDefault / transientEmpty / transientNone test types
case class WithDefaultFields(name: String, age: Int = 25, active: Boolean = true)
case class WithOptionFields(name: String, email: Option[String] = None, phone: Option[String] = None)
case class WithCollectionFields(name: String, tags: List[String] = Nil, scores: Map[String, Int] = Map.empty)
case class WithMixedTransient(
    name: String,
    age: Int = 0,
    email: Option[String] = None,
    tags: List[String] = Nil
)

// BigDecimal / DoS protection test types
case class WithBigDecimalField(value: BigDecimal)
case class WithBigIntField(value: BigInt)
case class WithMapField(data: Map[String, Int])
case class WithListField(items: List[Int])

// UTF-8 field name test types
case class JsoniterWithUtf8FieldNames(
    @fieldName("名前") name: String,
    @fieldName("données") data: Int,
    @fieldName("field with spaces") value: Boolean
)

// Feature interaction test types
case class WithRenamedOption(@fieldName("e_mail") email: Option[String] = None, name: String)
case class WithStringifiedAndFieldName(@stringified @fieldName("item_count") count: Int, label: String)
case class AllOptionalWithDefaults(x: Int = 1, y: String = "hello", z: Boolean = true)
case class DeeplyNested1(a: DeeplyNested2)
case class DeeplyNested2(b: DeeplyNested3)
case class DeeplyNested3(c: DeeplyNested4)
case class DeeplyNested4(d: DeeplyNested5)
case class DeeplyNested5(value: Int)
