package hearth.kindlings.avroderivation

import hearth.kindlings.avroderivation.annotations.{avroDefault, avroDoc, avroNamespace, fieldName, transientField}

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

// Avro-specific annotation test types
@avroDoc("A documented person record")
case class DocumentedPerson(
    @avroDoc("The person's full name") name: String,
    @avroDoc("Age in years") age: Int
)

@avroNamespace("com.example.custom")
case class CustomNamespacePerson(name: String, age: Int)

@avroDoc("A record with custom namespace")
@avroNamespace("com.example.docs")
case class FullyAnnotatedRecord(
    @avroDoc("The identifier") id: String,
    value: Int
)

// Default value annotation test types
case class WithDefaults(
    name: String,
    @avroDefault("0") age: Int = 0,
    @avroDefault("\"unknown\"") role: String = "unknown"
)

case class WithOptionalDefault(
    name: String,
    @avroDefault("null") nickname: Option[String] = None
)

// Case class with logical types
case class EventRecord(
    id: java.util.UUID,
    timestamp: java.time.Instant,
    date: java.time.LocalDate,
    time: java.time.LocalTime,
    localTimestamp: java.time.LocalDateTime
)

// Scala Enumeration
object ScalaColor extends Enumeration {
  val Red, Green, Blue = Value
}

// BigDecimal and Either test types
case class WithBigDecimal(amount: BigDecimal)
case class WithEither(value: Either[String, Int])
case class WithEitherRecord(value: Either[String, SimplePerson])

// Collection test types
case class WithMutableBuffer(items: scala.collection.mutable.ArrayBuffer[Int])
case class WithVector(items: Vector[String])

// Unhandled type for compile-time error tests
class NotAnAvroType
