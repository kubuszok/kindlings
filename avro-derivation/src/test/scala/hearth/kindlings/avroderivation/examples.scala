package hearth.kindlings.avroderivation

import hearth.kindlings.avroderivation.annotations.{
  avroAlias,
  avroDefault,
  avroDoc,
  avroError,
  avroFixed,
  avroNamespace,
  avroProp,
  avroSortPriority,
  fieldName,
  transientField
}

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

// @avroFixed test types
final case class WithFixedBytes(@avroFixed(4) id: Array[Byte])
final case class WithFixedAndRegularBytes(@avroFixed(16) token: Array[Byte], data: Array[Byte])

// @avroError test types
@avroError
case class AvroErrorRecord(code: Int, message: String)

// ByteBuffer test types
case class WithByteBuffer(data: java.nio.ByteBuffer)

// @avroSortPriority test types
sealed trait PrioritizedShape
@avroSortPriority(2)
case class PCircle(radius: Double) extends PrioritizedShape
@avroSortPriority(1)
case class PRectangle(width: Double, height: Double) extends PrioritizedShape

sealed trait PrioritizedEnum
@avroSortPriority(3)
case object PAlpha extends PrioritizedEnum
@avroSortPriority(1)
case object PBeta extends PrioritizedEnum
@avroSortPriority(2)
case object PGamma extends PrioritizedEnum

// @avroProp test types
@avroProp("custom-key", "custom-value")
case class WithClassProp(name: String)

case class WithFieldProp(@avroProp("field-key", "field-value") name: String, age: Int)

@avroProp("key1", "val1")
@avroProp("key2", "val2")
case class WithMultipleProps(name: String)

// @avroAlias test types
@avroAlias("OldPersonName")
case class AliasedRecord(name: String, age: Int)

case class WithFieldAlias(@avroAlias("old_name") name: String, age: Int)

@avroAlias("V1Name")
@avroAlias("V2Name")
case class MultiAliasRecord(name: String)

// Custom field name test types (Avro field names must match [A-Za-z_][A-Za-z0-9_]*)
case class AvroWithCustomFieldNames(
    @fieldName("person_name") name: String,
    @fieldName("data_value") data: Int,
    @fieldName("is_active") value: Boolean
)

// Unhandled type for compile-time error tests
class NotAnAvroType
