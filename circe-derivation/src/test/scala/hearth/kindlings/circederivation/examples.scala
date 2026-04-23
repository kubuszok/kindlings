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
object CirceAliases {
  type Name = String
}
case class WithAlias(name: CirceAliases.Name, age: Int)

class NotACirceType

// Non-case-class sealed trait leaves (Gap #11)
sealed trait MixedADT
case class CaseLeaf(x: Int) extends MixedADT
class PlainLeaf(val x: Int) extends MixedADT {
  override def equals(obj: Any): Boolean = obj match {
    case other: PlainLeaf => x == other.x
    case _                => false
  }
  override def hashCode(): Int = x.hashCode()
}

// WithInstant is in src/test/scalajvm (uses java.time, JVM-only)

// Option field test types
case class WithOptionalField(name: String, opt: Option[String])
case class WithOptionalAndDefault(name: String, opt: Option[String] = Some("default"))

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

// Collection test types
case class WithMutableBuffer(items: scala.collection.mutable.ArrayBuffer[Int])
case class WithVector(items: Vector[String])

// Higher-kinded type test
case class HigherKindedType[F[_]](value: F[Int])

// Non-String map key test types
case class WithIntKeyMap(data: Map[Int, String])
case class WithLongKeyMap(data: Map[Long, String])
final case class UserId(value: Int) extends AnyVal
case class WithUserIdKeyMap(users: Map[UserId, String])

// Consecutive capitals test types
case class HTMLParser(content: String)
case class HTTPSConnection(url: String, port: Int)

// Option+default test types
case class WithOptionAndDefault(name: String, opt: Option[String] = Some("default"))
case class WithOptionNoDefault(name: String, opt: Option[String])

// Multi-level sealed hierarchy test types
sealed trait GrandParent
sealed trait Parent extends GrandParent
case class Child(value: Int) extends Parent
case class Uncle(name: String) extends GrandParent

// Indirect recursion test types
case class RecursiveNode(id: String, children: List[RecursiveNode])
case class RecursiveParent(name: String, nodes: List[RecursiveNode])

// UTF-8 field name test types
case class CirceWithUtf8FieldNames(
    @fieldName("名前") name: String,
    @fieldName("données") data: Int,
    @fieldName("field with spaces") value: Boolean
)

// Recursive sealed trait for discriminator test
sealed trait RecursiveAnimal
case class Pack(name: String, members: List[RecursiveAnimal]) extends RecursiveAnimal
case class Lone(name: String) extends RecursiveAnimal

// Large case class (33 fields) for macro scalability stress test
case class LargeProduct(
    f01: Int,
    f02: Int,
    f03: Int,
    f04: Int,
    f05: Int,
    f06: Int,
    f07: Int,
    f08: Int,
    f09: Int,
    f10: Int,
    f11: Int,
    f12: Int,
    f13: Int,
    f14: Int,
    f15: Int,
    f16: Int,
    f17: Int,
    f18: Int,
    f19: Int,
    f20: Int,
    f21: Int,
    f22: Int,
    f23: Int,
    f24: Int,
    f25: Int,
    f26: Int,
    f27: Int,
    f28: Int,
    f29: Int,
    f30: Int,
    f31: Int,
    f32: Int,
    f33: String
)

// Large enum (33 variants) for macro scalability stress test
sealed trait LargeEnum
case object V01 extends LargeEnum; case object V02 extends LargeEnum
case object V03 extends LargeEnum; case object V04 extends LargeEnum
case object V05 extends LargeEnum; case object V06 extends LargeEnum
case object V07 extends LargeEnum; case object V08 extends LargeEnum
case object V09 extends LargeEnum; case object V10 extends LargeEnum
case object V11 extends LargeEnum; case object V12 extends LargeEnum
case object V13 extends LargeEnum; case object V14 extends LargeEnum
case object V15 extends LargeEnum; case object V16 extends LargeEnum
case object V17 extends LargeEnum; case object V18 extends LargeEnum
case object V19 extends LargeEnum; case object V20 extends LargeEnum
case object V21 extends LargeEnum; case object V22 extends LargeEnum
case object V23 extends LargeEnum; case object V24 extends LargeEnum
case object V25 extends LargeEnum; case object V26 extends LargeEnum
case object V27 extends LargeEnum; case object V28 extends LargeEnum
case object V29 extends LargeEnum; case object V30 extends LargeEnum
case object V31 extends LargeEnum; case object V32 extends LargeEnum
case object V33 extends LargeEnum

// Multi-level hierarchy with field name matching subtype name
sealed trait Transport
case class Car(speed: Int) extends Transport
case class Bicycle(speed: Int) extends Transport
case class Garage(car: Car, bicycle: Option[Bicycle]) extends Transport // "car" field name matches subtype name "Car"

// Generics with defaults
case class BoxWithDefault[A](value: A, label: String = "unlabeled")

// Option null vs absent interaction types
case class OptionMatrix(
    a: Option[String],
    b: Option[String] = Some("default-b"),
    c: String = "default-c"
)
