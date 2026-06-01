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

// --- Value class ---

case class WithWrappedInt(value: WrappedInt)

// --- Recursive Option ---

case class LinkedNode(value: String, next: Option[LinkedNode])

// --- Complex defaults ---

case class WithComplexDefaults(
    name: String,
    tags: List[String] = List("default"),
    scores: Map[String, Int] = Map("init" -> 0)
)

// --- Option with None default ---

case class WithOptionDefault(name: String, extra: Option[String] = None)

// --- Recursive sealed trait ---

sealed trait TreeNode
case class Branch(value: Int, left: TreeNode, right: TreeNode) extends TreeNode
case class Leaf(value: Int) extends TreeNode

// --- Nested / complex maps ---

case class WithNestedMap(data: Map[String, Map[String, Int]])
case class WithMapOfCaseClass(data: Map[String, SimplePerson])

// --- Discriminator field collision ---

case class WithTypeField(`type`: String, name: String)

// --- Multi-word field names (for name transform tests) ---

case class MultiWordFields(firstName: String, lastName: String, postalCode: Int)
case class WithDefaultsMultiWord(firstName: String, middleName: String = "N/A", age: Int = 0)

// --- Sealed traits for constructor name transform tests ---

sealed trait ColorChoice
case class BrightRed(intensity: Int) extends ColorChoice
case class DarkBlue(intensity: Int) extends ColorChoice
case object PaleGreen extends ColorChoice

// --- Combinatorial: wrapper x inner type ---
// Inner types (SimplePerson, Shape) have no pre-existing reader/writer instances;
// the macro must derive them recursively (bug #120 pattern).

case class CombOuter(
    optPrimitive: Option[Int],
    optCaseClass: Option[SimplePerson],
    optSealedTrait: Option[Shape],
    listCaseClass: List[SimplePerson],
    mapCaseClass: Map[String, SimplePerson]
)

// --- Annotation x type shape ---
// Sealed traits whose subtypes carry field-level annotations.

sealed trait AnnotatedShape
case class AnnotatedCircle(@configKey("r") radius: Double) extends AnnotatedShape
case class AnnotatedRect(
    @configKey("w") width: Double,
    @configKey("h") height: Double
) extends AnnotatedShape

sealed trait TransientShape
case class TransientCircle(radius: Double, @transientField memo: String = "") extends TransientShape
case class TransientRect(width: Double, height: Double, @transientField memo: String = "") extends TransientShape
