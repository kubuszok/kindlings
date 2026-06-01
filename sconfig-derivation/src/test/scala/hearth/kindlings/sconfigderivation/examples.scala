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

// --- Recursive sealed trait ---

sealed trait TreeNode
case class Branch(value: Int, left: TreeNode, right: TreeNode) extends TreeNode
case class Leaf(value: Int) extends TreeNode

case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

case class WithOption(name: String, nickname: Option[String])
case class WithList(items: List[Int])
case class WithMap(scores: Map[String, Int])

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
