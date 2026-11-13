package hearth.kindlings.ubjsonderivation

import hearth.kindlings.ubjsonderivation.annotations.{fieldName, transientField}

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

// Generic case classes
case class Box[A](value: A)
case class Pair[A, B](first: A, second: B)

// Deeply nested (3 levels)
case class GeoCoordinates(lat: Double, lon: Double)
case class FullAddress(street: String, city: String, geo: GeoCoordinates)
case class PersonFull(name: String, address: FullAddress)

// Type alias
object UBJsonAliases {
  type Name = String
}
case class WithAlias(name: UBJsonAliases.Name, age: Int)

// Per-field annotation test types
case class UBJsonWithFieldName(@fieldName("user_name") userName: String, age: Int)
case class UBJsonWithTransient(name: String, @transientField cache: Option[String] = None)
case class UBJsonWithBothAnnotations(
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

// Feature interaction test types
case class WithRenamedOption(@fieldName("e_mail") email: Option[String] = None, name: String)
case class AllOptionalWithDefaults(x: Int = 1, y: String = "hello", z: Boolean = true)

// Nested structure test types
case class NestedLists(matrix: List[List[Int]])
case class OptionalList(items: Option[List[Int]])

// Value class edge cases
case class WrappedString(value: String) extends AnyVal
case class WithOptionalWrapped(item: Option[WrappedInt])
case class WithWrappedList(items: List[WrappedInt])

// Mixed enum (sealed trait with both case objects and case classes)
sealed trait MixedEnum
case object Pending extends MixedEnum
case object Done extends MixedEnum
case class InProgress(progress: Int) extends MixedEnum

// CamelCase test
case class CamelCasePerson(firstName: String, lastName: String)

// Byte/Short boundaries
case class ByteBoundaries(min: Byte, max: Byte)
case class ShortBoundaries(min: Short, max: Short)

// Unicode content
case class UnicodeContent(value: String)

// Recursive sealed trait hierarchy
sealed trait TreeNode
case class Branch(left: TreeNode, right: TreeNode) extends TreeNode
case class Leaf(value: Int) extends TreeNode

// Mutual recursion
case class MutRecA(label: String, b: Option[MutRecB] = None)
case class MutRecB(count: Int, a: Option[MutRecA] = None)

// Indirect recursion test types
case class RecursiveNode(id: String, children: List[RecursiveNode])
case class RecursiveParent(name: String, nodes: List[RecursiveNode])

// Recursive types: isolating Option vs plain field
case class Recursive3Plain(id: String, extra: String, children: List[Recursive3Plain] = Nil)
case class Recursive3Option(id: String, extra: Option[String] = None, children: List[Recursive3Option] = Nil)

// Combinatorial wrapper x inner type test types
// NOTE: no pre-existing codecs for CombInner* — macro must derive recursively
case class CombInnerCC(label: String, count: Int)

sealed trait CombInnerST
case class CombVariantA(x: Int) extends CombInnerST
case class CombVariantB(text: String) extends CombInnerST

case class CombOuter(
    optPrimitive: Option[Int],
    optCaseClass: Option[CombInnerCC],
    optSealedTrait: Option[CombInnerST],
    listCaseClass: List[CombInnerCC],
    mapCaseClass: Map[String, CombInnerCC]
)

// Large recursive type graph (reproduces G3dModelJson scenario)
case class LargeRecKeyframeV1(
    keytime: Float = 0f,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None
)
case class LargeRecKeyframeV2(keytime: Float = 0f, value: List[Float])
case class LargeRecAnimBone(
    boneId: String,
    keyframes: Option[List[LargeRecKeyframeV1]] = None,
    translation: Option[List[LargeRecKeyframeV2]] = None,
    rotation: Option[List[LargeRecKeyframeV2]] = None,
    scaling: Option[List[LargeRecKeyframeV2]] = None
)
case class LargeRecAnimation(id: String = "", bones: List[LargeRecAnimBone] = Nil)
case class LargeRecMeshPart(id: String, tpe: String, indices: List[Short])
case class LargeRecMesh(id: String = "", attributes: List[String], vertices: List[Float], parts: List[LargeRecMeshPart])
case class LargeRecTexture(
    id: String,
    filename: String,
    uvTranslation: Option[List[Float]] = None,
    uvScaling: Option[List[Float]] = None,
    tpe: String
)
case class LargeRecMaterial(
    id: String,
    diffuse: Option[List[Float]] = None,
    ambient: Option[List[Float]] = None,
    emissive: Option[List[Float]] = None,
    specular: Option[List[Float]] = None,
    reflection: Option[List[Float]] = None,
    shininess: Float = 0f,
    opacity: Float = 1f,
    textures: List[LargeRecTexture] = Nil
)
case class LargeRecBone(
    node: String,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None
)
case class LargeRecNodePart(meshpartid: String, materialid: String, bones: List[LargeRecBone] = Nil)
case class LargeRecNode(
    id: String,
    translation: Option[List[Float]] = None,
    rotation: Option[List[Float]] = None,
    scale: Option[List[Float]] = None,
    mesh: Option[String] = None,
    parts: List[LargeRecNodePart] = Nil,
    children: List[LargeRecNode] = Nil
)
case class LargeRecModel(
    version: List[Short],
    id: String = "",
    meshes: List[LargeRecMesh] = Nil,
    materials: List[LargeRecMaterial] = Nil,
    nodes: List[LargeRecNode] = Nil,
    animations: List[LargeRecAnimation] = Nil
)

// Absent-field decoding test types (see UBJsonAbsentFieldsSpec)
case class UbAbsentRecord(
    name: String,
    opt: Option[String],
    n: Int = 7,
    xs: List[Int] = Nil,
    ys: Vector[Int],
    m: Map[String, Int]
)
case class UbAbsentOptionWithNoneDefault(next: Option[String] = None)
sealed trait UbAbsentAdt
case class UbAbsentLeaf(name: String, opt: Option[Int], n: Int = 3, xs: List[Int]) extends UbAbsentAdt

/** Configs read from mutable fields so that the macro cannot evaluate them at compile time. */
object UbAbsentRuntimeConfigs {
  @volatile var default: UBJsonConfig = UBJsonConfig()
  @volatile var requireDefault: UBJsonConfig = UBJsonConfig().withRequireDefaultFields
}

// jsoniter-scala parity test types (see UBJsonParitySpec)
case class UbZeroFields()
case class UbAllTransient(@transientField x: Int = 1)
case class UbEmptyCapable(a: Array[Int], s: String, m: Map[String, Int], l: List[Int])
case class UbArrayDefault(a: Array[Int] = Array(1, 2), n: Int = 0)

/** Configs read from mutable fields, so that the macro cannot evaluate them at compile time. */
object UbParityRuntimeConfigs {
  @volatile var default: UBJsonConfig = UBJsonConfig()
  @volatile var strict: UBJsonConfig = UBJsonConfig().withSkipUnexpectedFields(false)
  @volatile var setLimit2: UBJsonConfig = UBJsonConfig().withSetMaxInsertNumber(2)
  @volatile var transientEmpty: UBJsonConfig = UBJsonConfig().withTransientEmpty
  @volatile var transientDefault: UBJsonConfig = UBJsonConfig().withTransientDefault
}

// jsoniter-derivation parity (see UBJsonJsoniterParitySpec)
sealed trait UbMixed
object UbMixed {
  case object Origin extends UbMixed
  final case class Point(x: Int, y: Int) extends UbMixed
  @hearth.kindlings.ubjsonderivation.annotations.named("renamed-leaf")
  final case class Renamed(v: String) extends UbMixed
}
case class UbNamedField(@hearth.kindlings.ubjsonderivation.annotations.named("b") a: Int)
object UbSize extends Enumeration {
  val Small: Value = Value
  val Big: Value = Value("BIG")
}
case class UbTuples(t: (Int, String), l: List[(Int, Int)], size: UbSize.Value)
case class UbBigInt(b: BigInt)
case class UbDuplicatedKeys(a: Int, @fieldName("a") b: Int)

object UbJsoniterParityRuntimeConfigs {
  @volatile var default: UBJsonConfig = UBJsonConfig()
  @volatile var discriminator: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
  @volatile var circeLike: UBJsonConfig = UBJsonConfig().withCirceLikeObjectEncoding
  @volatile var fullNames: UBJsonConfig =
    UBJsonConfig().withDiscriminator("type").withAdtLeafClassNameMapper((s: String) => s)
  @volatile var contradictory: UBJsonConfig = UBJsonConfig().withRequireDefaultFields.withTransientDefault
  @volatile var bigIntDigits3: UBJsonConfig = UBJsonConfig().withBigIntDigitsLimit(3)
}

object UbDiscriminatorRuntimeConfigs {
  @volatile var anywhere: UBJsonConfig = UBJsonConfig().withDiscriminator("type").withRequireDiscriminatorFirst(false)
  @volatile var anywhereStrict: UBJsonConfig =
    UBJsonConfig().withDiscriminator("type").withRequireDiscriminatorFirst(false).withSkipUnexpectedFields(false)
}

// --- Ported from jsoniter-derivation: plain classes, multiple parameter lists, scala.transient, Enumeration ids,
// nested options, nested sealed hierarchies ---

class UbPlainClass(val id: Int, var name: String) {
  override def hashCode(): Int = id * 31 + java.util.Objects.hashCode(name)
  override def equals(obj: Any): Boolean = obj match {
    case c: UbPlainClass => id == c.id && java.util.Objects.equals(name, c.name)
    case _               => false
  }
  override def toString: String = s"UbPlainClass($id,$name)"
}
class UbPlainWithDefaults(val id: Int, val tags: List[String] = List("a"), val note: Option[String] = None) {
  override def hashCode(): Int = java.util.Objects.hash(Int.box(id), tags, note)
  override def equals(obj: Any): Boolean = obj match {
    case c: UbPlainWithDefaults => id == c.id && tags == c.tags && note == c.note
    case _                      => false
  }
  override def toString: String = s"UbPlainWithDefaults($id,$tags,$note)"
}
case class UbHasPlain(inner: UbPlainClass, list: List[UbPlainClass])
class UbNoAccessor(val a: Int, b: Int) { override def toString: String = s"UbNoAccessor($a,$b)" }

sealed trait UbPlainAdt
class UbPlainLeaf() extends UbPlainAdt {
  override def hashCode(): Int = 1
  override def equals(obj: Any): Boolean = obj.isInstanceOf[UbPlainLeaf]
  override def toString: String = "UbPlainLeaf()"
}
class UbPlainLeafWithFields(val x: Int, var y: String) extends UbPlainAdt {
  override def hashCode(): Int = x
  override def equals(obj: Any): Boolean = obj match {
    case c: UbPlainLeafWithFields => x == c.x && y == c.y
    case _                        => false
  }
  override def toString: String = s"UbPlainLeafWithFields($x,$y)"
}
object UbPlainObject extends UbPlainAdt

// a non-abstract sealed base is one of its own leaves (as in jsoniter-scala)
sealed class UbSealedBase
case class UbSealedB(n: Int) extends UbSealedBase
case class UbSealedC(s: String) extends UbSealedBase

case class UbMultiList(i: Int)(val l: Long)(var s: String) {
  override def toString: String = s"UbMultiList($i)($l)($s)"
}
case class UbMultiListDependent(i: Int = 1)(val l: Long = i - 1L)(var s: String = s"$i-$l") {
  override def toString: String = s"UbMultiListDependent($i)($l)($s)"
}
case class UbGenericMultiList[A](a: A)(@transientField val n: Int = 2)(val as: List[A] = List.fill(n)(a))(
    val m: Int = as.size * n
) {
  override def toString: String = s"UbGenericMultiList($a)($n)($as)($m)"
}
sealed trait UbMultiListAdt
case class UbMultiData1(i: Int)(val pos: Int) extends UbMultiListAdt
case class UbMultiData2(i: Int)(val pos: Int) extends UbMultiListAdt

case class UbScalaTransient(required: String, @transient transient: String = "default")

object UbLocation extends Enumeration {
  type UbLocation = Value
  val IP, GPS: UbLocation = Value
  val Named: UbLocation = Value("named-value")
}
case class UbEnums(lt: UbLocation.UbLocation, l: List[UbLocation.UbLocation])

case class UbNestedOptions(f1: Option[String], f2: Option[Option[String]] = None)
case class UbNestedOptions3(y: Option[String], x: Option[Option[Option[String]]] = None)

sealed trait UbVehicle
sealed trait UbMotorVehicle extends UbVehicle
case class UbTruck(payload: Int) extends UbMotorVehicle
case class UbMotorcycle(cc: Int) extends UbMotorVehicle
case object UbScooter extends UbMotorVehicle
case class UbBicycle(gears: Int) extends UbVehicle

case class UbNotNullable(s: String, n: Int)

object UbPortRuntimeConfigs {
  @volatile var default: UBJsonConfig = UBJsonConfig()
  @volatile var discriminator: UBJsonConfig = UBJsonConfig().withDiscriminator("type")
  @volatile var discriminatorAnywhere: UBJsonConfig =
    UBJsonConfig().withDiscriminator("type").withRequireDiscriminatorFirst(false)
  @volatile var circeLike: UBJsonConfig = UBJsonConfig().withCirceLikeObjectEncoding
  @volatile var scalaTransient: UBJsonConfig = UBJsonConfig().withScalaTransientSupport
  @volatile var enumIds: UBJsonConfig = UBJsonConfig().withUseScalaEnumValueId
  @volatile var skipNestedOptions: UBJsonConfig = UBJsonConfig().withSkipNestedOptionValues.withTransientNone
  @volatile var discriminatorTransientNone: UBJsonConfig = UBJsonConfig().withDiscriminator("type").withTransientNone
  @volatile var transientNull: UBJsonConfig = UBJsonConfig().withTransientNull
  @volatile var transientDefault: UBJsonConfig = UBJsonConfig().withTransientDefault
}

case class UbScalaTransientNoDefault(a: Int, @transient b: Int)

case class UbFieldDefaults(l: List[Int] = List(0), o: Option[Int] = Some(1), s: String = "d", n: Int = 2)
