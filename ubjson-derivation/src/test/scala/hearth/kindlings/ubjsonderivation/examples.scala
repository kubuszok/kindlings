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

// Indirect recursion test types
case class RecursiveNode(id: String, children: List[RecursiveNode])
case class RecursiveParent(name: String, nodes: List[RecursiveNode])

// Recursive types: isolating Option vs plain field
case class Recursive3Plain(id: String, extra: String, children: List[Recursive3Plain] = Nil)
case class Recursive3Option(id: String, extra: Option[String] = None, children: List[Recursive3Option] = Nil)

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
