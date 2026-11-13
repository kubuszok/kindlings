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
case class WithSetField(items: Set[Int])

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

// Numeric boundary test types
case class ByteBoundaries(min: Byte, max: Byte)
case class ShortBoundaries(min: Short, max: Short)

// Unicode test types
case class UnicodeContent(value: String)

// Nested structure test types
case class NestedLists(matrix: List[List[Int]])
case class OptionalList(items: Option[List[Int]])

// Indirect recursion test types
case class RecursiveNode(id: String, children: List[RecursiveNode])
case class RecursiveParent(name: String, nodes: List[RecursiveNode])

// Value class edge cases
case class WrappedString(value: String) extends AnyVal
case class WithOptionalWrapped(item: Option[WrappedInt])
case class WithWrappedList(items: List[WrappedInt])

// Array[T] test types
case class WithIntArray(values: Array[Int]) {
  override def equals(o: Any): Boolean = o match {
    case that: WithIntArray => java.util.Arrays.equals(values, that.values)
    case _                  => false
  }
}
case class WithStringArray(values: Array[String]) {
  override def equals(o: Any): Boolean = o match {
    case that: WithStringArray =>
      java.util.Arrays.equals(values.asInstanceOf[Array[AnyRef]], that.values.asInstanceOf[Array[AnyRef]])
    case _ => false
  }
}

// UUID test type (now supported as built-in)
case class WithUUID(id: java.util.UUID, name: String)

// Unit test type
case class WithUnit(value: Unit, name: String)

// Nested sealed trait hierarchy
sealed trait Vehicle
sealed trait MotorVehicle extends Vehicle
case class Truck(payload: Int) extends MotorVehicle
case class Motorcycle(cc: Int) extends MotorVehicle
case class Bicycle(gears: Int) extends Vehicle

// Direct recursive sealed trait (bare field, not wrapped in List/Option)
sealed trait TreeNode
case class Branch(value: Int, left: TreeNode, right: TreeNode) extends TreeNode
case class Leaf(value: Int) extends TreeNode

// Mutual recursion (bare Option fields)
case class MutRecA(value: Int, b: Option[MutRecB])
case class MutRecB(value: String, a: Option[MutRecA])

// Combinatorial wrapper x inner type (no pre-existing codecs for SimplePerson/Shape — must derive recursively, bug #120)
case class CombOuter(
    optPrimitive: Option[Int],
    optCaseClass: Option[SimplePerson],
    optSealedTrait: Option[Shape],
    optValueClass: Option[WrappedInt],
    listCaseClass: List[SimplePerson],
    listSealedTrait: List[Shape],
    mapCaseClass: Map[String, SimplePerson],
    mapSealedTrait: Map[String, Shape]
)

// Annotation x type shape test types
case class AnnotatedCaseClass(
    @fieldName("full_name") fullName: String,
    @stringified score: Int,
    @fieldName("is_active") @stringified isActive: Int
)

sealed trait AnnotatedSealedTrait
case class AnnotatedSubA(@fieldName("sub_name") subName: String, @stringified value: Int) extends AnnotatedSealedTrait
case class AnnotatedSubB(@fieldName("sub_label") subLabel: String) extends AnnotatedSealedTrait

// @transientField on sealed trait subtypes
sealed trait TransientSealedTrait
case class TransientSubA(name: String, @transientField cache: String = "default-cache") extends TransientSealedTrait
case class TransientSubB(value: Int, @transientField temp: Int = -1) extends TransientSealedTrait

// HashMap / TreeMap test types
case class WithHashMap(data: scala.collection.immutable.HashMap[String, Int])
case class WithTreeMap(data: scala.collection.immutable.TreeMap[String, Int])
case class WithArrayBuffer(items: scala.collection.mutable.ArrayBuffer[Int])

// IntMap / LongMap test types
case class WithIntMap(data: scala.collection.immutable.IntMap[String])
case class WithLongMap(data: scala.collection.immutable.LongMap[String])

// Gap 5.2: additional collection types
case class WithLazyList(items: LazyList[Int])
case class WithQueue(items: scala.collection.immutable.Queue[Int])
case class WithLinkedHashMap(data: scala.collection.mutable.LinkedHashMap[String, Int])

// inlineOneValueClasses test types
case class Wrapper(inner: Int)
case class StringWrapper(value: String)

// Name collision test types (Scala keywords as field names)
case class WithKeywordFields(`type`: String, `class`: Int, `val`: Boolean)
sealed trait KeywordEnum
case class `match`(pattern: String) extends KeywordEnum
case object `null` extends KeywordEnum

// Either test types
case class WithEitherField(result: Either[String, Int])
case class WithNestedEither(value: Either[Either[Int, String], Boolean])

// BitSet test types
case class WithImmutableBitSet(bits: scala.collection.immutable.BitSet)
case class WithMutableBitSet(bits: scala.collection.mutable.BitSet)

// Absent-field decoding test types (issue #264, see AbsentFieldsSpec)
case class AbsentOptionOnly(next: Option[String])
case class AbsentOptionWithNoneDefault(next: Option[String] = None)
case class AbsentOptionWithSomeDefault(opt: Option[String] = Some("d"))
case class AbsentFieldsRecord(
    name: String,
    opt: Option[String],
    n: Int = 7,
    xs: List[Int] = Nil,
    ys: Vector[Int],
    m: Map[String, Int]
)
case class AbsentPrimitive(label: String, count: Int)
case class AbsentCamelCase(firstName: String)
case class AbsentRenamed(@fieldName("the_name") name: String)
case class AbsentGeneric[A](value: A, opt: Option[A], xs: List[A], n: Int = 5)
sealed trait AbsentAdt
case class AbsentLeaf(name: String, opt: Option[Int], n: Int = 3, xs: List[Int]) extends AbsentAdt

/** Configs read from mutable fields so that the macro cannot evaluate them at compile time (forces generic codegen). */
object AbsentFieldsRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var allTransient: JsoniterConfig =
    JsoniterConfig.default.withTransientNone.withTransientDefault.withTransientEmpty
  @volatile var requireDefault: JsoniterConfig = JsoniterConfig.default.withRequireDefaultFields
  @volatile var requireCollection: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
  @volatile var strict: JsoniterConfig = JsoniterConfig.default.withSkipUnexpectedFields(false)
  @volatile var checkDuplication: JsoniterConfig = JsoniterConfig.default.withCheckFieldDuplication
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
}

/** Conflicting flag combinations; a compile-time-known config with them emits a derivation warning. */
object ConflictingRuntimeConfigs {
  @volatile var transientDefaultRequireDefault: JsoniterConfig =
    JsoniterConfig.default.withTransientDefault.withRequireDefaultFields
  @volatile var requireCollectionTransientEmpty: JsoniterConfig =
    JsoniterConfig.default.withRequireCollectionFields.withTransientEmpty
}

// jsoniter-scala parity test types (see JsoniterScalaParitySpec)
case class ParityZeroFields()
case class ParityAllTransient(@transientField x: Int = 1)
sealed trait ParityZeroAdt
case class ParityZeroLeaf() extends ParityZeroAdt
case class ParityZeroLeafWithField(a: Int) extends ParityZeroAdt
case object ParityZeroObj extends ParityZeroAdt
case class ParityEmptyCapable(a: Array[Int], s: String, m: Map[String, Int], l: List[Int])
case class ParityArrayDefault(a: Array[Int] = Array(1, 2), n: Int = 0)
case class ParityNullCollections(l: List[Int], m: Map[String, Int], v: Vector[String])
case class ParityInner(x: Int)
case class ParityNullDefaults(
    s: String = "VVV",
    bi: BigInt = BigInt(-1),
    l: List[Int] = List(0),
    m: Map[String, Int] = Map("a" -> 1),
    inner: ParityInner = ParityInner(1)
)
case class ParityPrimitiveDefault(n: Int = 5)
case class ParityBigDecimal(value: BigDecimal)
case class ParityBigInt(value: BigInt)
case class ParityUserId(id: String) extends AnyVal
case class ParityNoKeyCodec(a: Int, b: Int)
sealed trait ParityAdt
case class ParityAAA(a: Int) extends ParityAdt
case object ParityDDD extends ParityAdt
case class ParityNestedOptions(f1: Option[String], f2: Option[Option[String]] = None)
case class ParityWithUnit(u: Unit, i: Int)
case class ParityHasWrapper(w: Wrapper, s: StringWrapper)

/** Configs read from mutable fields, so that the macro cannot evaluate them at compile time (forces the generic,
  * runtime-config codegen). Their compile-time-known counterparts live in `OptimizedCodecs` (scala-2 / scala-3).
  */
object ParityRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var strict: JsoniterConfig = JsoniterConfig.default.withSkipUnexpectedFields(false)
  @volatile var setLimit10: JsoniterConfig = JsoniterConfig.default.withSetMaxInsertNumber(10)
  @volatile var jsoniterScalaDefaults: JsoniterConfig = JsoniterConfig.jsoniterScalaDefaults
  @volatile var transientEmpty: JsoniterConfig = JsoniterConfig.default.withTransientEmpty
  @volatile var transientDefault: JsoniterConfig = JsoniterConfig.default.withTransientDefault
  @volatile var requireCollection: JsoniterConfig = JsoniterConfig.default.withRequireCollectionFields
  @volatile var precision5: JsoniterConfig = JsoniterConfig.default.withBigDecimalPrecision(5)
  @volatile var scaleLimit2: JsoniterConfig = JsoniterConfig.default.withBigDecimalScaleLimit(2)
  @volatile var digitsLimit3: JsoniterConfig = JsoniterConfig.default.withBigDecimalDigitsLimit(3)
  @volatile var bigIntDigitsLimit3: JsoniterConfig = JsoniterConfig.default.withBigIntDigitsLimit(3)
  @volatile var transientNull: JsoniterConfig = JsoniterConfig.default.withTransientNull
  @volatile var scalaTransient: JsoniterConfig = JsoniterConfig.default.withScalaTransientSupport
  @volatile var alwaysEmit: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAlwaysEmitDiscriminator
  @volatile var alwaysEmitStrict: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAlwaysEmitDiscriminator.withSkipUnexpectedFields(false)
  @volatile var alwaysEmitCheckDuplication: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withAlwaysEmitDiscriminator.withCheckFieldDuplication
  @volatile var skipNestedOptions: JsoniterConfig =
    JsoniterConfig.default.withTransientNone.withSkipNestedOptionValues
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  @volatile var discriminatorStrict: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withSkipUnexpectedFields(false)
  @volatile var discriminatorCheckDuplication: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withCheckFieldDuplication
  @volatile var inlineOneValueClasses: JsoniterConfig = JsoniterConfig.default.withInlineOneValueClasses
  @volatile var mapAsArray: JsoniterConfig = JsoniterConfig.default.withMapAsArray
}
case class ParityPrivateParam(private val token: String)

// requireDiscriminatorFirst (Spec ~2681, ~2829)
sealed trait DiscAdt
case class DiscA(a: Int) extends DiscAdt
case class DiscB(a: BigInt) extends DiscAdt
case class DiscC(a: Int, b: String) extends DiscAdt
case class DiscE() extends DiscAdt
case object DiscD extends DiscAdt

object DiscRuntimeConfigs {
  @volatile var anywhere: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withRequireDiscriminatorFirst(false)
  @volatile var anywhereT: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("t").withRequireDiscriminatorFirst(false)
  @volatile var anywhereStrict: JsoniterConfig =
    JsoniterConfig.default
      .withDiscriminator("type")
      .withRequireDiscriminatorFirst(false)
      .withSkipUnexpectedFields(false)
  @volatile var anywhereCheckDuplication: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withRequireDiscriminatorFirst(false).withCheckFieldDuplication
  @volatile var first: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
}

// tuples (Spec ~1720)
case class TupleFields(t1: (Int, Double, List[Char]), t2: (String, BigInt, Option[CardinalDirection]))

// case objects / enums as strings (Spec ~640, ~784, ~857, ~2687, ~2704)
object ParityLocation extends Enumeration {
  type ParityLocation = Value
  val IP, GPS: ParityLocation = Value
  val Named: ParityLocation = Value("named-value")
}
case class ParityEnums(lt: ParityLocation.ParityLocation, m: Map[ParityLocation.ParityLocation, Int])

object EnumRuntimeConfigs {
  @volatile var default: JsoniterConfig = JsoniterConfig.default
  @volatile var circeLike: JsoniterConfig = JsoniterConfig.default.withCirceLikeObjectEncoding
  @volatile var discriminator: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type")
  @volatile var discriminatorEnumAsStrings: JsoniterConfig =
    JsoniterConfig.default.withDiscriminator("type").withEnumAsStrings
  @volatile var enumIds: JsoniterConfig = JsoniterConfig.default.withDiscriminator("type").withUseScalaEnumValueId
}

// stringified (Spec ~224, ~454, ~1712, ~1732, ~1942, ~2383)
case class ParityStringified(
    @stringified i: Int,
    @stringified bi: BigInt,
    @stringified o: Option[Int],
    @stringified l: List[Int]
)
case class ParityStringifiedMixed(@stringified b: Boolean, n: Int, inner: ParityInner)
case class ParityAllNumbers(
    b: Byte,
    s: Short,
    i: Int,
    l: Long,
    f: Float,
    d: Double,
    bool: Boolean,
    jl: java.lang.Long,
    bd: BigDecimal,
    os: Option[Short],
    m: Map[String, Long],
    t: (Int, Double),
    c: Char,
    str: String
)

object StringifiedRuntimeConfigs {
  @volatile var stringified: JsoniterConfig = JsoniterConfig.default.withStringified
  @volatile var stringifiedDigits3: JsoniterConfig = JsoniterConfig.default.withStringified.withBigDecimalDigitsLimit(3)
  @volatile var stringifiedIds: JsoniterConfig = JsoniterConfig.default.withStringified.withUseScalaEnumValueId
}

case class StringifiedString(name: String, @stringified label: String)

// @named on ADT leaves (Spec ~2900-2910)
sealed abstract class NamedBase extends Product with Serializable
@hearth.kindlings.jsoniterderivation.annotations.named("X")
final case class NamedA(b: NamedB) extends NamedBase
@hearth.kindlings.jsoniterderivation.annotations.named("Y")
final case class NamedB(c: String) extends NamedBase
sealed abstract class NamedVersion(val value: String)
@hearth.kindlings.jsoniterderivation.annotations.named("8.10")
case object Version810 extends NamedVersion("8.10")
@hearth.kindlings.jsoniterderivation.annotations.named("8.09")
case object Version809 extends NamedVersion("8.9")

case class MapperFields(HTTPServer: Int, field1: Int)

// scalaTransientSupport / transientNull (Spec ~2590)
case class ParityScalaTransient(required: String, @transient transient: String = "default")
case class ParityNullable(s: String, n: Int)

// compile-time validations (Spec ~2365, ~3195, ~3223)
case class CollidingFields(@hearth.kindlings.jsoniterderivation.annotations.fieldName("x") a: Int, x: Int)
case class CamelFields(fooBar: Int, foo_bar: Int)
sealed trait CollidingAdt
case class CollidingLeaf(`type`: String) extends CollidingAdt
sealed trait CollidingNames
case class CollidingA() extends CollidingNames
case class CollidingB() extends CollidingNames

// literal types (LiteralTypesSpec)
case class ParityLiteralTypes(s: "VVV", b: true, c: '1', i: 2, l: 3L, f: 4.0f, d: 5.0)
