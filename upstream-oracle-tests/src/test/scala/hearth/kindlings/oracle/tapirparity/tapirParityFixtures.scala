package hearth.kindlings.oracle.tapirparity

import sttp.tapir.{AttributeKey, Validator}
import sttp.tapir.Schema.annotations.*

// Fixtures of the tapir parity oracle (TapirParityOracleSpec), mirroring the fixtures of tapir's own tests at v1.13.33
// (core/src/test/scala/sttp/tapir/generic/SchemaGenericAutoTest.scala, SchemaMacroTestData.scala,
// SchemaApplyValidationTestData.scala, scalajvm/.../SchemaSerialisationTest.scala). Each is derived both by Kindlings and
// by tapir.

// SchemaGenericAutoTest: A, B, C, D, F, G, H
final case class PtA(f1: String, f2: Int, f3: Option[String])
final case class PtB(g1: String, g2: PtA)
final case class PtC(h1: List[String], h2: Option[Int])
final case class PtD(someFieldName: String)
final case class PtF(f1: List[PtF], f2: Int)
final class PtCustom(val c: String)
final case class PtG(f1: Int, f2: PtCustom)
final case class PtH[T](data: T)

// SchemaGenericAutoTest: "add meta-data to schema from annotations"
@description("class I")
final case class PtI(
    @description("some int field")
    @default(1234)
    @encodedExample(1234)
    @format("int32")
    @validate[Int](Validator.max(100))
    int: Int,
    noDesc: Long,
    @description("another optional boolean flag")
    @encodedName("alternativeBooleanName")
    bool: Option[Boolean],
    @deprecated
    @description("child-k-desc")
    @encodedName("child-k-name")
    child: PtK
)

final case class PtK(
    @format("double64")
    double: Double,
    @format("special-string")
    str: String
)

// SchemaGenericAutoTest: "not transform names which are annotated with a custom name"
final case class PtL(
    @encodedName("specialName")
    firstField: Int,
    secondField: Int
)

// SchemaGenericAutoTest: "customise the schema using the given function"
@customise(s => s.attribute(PtM.testAttributeKey, "test"))
final case class PtM(field: Int)
object PtM {
  val testAttributeKey: AttributeKey[String] = new AttributeKey[String]("hearth.kindlings.oracle.tapirparity.PtM.key")
}

// SchemaGenericAutoTest: "apply multiple customise annotations in declaration order"
@customise(_.format("c1"))
@customise(s => s.format(s.format.getOrElse("") + "-c2"))
final case class PtMultiCustomised(
    @customise(_.format("f1"))
    @customise(s => s.format(s.format.getOrElse("") + "-f2"))
    field: Int
)

// Annotations upstream tests do not exercise on derived schemas: @title, @hidden, @deprecated on a type,
// @encodedExample/@default/@validate on a type.
@title("The J")
@deprecated
@encodedExample("""{"a":1}""")
@validate[PtJ](Validator.custom(_ => sttp.tapir.ValidationResult.Valid, Some("always valid")))
final case class PtJ(
    @title("field a") a: Int,
    @hidden b: Int,
    @default(Some(3), encoded = Some("3")) c: Option[Int]
)

// SchemaGenericAutoTest: recursive coproduct and products wrapped in Option / List
sealed trait PtNode
final case class PtEdge(id: Long, source: PtNode) extends PtNode
final case class PtSimpleNode(id: Long) extends PtNode

final case class PtIOpt(i1: Option[PtIOpt], i2: Int)
final case class PtJOpt(data: Option[PtIOpt])

final case class PtIList(i1: List[PtIList], i2: Int)
final case class PtJList(data: List[PtIList])

// SchemaGenericAutoTest: discriminators (case object child included)
sealed trait PtEntity
final case class PtPerson(first: String, age: Int) extends PtEntity
final case class PtOrganization(name: String) extends PtEntity
case object PtUnknownEntity extends PtEntity

// SchemaMacroTestData: subtypes inheriting annotations of the parent's members
sealed abstract class PtPet {
  @description("name")
  def name: String
}
final case class PtCat(@description("cat name") name: String, @description("cat food") catFood: String) extends PtPet
final case class PtDog(name: String, @description("dog food") dogFood: String) extends PtPet
sealed trait PtRodent extends PtPet {
  @description("likes nuts?")
  def likesNuts: Boolean
}
final case class PtHamster(name: String, likesNuts: Boolean) extends PtRodent

// Annotations on the parent type (magnolia's `inheritedAnnotations` of the subtypes)
@description("a shape")
@customise(_.format("shape"))
sealed trait PtAnnotatedShape
final case class PtSquare(side: Double) extends PtAnnotatedShape
@description("a round shape")
final case class PtRound(radius: Double) extends PtAnnotatedShape

// SchemaMacroTestData: @encodedName on types, and on a member of the parent
@encodedName("CustomHericium")
sealed trait PtHericium {
  @encodedName("customCommonField")
  @description("A common field")
  val commonField: Int
}
object PtHericium {
  @encodedName("CustomErinaceus")
  final case class Erinaceus(commonField: Int, e: String) extends PtHericium
  final case class Abietis(commonField: Int, a: Int) extends PtHericium
  final case class Botryoides(commonField: Int, b: Boolean) extends PtHericium
}

// SchemaMacroTestData: annotated enumerations
@description("it's a small alphabet")
@customise(_.format("letters"))
sealed trait PtLetters
object PtLetters {
  case object A extends PtLetters
  case object B extends PtLetters
  case object C extends PtLetters
}

@description("country")
@encodedName("country-encoded-name")
@customise(_.format("country"))
object PtCountries extends Enumeration {
  type Country = Value
  val PL, NL = Value
}
final case class PtUsesCountry(country: PtCountries.Country)

// Plain (not annotated) enumerations
sealed trait PtColor
object PtColor {
  case object Red extends PtColor
  case object Green extends PtColor
}
object PtSize extends Enumeration {
  val Small, Large = Value
}
final case class PtUsesEnums(color: PtColor, size: PtSize.Value, entity: PtEntity)

// SchemaGenericAutoTest: "add validators for collection and option elements"
final case class PtValidateEach(
    @validateEach(Validator.min(5))
    ints: List[Int],
    @validateEach[String](Validator.minLength(3))
    maybeString: Option[String]
)

// SchemaApplyValidationTestData
final case class PtRecursiveName(name: String, subNames: Option[Vector[PtRecursiveName]])
final case class PtSimpleDog(name: String)

// scalajvm/.../SchemaSerialisationTest
final case class PtSerPerson(
    @validate(Validator.minLength(3): Validator[String]) name: String,
    @validateEach(Validator.min(18)) age: Option[Int]
)
final case class PtSerFamily(person1: PtSerPerson, person2: PtSerPerson, others: List[PtSerPerson])

// Value classes (Scala 2 only upstream: SchemaMacroTest2, LegacySchemaGenericAutoTest)
final case class PtStringValueClass(value: String) extends AnyVal
final case class PtIntValueClass(value: Int) extends AnyVal
final case class PtDoubleValueClass(value: Double) extends AnyVal
final case class PtUsesValueClasses(
    s: PtStringValueClass,
    i: PtIntValueClass,
    d: PtDoubleValueClass,
    list: List[PtIntValueClass],
    map: Map[String, PtIntValueClass]
)

// Private constructor fields (cross-module finding: Hearth's caseFieldValuesAt omits them)
final case class PtPrivate(a: Int, private val b: String, c: Boolean) {
  def bValue: String = b
}

// Maps of products / generic products (SchemaGenericAutoTest "find schema for map of (generic) products")
final case class PtUsesMaps(products: Map[String, PtD], generic: Map[String, PtH[PtD]], ints: Map[String, Int])

// Schemas provided by the user with tapir's builders for fields (Schema.oneOfWrapped, Schema.oneOfUsingField,
// Schema.derivedEnumeration): picked up by Kindlings like by tapir.
sealed trait PtKinded { def kind: String }
final case class PtUser(firstName: String) extends PtKinded { def kind: String = "user" }
final case class PtOrg(name: String) extends PtKinded { def kind: String = "org" }
final case class PtUsesProvided(wrapped: PtEntity, byField: PtKinded, letters: PtLetters, custom: PtCustom)

// SchemaAnnotationsTest (SchemaAnnotationsTestData): every metadata annotation on a type, and @customise after them
@description("my-string")
@encodedExample("encoded-example")
@default(PtMyString("default"), encoded = Some("encoded-default"))
@format("utf8")
@deprecated
@encodedName("encoded-name")
@validate(Validator.pass[PtMyString])
final case class PtMyString(value: String)

@format("utf8")
@customise(_.format("c1"))
@customise(s => s.format(s.format.getOrElse("") + "-c2"))
final case class PtMyCustomisedString(value: String)

final case class PtUsesAnnotatedTypes(my: PtMyString, customised: PtMyCustomisedString)

// SchemaGenericAutoTest "find schema for simple types / optional types / collections", as fields
final case class PtBuiltIns(
    short: Short,
    long: Long,
    float: Float,
    double: Double,
    boolean: Boolean,
    bigDecimal: BigDecimal,
    javaBigDecimal: java.math.BigDecimal,
    optionalString: Option[String],
    array: Array[String],
    vector: Vector[String],
    set: Set[String],
    listOfA: List[PtA]
)
