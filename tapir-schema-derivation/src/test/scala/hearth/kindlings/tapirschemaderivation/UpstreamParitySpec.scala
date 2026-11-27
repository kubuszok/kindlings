package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import sttp.tapir.{Schema, SchemaType, Validator}
import sttp.tapir.Schema.SName
import sttp.tapir.Schema.annotations.*

// Behaviour matching tapir's own derivation (checked against tapir itself by upstream-oracle-tests'
// TapirParityOracleSpec); cases from tapir's SchemaGenericAutoTest / SchemaMacroTest(Data) at v1.13.33.

@customise(_.format("c1"))
@customise(s => s.format(s.format.getOrElse("") + "-c2"))
final case class UpMultiCustomised(
    @customise(_.format("f1"))
    @customise(s => s.format(s.format.getOrElse("") + "-f2"))
    field: Int
)

@description("a pet")
sealed abstract class UpPet {
  @description("the name")
  def name: String
}
final case class UpDog(name: String, @validate(Validator.min(0)) age: Int) extends UpPet
final case class UpCat(@description("cat name") name: String) extends UpPet

@encodedName("CustomNode")
sealed trait UpNode
object UpNode {
  @encodedName("CustomLeaf")
  final case class Leaf(@validate(Validator.min(1)) value: Int) extends UpNode
  final case class Branch(children: List[UpNode]) extends UpNode
}

@description("sizes")
object UpSize extends Enumeration {
  val Small, Large = Value
}
final case class UpUsesSize(size: UpSize.Value)

final class UpstreamParitySpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default.withDiscriminator("type")
  implicit val preferCirce: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  private def fields(s: Schema[?]): List[SchemaType.SProductField[?]] = s.schemaType match {
    case p: SchemaType.SProduct[?] => p.fields
    case _                         => Nil
  }

  private def subtypes(s: Schema[?]): List[Schema[?]] = s.schemaType match {
    case c: SchemaType.SCoproduct[?] => c.subtypes
    case _                           => Nil
  }

  test("several @customise annotations are applied in declaration order") {
    val schema = KindlingsSchema.derived[UpMultiCustomised].schema
    assertEquals(schema.format, Some("c1-c2"))
    assertEquals(fields(schema).head.schema.format, Some("f1-f2"))
  }

  test("subtypes inherit the annotations of the parent type and of its members, unless they have their own") {
    val pet = KindlingsSchema.derived[UpPet].schema
    val dog = subtypes(pet).find(_.name.exists(_.fullName.endsWith("UpDog"))).get
    val cat = subtypes(pet).find(_.name.exists(_.fullName.endsWith("UpCat"))).get
    assertEquals(dog.description, Some("a pet"))
    assertEquals(fields(dog).find(_.name.name == "name").flatMap(_.schema.description), Some("the name"))
    assertEquals(fields(cat).find(_.name.name == "name").flatMap(_.schema.description), Some("cat name"))
  }

  test("@encodedName on a type names its schema (also in references to it)") {
    val node = KindlingsSchema.derived[UpNode].schema
    assertEquals(node.name, Some(SName("CustomNode")))
    assertEquals(
      subtypes(node).flatMap(_.name.map(_.fullName)).toSet,
      Set("CustomLeaf", classOf[UpNode.Branch].getName.replace('$', '.'))
    )
  }

  test("subtype schemas are chosen for values of nested and renamed subtypes (applyValidation reaches their fields)") {
    val pet = KindlingsSchema.derived[UpPet].schema
    assertEquals(pet.applyValidation(UpDog("rex", -1)).map(_.invalidValue), List(-1))
    val node = KindlingsSchema.derived[UpNode].schema
    assertEquals(node.applyValidation(UpNode.Leaf(0)).map(_.invalidValue), List(0))
    assertEquals(node.applyValidation(UpNode.Branch(List(UpNode.Leaf(0)))).map(_.invalidValue), List(0))
  }

  test("maps are not optional on their own (only as fields, by the JSON config)") {
    assertEquals(KindlingsSchema.derived[Map[String, Int]].schema.isOptional, false)
  }

  test("scala.Enumeration values are named after the enumeration object, with its annotations") {
    val size: Schema[?] = fields(KindlingsSchema.derived[UpUsesSize].schema).head.schema
    assertEquals(size.name, Some(SName(classOf[UpSize.type].getName.stripSuffix("$"))))
    assertEquals(size.description, Some("sizes"))
  }
}
