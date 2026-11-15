package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import io.circe.KeyEncoder
import sttp.tapir.{Schema, SchemaType}

// The encoding of newtype libraries (scala-newtype's @newtype expansion, neotype/monix-newtypes' `Newtype`): an
// abstract type member `Type` of an object standing for the type. All of them share the member's short name `Type`.
object TmKeyA {
  type Base = Any
  trait Tag
  type Type <: Base with Tag
  def apply(value: String): Type = value.asInstanceOf[Type]
  implicit val keyEncoder: KeyEncoder[Type] = KeyEncoder.instance(_.asInstanceOf[String])
}
object TmKeyB {
  type Base = Any
  trait Tag
  type Type <: Base with Tag
  def apply(value: String): Type = value.asInstanceOf[Type]
  implicit val keyEncoder: KeyEncoder[Type] = KeyEncoder.instance(_.asInstanceOf[String])
}
object TmValA {
  type Base = Any
  trait Tag
  type Type <: Base with Tag
  implicit val schema: Schema[Type] = Schema.schemaForString.as[Type]
}
object TmValB {
  type Base = Any
  trait Tag
  type Type <: Base with Tag
  implicit val schema: Schema[Type] = Schema.schemaForInt.as[Type]
}

final case class TmBox[A](value: A)

final case class TmMaps(
    first: Map[TmKeyA.Type, TmValA.Type],
    second: Map[TmKeyB.Type, TmValB.Type],
    stringKeyed: Map[String, TmValA.Type],
    boxed: TmBox[TmValB.Type]
)

/** Named schemas involving a type member of an object (`Name.Type`) are named after the object (`Name`), so that two
  * different maps (e.g. `Map[KeyA.Type, ValA.Type]` and `Map[KeyB.Type, ValB.Type]`) never share a name.
  */
final class TypeMemberNamingSpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default

  private def fieldNames(schema: Schema[?]): Map[String, Option[Schema.SName]] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.name -> f.schema.name).toMap
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  private val pkg = "hearth.kindlings.tapirschemaderivation"

  group("SNames of types which are type members of objects") {

    test("maps keyed and valued by them are named after the objects, and never collide") {
      val names = fieldNames(KindlingsSchema.derived[TmMaps].schema)
      assertEquals(names("first"), Some(Schema.SName("Map", List(s"$pkg.TmKeyA", s"$pkg.TmValA"))))
      assertEquals(names("second"), Some(Schema.SName("Map", List(s"$pkg.TmKeyB", s"$pkg.TmValB"))))
      assertEquals(names("stringKeyed"), Some(Schema.SName("Map", List(s"$pkg.TmValA"))))
      assertNotEquals(names("first"), names("second"))
    }

    test("generic case classes applied to them are named after the objects") {
      val names = fieldNames(KindlingsSchema.derived[TmMaps].schema)
      assertEquals(names("boxed"), Some(Schema.SName(s"$pkg.TmBox", List(s"$pkg.TmValB"))))
    }
  }
}
