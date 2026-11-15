package hearth.kindlings.oracle.newtype

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration as CirceConfig
import hearth.kindlings.oracle.Oracle
import hearth.kindlings.tapirschemaderivation.KindlingsSchema
import io.estatico.newtype.macros.{newsubtype, newtype}
import sttp.tapir.{Schema, SchemaType}
import sttp.tapir.generic.Configuration as TapirConfig

// @newtype expands to a type alias + companion, and Scala 2 has no top-level type aliases.
object NwTypes {
  @newtype case class NwName(value: String)
  @newsubtype case class NwAge(value: Int)
  @newtype case class NwTag(value: String)
}
import NwTypes.*

final case class NwAccount(name: NwName, age: NwAge, nickname: Option[NwName])

final case class NwKeyed(byName: Map[NwName, Int], byTag: Map[NwTag, NwName])

/** Kindlings with `kindlings-newtype-integration` vs tapir-newtype (Scala 2.13 only: tapir-newtype is not published for
  * Scala 3), which coerces the wrapped type's schema. scala-newtype has no circe or jsoniter integration.
  */
final class NewtypeOracleSpec extends MacroSuite {

  private object KindlingsSide {
    implicit val circe: CirceConfig = CirceConfig.default
    def schema: Schema[NwAccount] = KindlingsSchema.derived[NwAccount].schema
    def keyed: Schema[NwKeyed] = KindlingsSchema.derived[NwKeyed].schema
  }

  private object KindlingsWithTapirNewtype {
    implicit val circe: CirceConfig = CirceConfig.default
    import sttp.tapir.codec.newtype.*
    def schema: Schema[NwAccount] = KindlingsSchema.derived[NwAccount].schema
  }

  private object UpstreamSchema {
    implicit val tapir: TapirConfig = TapirConfig.default
    import sttp.tapir.codec.newtype.*
    import sttp.tapir.generic.auto.*
    def schema: Schema[NwAccount] = implicitly[Schema[NwAccount]]
    // tapir derives no schema for maps with non-String keys: its users declare `Schema.schemaForMap[K, V]`
    def byName: Schema[Map[NwName, Int]] = Schema.schemaForMap[NwName, Int](_.value)
    def byTag: Schema[Map[NwTag, NwName]] = Schema.schemaForMap[NwTag, NwName](_.value)
  }

  group("scala-newtype oracle") {

    test("tapir schema without tapir-newtype vs tapir-newtype: identical") {
      val differences = Oracle.diff(Oracle.jsonSchema(KindlingsSide.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(differences, Nil, s"Kindlings vs tapir-newtype differences:\n${differences.mkString("\n")}")
    }

    test("tapir schema with tapir-newtype imported vs tapir-newtype: identical") {
      val differences =
        Oracle.diff(Oracle.jsonSchema(KindlingsWithTapirNewtype.schema), Oracle.jsonSchema(UpstreamSchema.schema))
      assertEquals(differences, Nil, s"Kindlings + tapir-newtype vs tapir-newtype:\n${differences.mkString("\n")}")
    }

    test("the names of maps keyed by newtypes vs tapir's Schema.schemaForMap") {
      val names = KindlingsSide.keyed.schemaType match {
        case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.name -> f.schema.name).toMap
        case other                     => fail(s"Expected SProduct, got: $other")
      }
      // Documented (tapir guide, "Maps, collections and other built-in types"): Kindlings names a newtype after its
      // companion (`Map_NwName_Int`); tapir after the type member (`NwName.Type`, i.e. `Map_Type_Int` in OpenAPI, the
      // same short name for every newtype)
      def companionNamed(name: Option[Schema.SName]): Option[Schema.SName] =
        name.map(n => n.copy(typeParameterShortNames = n.typeParameterShortNames.map(_.stripSuffix(".Type"))))
      assertEquals(names("byName"), companionNamed(UpstreamSchema.byName.name))
      assertEquals(names("byTag"), companionNamed(UpstreamSchema.byTag.name))
      assertNotEquals(names("byName"), names("byTag"))
    }
  }
}
