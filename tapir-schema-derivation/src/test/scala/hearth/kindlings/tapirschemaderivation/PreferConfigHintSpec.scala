package hearth.kindlings.tapirschemaderivation

import hearth.MacroSuite
import hearth.kindlings.circederivation.Configuration
import hearth.kindlings.jsoniterderivation.JsoniterConfig
import sttp.tapir.Schema

final case class PreferHintPerson(name: String, age: Int)

/** The declarations the "Multiple JSON library configurations found" error suggests (see
  * `internal.compiletime.PreferSchemaConfigHintSpec`) compile as written.
  */
final class PreferConfigHintSpec extends MacroSuite {

  implicit val config: Configuration = Configuration.default
  implicit val jsoniterConfig: JsoniterConfig = JsoniterConfig.default

  group("PreferSchemaConfig declarations suggested by the error") {

    test("circe") {
      implicit val prefer: hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[
        hearth.kindlings.circederivation.Configuration
      ] = hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.circederivation.Configuration]
      val _ = prefer // used by the macro (implicit search), which the unused-symbol lint does not see
      val schema: Schema[PreferHintPerson] = KindlingsSchema.derived[PreferHintPerson].schema
      assert(schema.name.isDefined)
    }

    test("jsoniter-scala") {
      implicit val prefer: hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[
        hearth.kindlings.jsoniterderivation.JsoniterConfig
      ] = hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.jsoniterderivation.JsoniterConfig]
      val _ = prefer
      val schema: Schema[PreferHintPerson] = KindlingsSchema.derived[PreferHintPerson].schema
      assert(schema.name.isDefined)
    }
  }
}
