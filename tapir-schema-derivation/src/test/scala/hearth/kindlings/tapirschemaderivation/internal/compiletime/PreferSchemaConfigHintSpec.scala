package hearth.kindlings.tapirschemaderivation.internal.compiletime

import hearth.MacroSuite

/** The tapir-schema tests set `tapirSchemaDerivation.preferConfig=circe` module-wide, so the error itself cannot be
  * triggered there; its text is checked here (and that its suggestions compile, in `PreferConfigHintSpec`).
  */
final class PreferSchemaConfigHintSpec extends MacroSuite {

  test("the hint lists a compiling setting and implicit for every configuration found") {
    val message = PreferSchemaConfigHint.multipleConfigurations(
      "KindlingsSchema.derived",
      "tapirSchemaDerivation",
      List(
        PreferSchemaConfigHint.Found("circe", "hearth.kindlings.circederivation.Configuration"),
        PreferSchemaConfigHint.Found("jsoniter-scala", "hearth.kindlings.jsoniterderivation.JsoniterConfig")
      )
    )
    message ==>
      """KindlingsSchema.derived: Multiple JSON library configurations found: circe, jsoniter-scala.
        |Disambiguate by adding one of the scalac settings:
        |  scalacOptions += "-Xmacro-settings:tapirSchemaDerivation.preferConfig=circe"
        |  scalacOptions += "-Xmacro-settings:tapirSchemaDerivation.preferConfig=jsoniter-scala"
        |Or one of the implicits in an enclosing object (not a class body):
        |  implicit val prefer: hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.circederivation.Configuration] = hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.circederivation.Configuration]
        |  implicit val prefer: hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.jsoniterderivation.JsoniterConfig] = hearth.kindlings.tapirschemaderivation.PreferSchemaConfig[hearth.kindlings.jsoniterderivation.JsoniterConfig]""".stripMargin
  }
}
