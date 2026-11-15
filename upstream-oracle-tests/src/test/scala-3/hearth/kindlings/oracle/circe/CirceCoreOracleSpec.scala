package hearth.kindlings.oracle.circe

import hearth.MacroSuite
import hearth.kindlings.circederivation.{
  renaming as kRenaming,
  Configuration,
  KindlingsCodecAsObject,
  KindlingsDecoder,
  KindlingsEncoder
}
import io.circe.Codec
import io.circe.derivation.{
  renaming as cRenaming,
  Configuration as CoreConfiguration,
  ConfiguredCodec,
  ConfiguredEnumCodec
}

/** Kindlings vs Scala 3 circe-core's own derivation: `ConfiguredCodec.derived` / `ConfiguredEnumCodec.derived` (what
  * `derives ConfiguredCodec` calls) for Scala 3 enums, and the named-argument factories (`ConfiguredCodec.derive(...)`
  * vs `KindlingsCodecAsObject.derive(...)`).
  */
final class CirceCoreOracleSpec extends MacroSuite {

  private def check(differences: List[CiDiff], expected: List[CiExpected] = Nil): Unit = {
    val problems = CirceOracle.unexplained(differences, expected)
    assert(problems.isEmpty, s"Kindlings vs circe-core differences:\n${problems.mkString("\n")}")
  }

  private val planets = CiPlanet.values.toList
  private val vehicles = List(CiVehicle.Car(4), CiVehicle.Bike, CiVehicle.Truck(1.5, 3))
  private val garages = List(CiGarage(vehicles, Some(CiPlanet.EarthLike)), CiGarage(Nil, None))

  test("Scala 3 enum of parameterless cases: ConfiguredEnumCodec vs deriveEnum") {
    val inputs = List("\"Mercury\"", "\"earth_like\"", "\"EarthLike\"", "\"Pluto\"", "null", "1", "{\"Venus\":{}}")
    val plain = {
      given CoreConfiguration = CoreConfiguration.default
      CirceOracle.compare(
        "planet",
        CiSamples.pair(KindlingsCodecAsObject.deriveEnum[CiPlanet](), ConfiguredEnumCodec.derived[CiPlanet]),
        planets,
        inputs
      )
    }
    val snake = {
      given CoreConfiguration = CoreConfiguration.default.withSnakeCaseConstructorNames
      CirceOracle.compare(
        "planet snake",
        CiSamples.pair(
          KindlingsCodecAsObject.deriveEnum[CiPlanet](kRenaming.snakeCase),
          ConfiguredEnumCodec.derived[CiPlanet]
        ),
        planets,
        inputs
      )
    }
    check(plain ++ snake)
  }

  test("Scala 3 enum with parameterised cases: ConfiguredCodec.derived, wrapped and discriminated") {
    def compare(label: String, k: Configuration, u: CoreConfiguration): List[CiDiff] = {
      given CoreConfiguration = u
      given Configuration = k
      CirceOracle.compare(
        label,
        CiSamples.pair(KindlingsCodecAsObject.derived[CiVehicle], ConfiguredCodec.derived[CiVehicle]),
        vehicles,
        List("""{"Car":{"wheelCount":4},"Bike":{}}""", """{"type":"Bike","extra":1}""", """{"Bike":null}""")
      )
    }
    check(
      compare("vehicle", Configuration.default, CoreConfiguration.default) ++
        compare(
          "vehicle discriminator snake strict",
          Configuration.default.withDiscriminator("type").withSnakeCaseMemberNames.withStrictDecoding,
          CoreConfiguration.default.withDiscriminator("type").withSnakeCaseMemberNames.withStrictDecoding
        ) ++
        compare(
          "vehicle kebab constructors defaults strict",
          Configuration.default.withKebabCaseConstructorNames.withDefaults.withStrictDecoding,
          CoreConfiguration.default.withKebabCaseConstructorNames.withDefaults.withStrictDecoding
        ),
      List(CiExpected.hearthChildrenOrder)
    )
  }

  test("product of enums, with an enum codec in scope") {
    val k = {
      given Codec[CiPlanet] = KindlingsCodecAsObject.deriveEnum[CiPlanet]()
      given Configuration = Configuration.default.withPascalCaseMemberNames
      KindlingsCodecAsObject.derived[CiGarage]
    }
    val u = {
      given CoreConfiguration = CoreConfiguration.default.withPascalCaseMemberNames
      given Codec[CiPlanet] = ConfiguredEnumCodec.derived[CiPlanet]
      given Codec[CiVehicle] = ConfiguredCodec.derived[CiVehicle]
      ConfiguredCodec.derived[CiGarage]
    }
    check(CirceOracle.compare("garage", CiSamples.pair(k, u), garages))
  }

  test("named-argument factories and renaming.replaceWith") {
    val k = KindlingsCodecAsObject.derive[CiShape](
      transformMemberNames = kRenaming.screamingSnakeCase,
      transformConstructorNames = kRenaming.replaceWith("CiCircle" -> "round", "CiNoShape" -> "none"),
      discriminator = Some("kind"),
      strictDecoding = true
    )
    val u =
      ConfiguredCodec.derive[CiShape](
        transformMemberNames = cRenaming.screamingSnakeCase,
        transformConstructorNames = cRenaming.replaceWith("CiCircle" -> "round", "CiNoShape" -> "none"),
        discriminator = Some("kind"),
        strictDecoding = true
      )
    check(
      CirceOracle.compare(
        "factories",
        CiSamples.pair(k, u),
        CiSamples.shapes,
        List("""{"kind":"round","RADIUS":1.0}""", """{"kind":"CiCircle","RADIUS":1.0}""", """{"kind":"none"}""")
      )
    )
  }

  test("encoders and decoders derived separately (KindlingsEncoder / KindlingsDecoder)") {
    given CoreConfiguration = CoreConfiguration.default.withDiscriminator("t")
    given Configuration = Configuration.default.withDiscriminator("t")
    val u = ConfiguredCodec.derived[CiVehicle]
    check(
      CirceOracle.compare(
        "separate",
        CiSamples.pair(KindlingsEncoder.derived[CiVehicle], KindlingsDecoder.derived[CiVehicle], u, u),
        vehicles
      )
    )
  }
}
