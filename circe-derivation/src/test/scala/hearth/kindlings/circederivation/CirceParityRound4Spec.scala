package hearth.kindlings.circederivation

import hearth.MacroSuite
import io.circe.Json

// Types for the fourth round of upstream-circe parity tests.

// A value class over Option, as a field with a default.
final case class R4MaybeName(v: Option[String]) extends AnyVal
case class R4WithMaybeDefault(name: R4MaybeName = R4MaybeName(Some("dflt")), plain: Option[String] = Some("p"))

// Incomplete decoders with labelled supplied fields.
case class R4Qux(i: Int, a: String, j: Int)

// A universal-trait sum with an AnyVal child, and a value class, inside a collection / Option.
final case class R4Plate(value: String) extends AnyVal
sealed trait R4Vehicle extends Any
case class R4Car(seats: Int) extends R4Vehicle
final case class R4Badge(code: Int) extends AnyVal with R4Vehicle
case class R4Garage(vehicles: List[R4Vehicle], maybePlate: Option[R4Plate])
object R4GarageCodecs {
  implicit val config: Configuration = Configuration.default
  val encoder: io.circe.Encoder[R4Garage] = KindlingsEncoder.derived[R4Garage]
  val decoder: io.circe.Decoder[R4Garage] = KindlingsDecoder.derived[R4Garage]
  val optEncoder: io.circe.Encoder[R4OnlyOpt] = KindlingsEncoder.derived[R4OnlyOpt]
  val listEncoder: io.circe.Encoder[R4OnlyList] = KindlingsEncoder.derived[R4OnlyList]
}
case class R4OnlyOpt(maybePlate: Option[R4Plate])
case class R4OnlyList(vehicles: List[R4Vehicle])

final class CirceParityRound4Spec extends MacroSuite {

  group("upstream circe parity, round 4") {

    test("a universal-trait sum and a value class in a List / Option (derived in an object)") {
      val value = R4Garage(List(R4Car(2), R4Badge(7)), Some(R4Plate("X")))
      val json = R4GarageCodecs.encoder(value)
      json ==> Json.obj(
        "vehicles" -> Json.arr(
          Json.obj("R4Car" -> Json.obj("seats" -> Json.fromInt(2))),
          Json.obj("R4Badge" -> Json.fromInt(7))
        ),
        "maybePlate" -> Json.fromString("X")
      )
      R4GarageCodecs.decoder.decodeJson(json) ==> Right(value)
    }

    group("an absent value-class-over-Option field with a default, under useDefaults") {

      test("decodes to the default, like a plain Option field (fail-fast and accumulating)") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val expected = Right(R4WithMaybeDefault())
        KindlingsDecoder.decode[R4WithMaybeDefault](Json.obj()) ==> expected
        KindlingsDecoder.derived[R4WithMaybeDefault].decodeJson(Json.obj()) ==> expected
        KindlingsDecoder.derived[R4WithMaybeDefault].decodeAccumulating(Json.obj().hcursor).toEither ==> expected
        KindlingsCodecAsObject.derived[R4WithMaybeDefault].decodeJson(Json.obj()) ==> expected
      }

      test("an explicit null is None, like a plain Option field") {
        implicit val config: Configuration = Configuration.default.withDefaults
        val json = Json.obj("name" -> Json.Null, "plain" -> Json.Null)
        val expected = Right(R4WithMaybeDefault(R4MaybeName(None), None))
        KindlingsDecoder.decode[R4WithMaybeDefault](json) ==> expected
        KindlingsDecoder.derived[R4WithMaybeDefault].decodeJson(json) ==> expected
        KindlingsDecoder.derived[R4WithMaybeDefault].decodeAccumulating(json.hcursor).toEither ==> expected
      }

      test("without useDefaults an absent field is None") {
        val expected = Right(R4WithMaybeDefault(R4MaybeName(None), None))
        KindlingsDecoder.decode[R4WithMaybeDefault](Json.obj()) ==> expected
        KindlingsDecoder.derived[R4WithMaybeDefault].decodeJson(Json.obj()) ==> expected
      }

      test("with useDefaults only known at runtime") {
        implicit val runtimeConfig: Configuration = Configuration.default.copy(useDefaults = System.nanoTime() > 0L)
        val decoder = KindlingsDecoder.derived[R4WithMaybeDefault]
        decoder.decodeJson(Json.obj()) ==> Right(R4WithMaybeDefault())
        decoder.decodeAccumulating(Json.obj().hcursor).toEither ==> Right(R4WithMaybeDefault())
      }
    }
    group("incomplete decoders with labelled fields (circe-generic's shapeless FieldType)") {

      test("a Labelled parameter supplies the field of that name") {
        val decoder: io.circe.Decoder[Labelled["j", Int] => R4Qux] =
          KindlingsDecoder.incomplete[Labelled["j", Int] => R4Qux]
        val json = Json.obj("i" -> Json.fromInt(1), "a" -> Json.fromString("x"))
        decoder.decodeJson(json).map(_(Labelled["j", Int](3))) ==> Right(R4Qux(1, "x", 3))
      }

      test("labelled and by-type parameters together (labelled ones are selected first)") {
        val decoder = KindlingsDecoder.incomplete[(Int, Labelled["i", Int]) => R4Qux]
        decoder.decodeJson(Json.obj("a" -> Json.fromString("x"))).map(_(3, Labelled["i", Int](1))) ==>
          Right(R4Qux(1, "x", 3))
      }

      test("configured names apply to the decoded fields") {
        implicit val config: Configuration = Configuration.default.withTransformMemberNames(_.toUpperCase)
        val decoder = KindlingsDecoder.incomplete[Labelled["a", String] => R4Qux]
        decoder
          .decodeJson(Json.obj("I" -> Json.fromInt(1), "J" -> Json.fromInt(2)))
          .map(_(Labelled["a", String]("y"))) ==>
          Right(R4Qux(1, "y", 2))
      }

      test("an unknown field name or a wrong type is a compile-time error") {
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsDecoder, Labelled, R4Qux}
          KindlingsDecoder.incomplete[Labelled["k", Int] => R4Qux]
          """
        ).check("has no field named k")
        compileErrors(
          """
          import hearth.kindlings.circederivation.{KindlingsDecoder, Labelled, R4Qux}
          KindlingsDecoder.incomplete[Labelled["a", Int] => R4Qux]
          """
        ).check("has type")
      }
    }
  }
}
