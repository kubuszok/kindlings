package hearth.kindlings.integrationtests

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonValueCodec}
import hearth.MacroSuite
import hearth.kindlings.circederivation.{Configuration, KindlingsDecoder, KindlingsEncoder}
import hearth.kindlings.jsoniterderivation.{JsoniterConfig, KindlingsJsonValueCodec}
import hearth.kindlings.tapirschemaderivation.{KindlingsSchema, PreferSchemaConfig}
import io.circe.{Decoder, Encoder, Json}
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.any.{Not, Pure}
import io.github.iltotore.iron.constraint.numeric.Positive
import io.github.iltotore.iron.constraint.string.Blank
import sttp.tapir.{Schema, SchemaType}

import scala.util.Try

// Iron 3 `RefinedType`/`RefinedSubtype` companions define an opaque `T = A :| C` (the report's reproduction:
// histmersion notes/repros/kindlings034-iron-refinedtype-map-key).
object RtKey extends RefinedType[String, Pure]
type RtKey = RtKey.T

object RtName extends RefinedType[String, Not[Blank]]
type RtName = RtName.T

object RtCount extends RefinedType[Int, Positive]
type RtCount = RtCount.T

object RtSubName extends RefinedSubtype[String, Not[Blank]]
type RtSubName = RtSubName.T

final case class RtCounts(byKey: Map[RtKey, Int])
final case class RtPlainField(k: RtKey)
final case class RtFields(name: RtName, count: RtCount, sub: RtSubName)
final case class RtKeyed(byName: Map[RtName, Int], byCount: Map[RtCount, String])
// control from the report: a bare Iron type as a map key
final case class RtBareIronKey(byKey: Map[String :| Pure, Int])

object RtJsoniter {
  given config: JsoniterConfig = JsoniterConfig()
  given prefer: PreferSchemaConfig[JsoniterConfig] = PreferSchemaConfig[JsoniterConfig]

  val countsSchema: Schema[RtCounts] = KindlingsSchema.derived[RtCounts].schema
  val countsCodec: JsonValueCodec[RtCounts] = KindlingsJsonValueCodec.derived
  val plainSchema: Schema[RtPlainField] = KindlingsSchema.derived[RtPlainField].schema
  val plainCodec: JsonValueCodec[RtPlainField] = KindlingsJsonValueCodec.derived
  val fieldsSchema: Schema[RtFields] = KindlingsSchema.derived[RtFields].schema
  val fieldsCodec: JsonValueCodec[RtFields] = KindlingsJsonValueCodec.derived
  val keyedSchema: Schema[RtKeyed] = KindlingsSchema.derived[RtKeyed].schema
  val keyedCodec: JsonValueCodec[RtKeyed] = KindlingsJsonValueCodec.derived
  val bareSchema: Schema[RtBareIronKey] = KindlingsSchema.derived[RtBareIronKey].schema
  val bareCodec: JsonValueCodec[RtBareIronKey] = KindlingsJsonValueCodec.derived
}

object RtCirce {
  given config: Configuration = Configuration.default
  given prefer: PreferSchemaConfig[Configuration] = PreferSchemaConfig[Configuration]

  val fieldsSchema: Schema[RtFields] = KindlingsSchema.derived[RtFields].schema
  val fieldsEncoder: Encoder[RtFields] = KindlingsEncoder.derived
  val fieldsDecoder: Decoder[RtFields] = KindlingsDecoder.derived
  val keyedSchema: Schema[RtKeyed] = KindlingsSchema.derived[RtKeyed].schema
  val keyedEncoder: Encoder[RtKeyed] = KindlingsEncoder.derived
  val keyedDecoder: Decoder[RtKeyed] = KindlingsDecoder.derived
}

/** With kindlings-iron-integration, `RefinedType[A, C]#T` and `RefinedSubtype[A, C]#T` are value types wrapping `A`:
  * the schema and the codec of `A`, validated on decode, as fields and as map keys.
  */
final class IronRefinedTypeSpec extends MacroSuite {

  private def fields(schema: Schema[?]): Map[String, Schema[?]] = schema.schemaType match {
    case p: SchemaType.SProduct[?] => p.fields.map(f => f.name.encodedName -> (f.schema: Schema[?])).toMap
    case other                     => fail(s"Expected SProduct, got: $other")
  }

  private def openProductKeys(schema: Schema[?], value: Map[?, ?]): Set[String] = schema.schemaType match {
    case op: SchemaType.SOpenProduct[?, ?] =>
      op.asInstanceOf[SchemaType.SOpenProduct[Any, Any]].mapFieldValues(value).keySet
    case other => fail(s"Expected SOpenProduct, got: $other")
  }

  group("the report's reproduction: Map[Key.T, Int] (RefinedType[String, Pure])") {

    test("jsoniter codec round-trips") {
      val value = readFromString[RtCounts]("""{"byKey":{"a":1}}""")(RtJsoniter.countsCodec)
      assertEquals(value, RtCounts(Map(RtKey("a") -> 1)))
      assertEquals(writeToString(value)(RtJsoniter.countsCodec), """{"byKey":{"a":1}}""")
    }

    test("tapir schema is an SOpenProduct of integers keyed by the String") {
      val byKey = fields(RtJsoniter.countsSchema)("byKey")
      assertEquals(openProductKeys(byKey, Map(RtKey("a") -> 1)), Set("a"))
      byKey.schemaType match {
        case op: SchemaType.SOpenProduct[?, ?] => assert(op.valueSchema.schemaType.isInstanceOf[SchemaType.SInteger[?]])
        case other                             => fail(s"Expected SOpenProduct, got: $other")
      }
    }

    test("as a plain field: the schema of String and the String codec") {
      assert(fields(RtJsoniter.plainSchema)("k").schemaType.isInstanceOf[SchemaType.SString[?]])
      assertEquals(readFromString[RtPlainField]("""{"k":"a"}""")(RtJsoniter.plainCodec), RtPlainField(RtKey("a")))
    }

    test("control: a bare Iron type as a map key") {
      assertEquals(
        readFromString[RtBareIronKey]("""{"byKey":{"a":1}}""")(RtJsoniter.bareCodec).byKey.map((k, v) =>
          (k: String) -> v
        ),
        Map("a" -> 1)
      )
      assertEquals(openProductKeys(fields(RtJsoniter.bareSchema)("byKey"), Map(("a": String :| Pure) -> 1)), Set("a"))
    }
  }

  group("jsoniter") {

    test("RefinedType and RefinedSubtype fields round-trip with the underlying JSON values") {
      val value = RtFields(RtName("Alice"), RtCount(3), RtSubName("Bob"))
      val json = writeToString(value)(RtJsoniter.fieldsCodec)
      assertEquals(json, """{"name":"Alice","count":3,"sub":"Bob"}""")
      assertEquals(readFromString[RtFields](json)(RtJsoniter.fieldsCodec), value)
    }

    test("decoding validates the constraint") {
      assert(Try(readFromString[RtFields]("""{"name":" ","count":3,"sub":"Bob"}""")(RtJsoniter.fieldsCodec)).isFailure)
      assert(Try(readFromString[RtFields]("""{"name":"A","count":-1,"sub":"Bob"}""")(RtJsoniter.fieldsCodec)).isFailure)
      assert(Try(readFromString[RtFields]("""{"name":"A","count":1,"sub":""}""")(RtJsoniter.fieldsCodec)).isFailure)
    }

    test("RefinedType map keys (over String and Int) round-trip and are validated") {
      val value = RtKeyed(Map(RtName("a") -> 1), Map(RtCount(7) -> "x"))
      val json = writeToString(value)(RtJsoniter.keyedCodec)
      assertEquals(json, """{"byName":{"a":1},"byCount":{"7":"x"}}""")
      assertEquals(readFromString[RtKeyed](json)(RtJsoniter.keyedCodec), value)
      assert(Try(readFromString[RtKeyed]("""{"byName":{},"byCount":{"-7":"x"}}""")(RtJsoniter.keyedCodec)).isFailure)
    }

    test("tapir schemas: the underlying types, map keys as the codec writes them") {
      val f = fields(RtJsoniter.fieldsSchema)
      assert(f("name").schemaType.isInstanceOf[SchemaType.SString[?]])
      assert(f("count").schemaType.isInstanceOf[SchemaType.SInteger[?]])
      assert(f("sub").schemaType.isInstanceOf[SchemaType.SString[?]])
      val k = fields(RtJsoniter.keyedSchema)
      assertEquals(openProductKeys(k("byName"), Map(RtName("a") -> 1)), Set("a"))
      assertEquals(openProductKeys(k("byCount"), Map(RtCount(7) -> "x")), Set("7"))
    }
  }

  group("circe") {

    test("RefinedType and RefinedSubtype fields round-trip and are validated") {
      val value = RtFields(RtName("Alice"), RtCount(3), RtSubName("Bob"))
      val json = RtCirce.fieldsEncoder(value)
      assertEquals(
        json,
        Json.obj("name" -> Json.fromString("Alice"), "count" -> Json.fromInt(3), "sub" -> Json.fromString("Bob"))
      )
      assertEquals(RtCirce.fieldsDecoder.decodeJson(json), Right(value))
      assert(RtCirce.fieldsDecoder.decodeJson(json.deepMerge(Json.obj("count" -> Json.fromInt(0)))).isLeft)
    }

    test("RefinedType map keys round-trip and are validated") {
      val value = RtKeyed(Map(RtName("a") -> 1), Map(RtCount(7) -> "x"))
      val json = RtCirce.keyedEncoder(value)
      assertEquals(
        json,
        Json.obj("byName" -> Json.obj("a" -> Json.fromInt(1)), "byCount" -> Json.obj("7" -> Json.fromString("x")))
      )
      assertEquals(RtCirce.keyedDecoder.decodeJson(json), Right(value))
      assert(
        RtCirce.keyedDecoder
          .decodeJson(json.deepMerge(Json.obj("byCount" -> Json.obj("0" -> Json.fromString("x")))))
          .isLeft
      )
    }

    test("tapir schemas") {
      assert(fields(RtCirce.fieldsSchema)("count").schemaType.isInstanceOf[SchemaType.SInteger[?]])
      assertEquals(openProductKeys(fields(RtCirce.keyedSchema)("byCount"), Map(RtCount(7) -> "x")), Set("7"))
    }
  }
}
