package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** ADT / enum behaviours aligned with jsoniter-scala's `JsonCodecMaker.make` (v2.42.3). The `Spec ~N` references point
  * at upstream's `JsonCodecMakerSpec.scala`.
  *
  * Config-dependent scenarios run on the optimized codegen (`OptimizedCodecs`) and on the generic one (configs read
  * from `@volatile var`s, unknown at compile time).
  */
final class JsoniterScalaAdtParitySpec extends MacroSuite {

  private def decodeError(fragment: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(error.getMessage.contains(fragment), s"expected an error containing '$fragment', got: ${error.getMessage}")
  }

  private def runtime[A](config: JsoniterConfig)(derive: JsoniterConfig => A): A = derive(config)

  group("jsoniter-scala ADT parity") {

    group("requireDiscriminatorFirst (Spec ~2681, ~2829)") {
      val all = List[DiscAdt](DiscA(1), DiscB(BigInt(1)), DiscC(1, "VVV"), DiscE(), DiscD)

      test("with false, the discriminator may appear anywhere in the object") {
        val cRuntime =
          runtime(DiscRuntimeConfigs.anywhere)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhere, cRuntime).foreach { c =>
          readFromString[List[DiscAdt]](
            """[{"type":"DiscA","a":1},{"a":1,"type":"DiscB"},{"a":1,"type":"DiscC","b":"VVV"},{"x":1,"type":"DiscE"},{"y":[1],"type":"DiscD","z":2}]"""
          )(c) ==> all
          writeToString(all)(c) ==>
            """[{"type":"DiscA","a":1},{"type":"DiscB","a":1},{"type":"DiscC","a":1,"b":"VVV"},{"type":"DiscE"},{"type":"DiscD"}]"""
        }
        val tRuntime =
          runtime(DiscRuntimeConfigs.anywhereT)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhereT, tRuntime).foreach { c =>
          readFromString[List[DiscAdt]]("""[{"a":2,"b":"WWW","t":"DiscC"},{"a":1,"t":"DiscC","b":"VVV"}]""")(c) ==>
            List(DiscC(2, "WWW"), DiscC(1, "VVV"))
        }
      }

      test("with false, a discriminator far away in the last position is found (Spec ~2822)") {
        val cRuntime =
          runtime(DiscRuntimeConfigs.anywhere)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        val longStr = "W" * 100000
        val longBigInt = BigInt("9" * 300) // bigIntDigitsLimit is tested in JsoniterScalaParitySpec
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhere, cRuntime).foreach { c =>
          readFromString[List[DiscAdt]](
            s"""[{"a":2,"b":"$longStr","type":"DiscC"},{"a":1,"type":"DiscC","b":"VVV"}]"""
          )(
            c
          ) ==> List(DiscC(2, longStr), DiscC(1, "VVV"))
          readFromString[List[DiscAdt]](s"""[{"a":$longBigInt,"type":"DiscB"},{"a":1,"type":"DiscB"}]""")(c) ==>
            List(DiscB(longBigInt), DiscB(BigInt(1)))
        }
      }

      test("with false, a missing discriminator is a missing required field") {
        val cRuntime =
          runtime(DiscRuntimeConfigs.anywhere)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhere, cRuntime).foreach { c =>
          decodeError("""missing required field "type"""")(readFromString[List[DiscAdt]]("""[{"a":1}]""")(c))
          decodeError("""illegal value of discriminator field "type"""")(
            readFromString[List[DiscAdt]]("""[{"a":1,"type":"XXX"}]""")(c)
          )
        }
      }

      test("with false and skipUnexpectedFields = false, the discriminator is not an unexpected field") {
        val cRuntime =
          runtime(DiscRuntimeConfigs.anywhereStrict)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhereStrict, cRuntime).foreach { c =>
          readFromString[List[DiscAdt]]("""[{"a":1,"type":"DiscC","b":"VVV"},{"type":"DiscE"}]""")(c) ==>
            List(DiscC(1, "VVV"), DiscE())
          decodeError("""unexpected field "x"""")(
            readFromString[List[DiscAdt]]("""[{"a":1,"x":2,"type":"DiscA"}]""")(c)
          )
          decodeError("""unexpected field "x"""")(readFromString[List[DiscAdt]]("""[{"x":2,"type":"DiscE"}]""")(c))
        }
      }

      test("with false and checkFieldDuplication, a second discriminator is a duplicate") {
        val cRuntime = runtime(DiscRuntimeConfigs.anywhereCheckDuplication) { implicit c =>
          KindlingsJsonValueCodec.derived[List[DiscAdt]]
        }
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discAnywhereCheckDuplication, cRuntime).foreach { c =>
          decodeError("""duplicated field "type"""")(
            readFromString[List[DiscAdt]]("""[{"a":1,"type":"DiscA","type":"DiscA"}]""")(c)
          )
          decodeError("""duplicated field "type"""")(
            readFromString[List[DiscAdt]]("""[{"type":"DiscE","type":"DiscE"}]""")(c)
          )
        }
      }

      test("by default (true), the discriminator must be the first field") {
        val cRuntime = runtime(DiscRuntimeConfigs.first)(implicit c => KindlingsJsonValueCodec.derived[List[DiscAdt]])
        List[JsonValueCodec[List[DiscAdt]]](OptimizedCodecs.discFirst, cRuntime).foreach { c =>
          readFromString[List[DiscAdt]]("""[{"type":"DiscC","a":1,"b":"VVV"}]""")(c) ==> List(DiscC(1, "VVV"))
          decodeError("""expected key: "type"""")(readFromString[List[DiscAdt]]("""[{"a":1,"type":"DiscA"}]""")(c))
          decodeError("""illegal value of discriminator field "type"""")(
            readFromString[List[DiscAdt]]("""[{"type":"XXX","a":1}]""")(c)
          )
        }
      }
    }
    group("case objects and enums as strings (Spec ~640, ~784, ~857, ~2687, ~2704)") {
      val adts = List[ParityAdt](ParityAAA(1), ParityDDD)

      test("without a discriminator, case objects of ADTs are strings") {
        val cRuntime =
          runtime(EnumRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[List[ParityAdt]])
        List[JsonValueCodec[List[ParityAdt]]](KindlingsJsonValueCodec.derived[List[ParityAdt]], cRuntime).foreach { c =>
          writeToString(adts)(c) ==> """[{"ParityAAA":{"a":1}},"ParityDDD"]"""
          readFromString[List[ParityAdt]]("""[{"ParityAAA":{"a":1}},"ParityDDD"]""")(c) ==> adts
          // Kindlings superset (NOT upstream parity): the circe-like form is accepted on decoding too
          readFromString[List[ParityAdt]]("""[{"ParityDDD":{}}]""")(c) ==> List(ParityDDD)
          decodeError("illegal discriminator")(readFromString[List[ParityAdt]]("""["XXX"]""")(c))
          decodeError("illegal discriminator")(readFromString[List[ParityAdt]]("""[{"XXX":{}}]""")(c))
        }
        val dirRuntime =
          runtime(EnumRuntimeConfigs.default)(implicit c => KindlingsJsonValueCodec.derived[List[CardinalDirection]])
        List[JsonValueCodec[List[CardinalDirection]]](
          KindlingsJsonValueCodec.derived[List[CardinalDirection]],
          dirRuntime
        )
          .foreach { c =>
            writeToString(List[CardinalDirection](North, West))(c) ==> """["North","West"]"""
            readFromString[List[CardinalDirection]]("""["North","West"]""")(c) ==> List(North, West)
          }
      }

      test("with circeLikeObjectEncoding, case objects are wrapped empty objects") {
        val cRuntime =
          runtime(EnumRuntimeConfigs.circeLike)(implicit c => KindlingsJsonValueCodec.derived[List[ParityAdt]])
        List[JsonValueCodec[List[ParityAdt]]](OptimizedCodecs.adtCirceLike, cRuntime).foreach { c =>
          writeToString(adts)(c) ==> """[{"ParityAAA":{"a":1}},{"ParityDDD":{}}]"""
          readFromString[List[ParityAdt]]("""[{"ParityAAA":{"a":1}},{"ParityDDD":{}}]""")(c) ==> adts
        }
      }

      test("with a discriminator, case objects are objects with the discriminator") {
        val cRuntime =
          runtime(EnumRuntimeConfigs.discriminator)(implicit c => KindlingsJsonValueCodec.derived[List[ParityAdt]])
        List[JsonValueCodec[List[ParityAdt]]](OptimizedCodecs.adtDiscriminator, cRuntime).foreach { c =>
          writeToString(adts)(c) ==> """[{"type":"ParityAAA","a":1},{"type":"ParityDDD"}]"""
          readFromString[List[ParityAdt]]("""[{"type":"ParityAAA","a":1},{"type":"ParityDDD"}]""")(c) ==> adts
        }
        val dirRuntime = runtime(EnumRuntimeConfigs.discriminator) { implicit c =>
          KindlingsJsonValueCodec.derived[List[CardinalDirection]]
        }
        List[JsonValueCodec[List[CardinalDirection]]](OptimizedCodecs.directionsDiscriminator, dirRuntime).foreach { c =>
          writeToString(List[CardinalDirection](North))(c) ==> """[{"type":"North"}]"""
        }
      }

      test("enumAsStrings writes ADTs of case objects as strings even with a discriminator") {
        val dirRuntime = runtime(EnumRuntimeConfigs.discriminatorEnumAsStrings) { implicit c =>
          KindlingsJsonValueCodec.derived[List[CardinalDirection]]
        }
        List[JsonValueCodec[List[CardinalDirection]]](OptimizedCodecs.directionsEnumAsStrings, dirRuntime).foreach {
          c =>
            writeToString(List[CardinalDirection](North, West))(c) ==> """["North","West"]"""
            readFromString[List[CardinalDirection]]("""["North",{"type":"West"}]""")(c) ==> List(North, West)
            decodeError("illegal discriminator")(readFromString[List[CardinalDirection]]("""["XXX"]""")(c))
        }
      }

      test("scala.Enumeration values are strings (their toString) whatever the discriminator, also as map keys") {
        val value = ParityEnums(ParityLocation.Named, Map(ParityLocation.GPS -> 1, ParityLocation.Named -> 2))
        val json = """{"lt":"named-value","m":{"GPS":1,"named-value":2}}"""
        val cRuntime =
          runtime(EnumRuntimeConfigs.discriminator)(implicit c => KindlingsJsonValueCodec.derived[ParityEnums])
        List[JsonValueCodec[ParityEnums]](
          KindlingsJsonValueCodec.derived[ParityEnums],
          OptimizedCodecs.enumsDiscriminator,
          cRuntime
        ).foreach { c =>
          writeToString(value)(c) ==> json
          readFromString[ParityEnums](json)(c) ==> value
          decodeError("illegal enum value \"XXX\"")(readFromString[ParityEnums]("""{"lt":"XXX","m":{}}""")(c))
        }
      }

      test("scala.Enumeration values with useScalaEnumValueId are ids, also as map keys") {
        val value = ParityEnums(ParityLocation.GPS, Map(ParityLocation.Named -> 2))
        val json = """{"lt":1,"m":{"2":2}}"""
        val cRuntime = runtime(EnumRuntimeConfigs.enumIds)(implicit c => KindlingsJsonValueCodec.derived[ParityEnums])
        List[JsonValueCodec[ParityEnums]](OptimizedCodecs.enumIds, cRuntime).foreach { c =>
          writeToString(value)(c) ==> json
          readFromString[ParityEnums](json)(c) ==> value
          decodeError("illegal enum value 7")(readFromString[ParityEnums]("""{"lt":7,"m":{}}""")(c))
        }
      }
    }
    group("@named on ADT leaves (Spec ~2900-2910)") {

      test("overrides the discriminator value and the wrapper key, and is not mapped") {
        val values = List[NamedBase](NamedA(NamedB("x")), NamedB("x"))
        val discriminator = {
          implicit val config: JsoniterConfig =
            JsoniterConfig.default
              .withDiscriminator("type")
              .withAdtLeafClassNameMapper(JsoniterConfig.simpleClassName.andThen(_.toLowerCase))
          KindlingsJsonValueCodec.derived[List[NamedBase]]
        }
        writeToString(values)(discriminator) ==> """[{"type":"X","b":{"c":"x"}},{"type":"Y","c":"x"}]"""
        readFromString[List[NamedBase]]("""[{"type":"X","b":{"c":"x"}},{"type":"Y","c":"x"}]""")(
          discriminator
        ) ==> values
        val wrapper = KindlingsJsonValueCodec.derived[List[NamedBase]]
        writeToString(values)(wrapper) ==> """[{"X":{"b":{"c":"x"}}},{"Y":{"c":"x"}}]"""
      }

      test("renames case objects, e.g. names with dots") {
        val codec = KindlingsJsonValueCodec.derived[List[NamedVersion]]
        writeToString(List[NamedVersion](Version809, Version810))(codec) ==> """["8.09","8.10"]"""
        readFromString[List[NamedVersion]]("""["8.09","8.10"]""")(codec) ==> List(Version809, Version810)
      }
    }
  }
}
