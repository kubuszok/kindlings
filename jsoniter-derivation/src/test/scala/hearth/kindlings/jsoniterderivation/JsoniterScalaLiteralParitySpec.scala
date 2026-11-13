package hearth.kindlings.jsoniterderivation

import com.github.plokhotnyuk.jsoniter_scala.core.{readFromString, writeToString, JsonReaderException, JsonValueCodec}
import hearth.MacroSuite

/** Literal (singleton) types as in jsoniter-scala's `JsonCodecMakerLiteralTypesSpec`. */
final class JsoniterScalaLiteralParitySpec extends MacroSuite {

  private def decodeError(fragment: String)(thunk: => Any): Unit = {
    val error = intercept[JsonReaderException](thunk)
    assert(error.getMessage.contains(fragment), s"expected an error containing '$fragment', got: ${error.getMessage}")
  }

  private def roundTrip[A](codec: JsonValueCodec[A])(value: A, json: String): Unit = {
    writeToString(value)(codec) ==> json
    readFromString[A](json)(codec) ==> value
  }

  group("literal types (jsoniter-scala parity)") {

    test("top-level literal types and errors") {
      roundTrip(KindlingsJsonValueCodec.derived['1'])('1', "\"1\"")
      roundTrip(KindlingsJsonValueCodec.derived[4.0f])(4.0f, "4.0")
      decodeError("expected value: \"VVV\"")(readFromString["VVV"]("\"XXX\"")(KindlingsJsonValueCodec.derived["VVV"]))
      decodeError("expected value: true")(readFromString[true]("false")(KindlingsJsonValueCodec.derived[true]))
      decodeError("expected value: \"1\"")(readFromString['1']("\"0\"")(KindlingsJsonValueCodec.derived['1']))
      decodeError("expected value: 2")(readFromString[2]("0")(KindlingsJsonValueCodec.derived[2]))
      decodeError("expected value: 4.0")(readFromString[4.0f]("0.0")(KindlingsJsonValueCodec.derived[4.0f]))
    }

    test("stringified literal types") {
      val runtimeCodec = {
        implicit val c: JsoniterConfig = StringifiedRuntimeConfigs.stringified
        KindlingsJsonValueCodec.derived[2]
      }
      List[JsonValueCodec[2]](OptimizedCodecs.stringifiedLiteral, runtimeCodec).foreach { c =>
        roundTrip(c)(2, "\"2\"")
        decodeError("expected value: \"2\"")(readFromString[2]("\"0\"")(c))
      }
    }

    test("literal types as map keys") {
      roundTrip(KindlingsJsonValueCodec.derived[Map["VVV", Int]])(Map[("VVV"), Int](("VVV", 0)), "{\"VVV\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map[true, Int]])(Map[true, Int]((true, 0)), "{\"true\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map['1', Int]])(Map['1', Int](('1', 0)), "{\"1\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map[2, Int]])(Map[2, Int]((2, 0)), "{\"2\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map[3L, Int]])(Map[3L, Int]((3L, 0)), "{\"3\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map[4.0f, Int]])(Map[4.0f, Int]((4.0f, 0)), "{\"4.0\":0}")
      roundTrip(KindlingsJsonValueCodec.derived[Map[5.0, Int]])(Map[5.0, Int]((5.0, 0)), "{\"5.0\":0}")
      decodeError("expected key: \"2\"")(readFromString[Map[2, Int]]("{\"0\":0}")(KindlingsJsonValueCodec.derived[Map[2, Int]]))
    }
  }
}
